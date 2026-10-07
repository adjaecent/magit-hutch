(ns eval.trace
  "Query Perfetto traces via `trace_processor`.

The Perfetto trace hutch emits is the single artifact -- findings, rounds,
and token counts all come from here.  We shell out to `trace_processor'
in CLI mode (`trace_processor <trace> -q <sql-file>`) once per query and
parse the CSV response.

CLI mode replaced the HTTP mode used earlier: newer trace_processor
versions (v40+) switched the /query endpoint to a protobuf request/response
protocol.  The CLI's -q flag stays documented as long-term stable, and the
CSV output is trivial to parse.

Assumes `trace_processor' is on PATH.  Install via:
  brew install perfetto     (macOS)
  https://perfetto.dev/docs/quickstart/trace-analysis   (other)"
  (:require [babashka.fs :as fs]
            [babashka.process :refer [sh]]
            [cheshire.core :as json]
            [clojure.data.csv :as csv]
            [clojure.string :as str]
            [eval.config :as cfg :refer [log]]))

(def ^:private tp-bin "trace_processor")

;;; --- Lifecycle stubs (CLI mode is stateless; kept for eval.clj compat) ---

(defn start!
  "No-op in CLI mode.  Kept so eval.clj doesn't need to change."
  [] nil)

(defn stop!
  "No-op in CLI mode."
  [_] nil)

;;; --- CLI query ---

(defn- run-sql
  "Run SQL against TRACE-FILE via `trace_processor -q'.
Returns a vector of maps (keyed by column name), or nil on failure."
  [trace-file sql]
  (let [sql-file (fs/create-temp-file {:prefix "hutch-eval-sql-" :suffix ".sql"})]
    (try
      (spit (fs/file sql-file) sql)
      (let [{:keys [exit out err]} (sh tp-bin (str trace-file) "-q" (str sql-file))]
        (if-not (zero? exit)
          (do (log "  trace_processor -q failed (exit" exit "):"
                   (some-> err (subs 0 (min 400 (count err)))))
              nil)
          (let [rows (csv/read-csv (str/trim out))]
            (when (seq rows)
              (let [[header & data] rows
                    columns         (mapv keyword header)]
                (mapv #(zipmap columns %) data))))))
      (finally
        (fs/delete-if-exists sql-file)))))

;;; --- Queries ---

;; Findings come from the trace JSON directly, not SQL.  trace_processor
;; flattens array-of-object args into separate rows (args.findings.id,
;; args.findings.file, ...), so EXTRACT_ARG on 'findings' returns NULL --
;; the JSON blob is only reconstructable via an ugly pivot.  The trace
;; file has the complete list already.

(def ^:private q-rounds
  ;; Each `llm-call' slice is exactly one turn.  Slice lifecycle in
  ;; hutch-agent.el: `hutch--agent' opens turn 1; every `is-tool-resp'
  ;; closes the current slice and opens the next unless submit_review
  ;; fired (in which case the final slice is closed and no new one is
  ;; opened).  So COUNT(*) equals the number of tool-result callbacks.
  "SELECT COUNT(*) AS n
   FROM slice
   WHERE name = 'llm-call';")

(def ^:private q-tokens
  ;; Counter tracks are per (name, thread): each scope's :shash tid gets
  ;; its own tokens.input / tokens.output track.  MAX per track is the
  ;; final cumulative value for that scope; SUM across tracks with the
  ;; same name gives the cross-scope total.  Single-scope runs have one
  ;; track per name, so SUM == MAX.
  "SELECT name, SUM(m) AS max_value FROM (
     SELECT ct.name AS name, MAX(c.value) AS m
     FROM counter c JOIN counter_track ct ON c.track_id = ct.id
     WHERE ct.name LIKE 'tokens%' OR ct.name IN ('input', 'output')
     GROUP BY ct.id
   )
   GROUP BY name;")

;;; --- Parsing ---

(defn- read-findings
  "Read the sanitized-findings list directly from the trace JSON.
Returns [] if the event isn't present or the arg is missing."
  [trace-file]
  (try
    (let [t      (json/parse-string (slurp trace-file) true)
          events (or (:traceEvents t) [])
          sf     (some #(when (= "sanitized-findings" (:name %)) %) events)
          findings (get-in sf [:args :findings])]
      (cond
        (nil? findings)        []
        (sequential? findings) (vec findings)
        :else                  []))
    (catch Exception e
      (log "  findings read failed:" (.getMessage e))
      [])))

(defn- as-long [s]
  (when s
    (try (long (Double/parseDouble (str s)))
         (catch Exception _ 0))))

(defn- parse-tokens [rows]
  (let [by-name (into {} (map (juxt (comp str :name) (comp as-long :max_value)) rows))
        pick    (fn [& keys] (or (some by-name keys) 0))]
    ;; trace_processor v40+ names counter tracks "tokens input" / "tokens output"
    ;; (space).  Older versions used "tokens.input" (dot).  Accept both.
    {:input-tokens  (pick "tokens input" "tokens.input" "input")
     :output-tokens (pick "tokens output" "tokens.output" "output")}))

;;; --- Extract ---

(defn extract-from-file
  "Load TRACE-FILE and return {:findings [...] :rounds N :input-tokens N :output-tokens N}."
  [trace-file]
  (let [findings (read-findings trace-file)
        rounds   (or (some-> (run-sql trace-file q-rounds) first :n as-long) 0)
        tokens   (parse-tokens (or (run-sql trace-file q-tokens) []))]
    (merge {:findings findings :rounds rounds} tokens)))
