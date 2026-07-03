(ns eval.trace
  "Query Perfetto traces via `trace_processor`.

The Perfetto trace hutch emits is the single artifact — findings, rounds,
and token counts all come from here.  We shell out to `trace_processor`
in HTTP mode once per invocation: start a subprocess, POST the trace bytes
to /parse, POST SQL to /query, read JSON rows back, tear down.

Assumes `trace_processor` is on PATH.  Install via:
  brew install perfetto     (macOS)
  https://perfetto.dev/docs/quickstart/trace-analysis   (other)"
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :as bp]
            [cheshire.core :as json]
            [clojure.string :as str]
            [eval.config :as cfg :refer [log]]))

;;; --- trace_processor lifecycle ---

(def ^:private port 9001)
(def ^:private base-url (str "http://127.0.0.1:" port))
(def ^:private tp-bin "trace_processor")

(defn- healthy? []
  (try (= 200 (:status (http/get (str base-url "/status")
                                 {:throw false :timeout 500})))
       (catch Exception _ false)))

(defn- wait-ready! [proc timeout-ms]
  (let [deadline  (+ (System/currentTimeMillis) timeout-ms)
        java-proc (:proc proc)]
    (loop []
      (cond
        (healthy?)                                :ok
        (>= (System/currentTimeMillis) deadline)  (throw (ex-info "trace_processor HTTP not ready" {}))
        (not (.isAlive java-proc))                (throw (ex-info "trace_processor exited early"
                                                                  {:exit-code (.exitValue java-proc)}))
        :else                                     (do (Thread/sleep 200) (recur))))))

(defn start!
  "Start a `trace_processor` HTTP server (idempotent — returns nil if
one is already listening on `port').  Returns the babashka process
handle, or nil.  Caller must invoke `stop!' on the returned handle.
If readiness times out or the process exits early, destroys the
subprocess before rethrowing so no orphaned trace_processor lingers."
  []
  (if (healthy?)
    (do (log "  trace_processor already listening on" port) nil)
    (let [proc (bp/process {:err :inherit
                            :out :inherit
                            :shutdown bp/destroy-tree}
                           tp-bin "-D" "--http-port" (str port))]
      (try
        (wait-ready! proc 15000)
        (log "  trace_processor up on" port)
        proc
        (catch Exception e
          (bp/destroy-tree proc)
          (throw e))))))

(defn stop!
  "Stop a trace_processor process started via `start!'."
  [proc]
  (when proc (bp/destroy-tree proc)))

;;; --- Load + query ---

(defn- post-bytes [path body]
  (http/post (str base-url path)
             {:body body
              :headers {"Content-Type" "application/octet-stream"}
              :timeout 30000
              :throw false}))

(defn- post-sql [sql]
  (http/post (str base-url "/query")
             {:body sql
              :headers {"Content-Type" "text/plain"}
              :timeout 60000
              :throw false}))

(defn load-trace!
  "Load a trace file into the running trace_processor.  Replaces any
previously loaded trace."
  [trace-file]
  (let [bytes (java.nio.file.Files/readAllBytes (fs/path trace-file))
        ;; Some versions of trace_processor expect POST /restore_initial_tables
        ;; before /parse when reloading — we do it defensively.
        _ (post-bytes "/restore_initial_tables" (byte-array 0))
        resp (post-bytes "/parse" bytes)]
    (when-not (= 200 (:status resp))
      (throw (ex-info "trace_processor /parse failed"
                      {:status (:status resp)
                       :body (some-> resp :body (subs 0 (min 400 (count (:body resp)))))})))
    ;; Some versions need /notify_eof after streaming parse chunks.
    (post-bytes "/notify_eof" (byte-array 0))))

(defn query
  "Run SQL against the currently-loaded trace.  Returns rows as vectors
of maps keyed by column name (keyword).  Returns nil on error."
  [sql]
  (let [resp (post-sql sql)]
    (if-not (= 200 (:status resp))
      (do (log "  trace_processor /query failed" (:status resp)
               ":" (some-> resp :body (subs 0 (min 400 (count (:body resp))))))
          nil)
      (let [parsed  (json/parse-string (:body resp) true)
            columns (mapv keyword (:columns parsed))
            rows    (:values parsed)]
        (mapv #(zipmap columns %) rows)))))

;;; --- High-level extraction ---

(def ^:private q-findings
  ;; The `sanitized-findings` instant event carries the final findings
  ;; list (after hutch--run-gates), one entry per finding, in :args.
  "SELECT EXTRACT_ARG(arg_set_id, 'findings') AS findings
   FROM slice
   WHERE name = 'sanitized-findings'
   LIMIT 1;")

(def ^:private q-rounds
  ;; Each `llm-call' slice is exactly one turn.  Slice lifecycle in
  ;; hutch-agent.el: `hutch--agent' opens turn 1; every `is-tool-resp'
  ;; closes the current slice and opens the next unless submit_review
  ;; fired (in which case the final slice is closed and no new one is
  ;; opened).  So COUNT(*) equals the number of tool-result callbacks,
  ;; matching the old `parse-log-stats' `tool-result' count.  No -1.
  "SELECT COUNT(*) AS n
   FROM slice
   WHERE name = 'llm-call';")

(def ^:private q-tokens
  ;; Counter tracks are per (name, thread): each scope's :shash tid gets
  ;; its own `tokens.input' and `tokens.output' tracks.  MAX per track is
  ;; the final cumulative value for that scope; SUM across tracks with the
  ;; same name gives the cross-scope total.  Single-scope runs (current
  ;; eval filters to :branch) have one track per name, so SUM == MAX.
  "SELECT name, SUM(m) AS max_value FROM (
     SELECT ct.name AS name, MAX(c.value) AS m
     FROM counter c JOIN counter_track ct ON c.track_id = ct.id
     WHERE ct.name LIKE 'tokens%' OR ct.name IN ('input', 'output')
     GROUP BY ct.id
   )
   GROUP BY name;")

(defn- parse-findings [rows]
  (let [raw (some-> rows first :findings)]
    (cond
      (nil? raw)     []
      (string? raw)  (try (vec (json/parse-string raw true))
                          (catch Exception e
                            (log "  findings JSON parse failed:" (.getMessage e))
                            []))
      (sequential? raw) (vec raw)
      :else          [])))

(defn- parse-tokens [rows]
  (let [by-name (into {} (map (juxt (comp str :name) (comp long :max_value)) rows))
        pick    (fn [& keys] (or (some by-name keys) 0))]
    {:input-tokens  (pick "tokens.input" "input")
     :output-tokens (pick "tokens.output" "output")}))

(defn extract
  "Given a loaded trace, return {:findings [...] :rounds N :input-tokens N
:output-tokens N}."
  []
  (let [findings (parse-findings (query q-findings))
        rounds   (or (some-> (query q-rounds) first :n) 0)
        tokens   (parse-tokens (or (query q-tokens) []))]
    (merge {:findings findings :rounds rounds} tokens)))

;; trace_processor holds ONE loaded trace at a time — serialize load+query
;; pairs across parallel workers.
(def ^:private query-lock (Object.))

(defn extract-from-file
  "Load TRACE-FILE and return the extraction map.  Serialized: safe to
call from parallel workers, but only one load+query pair runs at a
time (trace_processor is single-trace)."
  [trace-file]
  (locking query-lock
    (load-trace! trace-file)
    (extract)))
