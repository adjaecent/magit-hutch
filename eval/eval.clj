#!/usr/bin/env bb
;;; eval.clj --- Hutch eval harness: Martian dataset, CR-Bench metrics.
;;;
;;; Subcommands:
;;;   bb eval.clj fetch
;;;     Download Martian's golden_comments/*.json into ./golden_comments/
;;;
;;;   bb eval.clj prep [--repo NAME] [--limit N]
;;;     Phase 1 only: clone source repos + materialize PR worktrees.
;;;     Idempotent — existing worktrees are reused.
;;;
;;;   bb eval.clj run [--repo NAME] [--limit N] [--judge MODEL]
;;;                   [--parallel N] [--out FILE]
;;;     Clone PRs (if not prepped), run hutch, judge findings, append JSONL.
;;;     Resumes from existing --out file by skipping already-evaluated URLs.
;;;
;;;   bb eval.clj rejudge [--repo NAME] [--limit N] [--judge MODEL]
;;;                        [--parallel N] [--out FILE]
;;;     Skip hutch (and its cost); re-extract findings from existing
;;;     Perfetto traces under ./traces/ and re-run the judge.  Useful for
;;;     swapping judge models or reformulating metrics without paying
;;;     for another hutch run.
;;;
;;;   bb eval.clj summary [PATH]
;;;     Print the summary table for an existing results JSONL file.
;;;     PATH defaults to ./results.jsonl.
;;;
;;; Judge models: 'provider_modelid' (e.g. anthropic_claude-opus-4-5-20251101,
;;; openai_gpt-5).  Required env: ANTHROPIC_API_KEY and/or OPENAI_API_KEY.
;;; Optional env: GITHUB_TOKEN, EMACS, EMACS_INIT, HUTCH_MODEL.

(ns eval
  (:require [babashka.fs :as fs]
            [babashka.process :refer [sh]]
            [clojure.tools.cli :refer [parse-opts]]
            [eval.agents :as agents]
            [eval.config :as cfg :refer [log]]
            [eval.metrics :as metrics]
            [eval.repo :as repo]
            [eval.trace :as trace]))

;;; --- CLI ---

(def cli-options
  [["-l" "--limit N" "Max PRs to evaluate"
    :parse-fn #(Integer/parseInt %) :default nil]
   ["-r" "--repo NAME" "Restrict to one repo (e.g. keycloak, sentry, grafana, cal_dot_com)"
    :default nil]
   ["-j" "--judge MODEL" "Judge model: provider_modelid"
    :default cfg/default-judge]
   ["-p" "--parallel N" "Max concurrent workers in Phase 2 (default: pmap's CPU+2)"
    :parse-fn #(Integer/parseInt %) :default nil]
   ["-o" "--out FILE" "Output JSONL file"
    :default "results.jsonl"]
   ["-h" "--help"]])

(defn- parse-cli [args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)]
    (when errors
      (binding [*out* *err*] (run! println errors))
      (System/exit 1))
    (when (:help options)
      (println summary)
      (System/exit 0))
    options))

;;; --- Pre-build ---

(defn- die [& msgs]
  (binding [*out* *err*] (run! println msgs))
  (System/exit 1))

(defn- prebuild-emacs! []
  (log "Pre-building emacs deps...")
  (let [result (sh cfg/emacs-bin "--batch" "--load" cfg/emacs-init
                   "--eval" "(progn (require 'hutch) (require 'hutch-eval))")]
    (if (zero? (:exit result))
      (log "Pre-build OK.")
      (die (format "ERROR: pre-build failed (exit %d):" (:exit result))
           (or (:err result) "(empty)")))))

(defn- check-trace-processor! []
  (let [{:keys [exit]} (sh "sh" "-c" "command -v trace_processor")]
    (when-not (zero? exit)
      (die "ERROR: `trace_processor' not on PATH."
           "  Install:"
           "    curl -LO https://get.perfetto.dev/trace_processor"
           "    chmod +x trace_processor"
           "    sudo mv trace_processor /usr/local/bin/    # or somewhere on PATH"
           "  Docs: https://perfetto.dev/docs/analysis/trace-processor"))))

;;; --- Phase 1: prep one PR ---

(defn- prep-pr
  "Parse URL + materialize the worktree.  Returns a prepped map or nil."
  [entry]
  (let [src-url (repo/pr-source-url entry)
        info    (repo/parse-pr-url src-url)]
    (if-not info
      (do (log "  skip (URL not parseable):" src-url) nil)
      (try
        (log "Preparing:" src-url)
        {:entry entry :src-url src-url :repo-dir (repo/ensure-repo info)}
        (catch Exception e
          (log "  clone/fetch failed for" src-url ":" (.getMessage e))
          nil)))))

;;; --- Phase 2: judge one PR, common to run + rejudge ---

(defn- log-pr-progress [src-url metrics findings run wall-ms]
  (log (format "  [%s] findings: %d  rounds: %d  wall: %.1fs"
               src-url (count findings) (:rounds run) (/ wall-ms 1000.0)))
  (log (format "  [%s] P=%.2f R=%.2f F1=%.2f Use=%.2f SNR=%.2f"
               src-url
               (:precision metrics) (:recall metrics) (:f1 metrics)
               (:usefulness metrics) (:snr metrics))))

(defn- judge-and-record
  "Given a completed hutch RUN ({:findings :rounds :input-tokens :output-tokens}),
call the judge, compute metrics, return the JSONL record."
  [{:keys [judge-model]} entry src-url run wall-ms]
  (let [findings  (:findings run)
        goldens   (vec (:comments entry))
        judgments (mapv #(agents/judge-finding judge-model % goldens) findings)
        m         (metrics/compute-metrics judgments (count goldens))]
    (log-pr-progress src-url m findings run wall-ms)
    {:pr_url        src-url
     :findings      findings
     :judgments     judgments
     :goldens_count (count goldens)
     :rounds        (:rounds run)
     :input_tokens  (:input-tokens run)
     :output_tokens (:output-tokens run)
     :wall_ms       wall-ms}))

(defn- process-pr
  "Produce a run map via PRODUCE-RUN then judge + record.  PRODUCE-RUN
receives the input map ({:entry :src-url :repo-dir}); returns a run
map or nil to skip the PR."
  [opts produce-run {:keys [entry src-url] :as input}]
  (log "Processing:" src-url)
  (let [t0  (System/currentTimeMillis)
        run (produce-run input)]
    (when run
      (judge-and-record opts entry src-url run
                        (- (System/currentTimeMillis) t0)))))

(defn- safe-process-pr [opts produce-run input]
  (try (process-pr opts produce-run input)
       (catch Exception e
         (log "  ERROR for" (:src-url input) ":" (.getMessage e))
         nil)))

(defn- process-phase [opts produce-run inputs parallel]
  (let [worker #(safe-process-pr opts produce-run %)
        mapper (if parallel #(cfg/bounded-pmap parallel worker %) #(pmap worker %))]
    (doall (remove nil? (mapper inputs)))))

;;; --- Produce-run strategies ---

(defn- produce-run-hutch
  "Full run: invoke hutch via emacs, then extract from the fresh trace."
  [{:keys [repo-dir src-url]}]
  (agents/run-hutch repo-dir src-url))

(defn- produce-run-trace
  "Rejudge: extract from an existing trace only.  Returns nil if the trace
is missing (PR is skipped)."
  [{:keys [src-url]}]
  (let [trace-dir (agents/trace-dir-for src-url)]
    (if-let [trace-file (agents/latest-trace-file trace-dir)]
      (do (log "  trace →" trace-file)
          (trace/extract-from-file trace-file))
      (do (log "  skip (no trace for" src-url ")") nil))))

(defn- rejudge-input
  "Wrap a golden entry into the shape process-pr expects (no :repo-dir)."
  [entry]
  {:entry entry :src-url (repo/pr-source-url entry) :repo-dir nil})

;;; --- cmd-* ---

(defn cmd-fetch [_] (repo/fetch-goldens))

(defn cmd-prep [args]
  (let [{:keys [limit repo]} (parse-cli args)
        entries              (cond->> (repo/load-goldens repo) limit (take limit))]
    (log (format "Repo:     %s" (or repo "(all)")))
    (log (format "Prepping %d PRs..." (count entries)))
    (let [prepped (doall (keep prep-pr entries))]
      (log (format "Prepared %d/%d worktrees." (count prepped) (count entries)))
      (log "Done."))))

(defn- log-run-banner [opts out-path done-count]
  (log (format "Emacs:    %s" cfg/emacs-bin))
  (log (format "Judge:    %s" (:judge opts)))
  (log (format "Parallel: %s" (or (:parallel opts) "default")))
  (log (format "Repo:     %s" (or (:repo opts) "(all)")))
  (log (format "Resuming: %d done → %s" done-count out-path))
  (when (pos? done-count)
    (log "\n  ⚠  appending to existing results.")
    (log "  ⚠  if you're using a different backend/judge,")
    (log "  ⚠  pass --out results-NAME.jsonl to keep runs separate.\n")))

(defn- pending-entries [{:keys [limit repo]} done]
  (cond->> (repo/load-goldens repo)
    (seq done) (remove #(done (repo/pr-source-url %)))
    limit      (take limit)))

(defn cmd-run [args]
  (let [{:keys [judge parallel out] :as opts} (parse-cli args)
        out-path (str (System/getProperty "user.dir") "/" out)
        done     (cfg/load-done-urls out-path)
        entries  (pending-entries opts done)]
    (check-trace-processor!)
    (log-run-banner opts out-path (count done))
    (log (format "Evaluating %d PRs..." (count entries)))
    (log "Phase 1: preparing worktrees...")
    (let [prepped (doall (keep prep-pr entries))]
      (log (format "Prepared %d/%d worktrees." (count prepped) (count entries)))
      (prebuild-emacs!)
      (log "Phase 2: running hutch + judge in parallel...")
      (log "Starting trace_processor HTTP server...")
      (let [tp (trace/start!)]
        (try
          (let [recs (process-phase {:judge-model judge}
                                    produce-run-hutch prepped parallel)]
            (cfg/write-jsonl! out-path recs)
            (metrics/print-summary recs)
            (log "Done."))
          (finally (trace/stop! tp)))))))

(defn cmd-summary [args]
  (let [path (or (first args) "results.jsonl")]
    (when-not (fs/exists? path)
      (die (format "No such file: %s" path)))
    (let [records (cfg/read-jsonl path)]
      (log (format "Loaded %d records from %s" (count records) path))
      (metrics/print-summary records))))

(defn cmd-rejudge
  "Re-run judgment against the existing trace files in `traces-dir'.  No
emacs invocation.  Useful for swapping the judge model or reformulating
metrics without paying for another hutch run."
  [args]
  (let [{:keys [judge parallel out repo limit]} (parse-cli args)
        out-path (str (System/getProperty "user.dir") "/" out)
        entries  (cond->> (repo/load-goldens repo) limit (take limit))
        inputs   (mapv rejudge-input entries)]
    (check-trace-processor!)
    (log (format "Judge:    %s" judge))
    (log (format "Parallel: %s" (or parallel "default")))
    (log (format "Repo:     %s" (or repo "(all)")))
    (log (format "Traces:   %s" cfg/traces-dir))
    (log (format "Out:      %s" out-path))
    (log (format "Rejudging %d PRs from existing traces..." (count entries)))
    (log "Starting trace_processor HTTP server...")
    (let [tp (trace/start!)]
      (try
        (let [recs (process-phase {:judge-model judge}
                                  produce-run-trace inputs parallel)]
          ;; Rejudge is meant to be a fresh re-run of the judge over existing
          ;; traces (swap judge model / metrics).  If we append to an existing
          ;; file, PR records get duplicated and metrics/print-summary lies.
          (fs/delete-if-exists out-path)
          (cfg/write-jsonl! out-path recs)
          (metrics/print-summary recs)
          (log (format "Done.  %d/%d PRs had traces." (count recs) (count entries))))
        (finally (trace/stop! tp))))))

;;; --- Main ---

(defn -main [& [cmd & args]]
  (case cmd
    "fetch"   (cmd-fetch   args)
    "prep"    (cmd-prep    args)
    "run"     (cmd-run     args)
    "rejudge" (cmd-rejudge args)
    "summary" (cmd-summary args)
    nil       (println "Usage: bb eval.clj <fetch|prep|run|rejudge|summary> [options]")
    (println "Unknown command:" cmd "(expected: fetch | prep | run | rejudge | summary)")))

(apply -main *command-line-args*)
