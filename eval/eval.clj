#!/usr/bin/env bb
;;; eval.clj --- Hutch eval harness: Martian dataset, CR-Bench metrics.
;;;
;;; Subcommands:
;;;   bb eval.clj fetch
;;;     Download Martian's golden_comments/*.json into ./golden_comments/
;;;
;;;   bb eval.clj prep [--repo NAME] [--limit N]
;;;     Phase 1 only: clone source repos + materialize PR worktrees.
;;;     Idempotent — existing worktrees are reused.  Useful for warming
;;;     up workspaces while you do something else.
;;;
;;;   bb eval.clj run [--repo NAME] [--limit N] [--judge MODEL] [--out FILE]
;;;     Clone PRs (if not prepped), run hutch, judge findings, append JSONL.
;;;     Resumes from existing --out file by skipping already-evaluated PR URLs.
;;;
;;;   bb eval.clj summary [PATH]
;;;     Print the summary table for an existing results JSONL file.
;;;     PATH defaults to ./results.jsonl.
;;;
;;; Judge models are addressed as 'provider_modelid', e.g.:
;;;   anthropic_claude-opus-4-5-20251101
;;;   openai_gpt-5
;;;
;;; Required env (depending on judge):
;;;   ANTHROPIC_API_KEY, OPENAI_API_KEY
;;; Optional env:
;;;   GITHUB_TOKEN  - higher rate limits for goldens fetch
;;;   EMACS         - emacs binary (default: 'emacs')
;;;   EMACS_INIT    - init file to load (default: ~/.emacs.d/init.el)
;;;   HUTCH_MODEL   - tool name written into the export record (default: 'magit-hutch')

(ns eval
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :as proc :refer [sh shell]]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [taoensso.timbre :as timbre]))

;;; --- Config ---

(def default-judge "openai_gpt-5.2")
(def martian-repo "withmartian/code-review-benchmark")
(def martian-goldens-path "offline/golden_comments")
(def goldens-dir "golden_comments")
(def logs-dir    "logs")
(def repo-cache  "/tmp/hutch-eval-repos")
(def clones-dir  (str repo-cache "/_clones"))
(def macos-emacs-bundle "/Applications/Emacs.app/Contents/MacOS/Emacs")
(def emacs-init  (or (System/getenv "EMACS_INIT") (str (System/getenv "HOME") "/.emacs.d/init.el")))
(def hutch-model (or (System/getenv "HUTCH_MODEL") "magit-hutch"))
(defn- detect-emacs []
  (or (System/getenv "EMACS")
      (when (zero? (:exit (sh "which" "emacs"))) "emacs")
      (when (fs/exists? macos-emacs-bundle) macos-emacs-bundle)))
(def emacs-bin   (detect-emacs))

;;; --- Logging ---

;; Timbre is thread-safe by design and bundled in babashka.  We replace
;; the default appender with a tiny custom one that prints vargs to
;; stderr verbatim — same shape as a plain `println`, no level/ns prefix,
;; serialized at the appender level.
(timbre/merge-config!
 {:min-level :info
  :appenders {:println {:enabled? false}
              :stderr  {:enabled?  true
                        :async?    false
                        :min-level :info
                        :fn        (fn [{:keys [vargs]}]
                                     (binding [*out* *err*]
                                       (apply println vargs)))}}})

(defmacro log [& args]
  `(timbre/info ~@args))

;;; --- Goldens fetch ---

(defn- gh-headers []
  (cond-> {"Accept" "application/vnd.github+json"}
    (System/getenv "GITHUB_TOKEN")
    (assoc "Authorization" (str "token " (System/getenv "GITHUB_TOKEN")))))

(defn- gh-list-dir [repo path]
  (-> (http/get (format "https://api.github.com/repos/%s/contents/%s" repo path)
                {:headers (gh-headers)})
      :body
      (json/parse-string true)))

(defn fetch-goldens
  "Download Martian's golden_comments/*.json into ./golden_comments/."
  []
  (fs/create-dirs goldens-dir)
  (doseq [{:keys [name download_url]} (gh-list-dir martian-repo martian-goldens-path)
          :when (str/ends-with? name ".json")]
    (log "  fetching" name)
    (spit (str goldens-dir "/" name) (:body (http/get download_url))))
  (log "Goldens written to" goldens-dir))

(defn load-goldens
  "Read PR entries from goldens-dir.  With REPO, only <REPO>.json."
  [& [repo]]
  (let [files (if repo
                [(str goldens-dir "/" repo ".json")]
                (fs/glob goldens-dir "*.json"))]
    (mapcat (fn [f] (json/parse-string (slurp (str f)) true)) files)))

;;; --- Bounded concurrency ---

(defn- bounded-pmap
  "Like pmap but caps concurrent in-flight work at N.
Smoother than chunked-pmap — work flows through as semaphore permits free up,
no burst-then-wait pattern that would worsen rate-limit pressure."
  [n f coll]
  (let [sem (java.util.concurrent.Semaphore. n)]
    (->> coll
         (mapv (fn [x]
                 (.acquire sem)
                 (future (try (f x) (finally (.release sem))))))
         (mapv deref))))

;;; --- PR helpers ---

(defn pr-source-url
  "Real source PR URL (prefer original_url, else url)."
  [entry]
  (or (:original_url entry) (:url entry)))

(defn parse-pr-url
  "Parse 'https://github.com/OWNER/REPO/pull/N' → {:owner :repo :pr}."
  [url]
  (when-let [m (re-find #"github\.com/([^/]+)/([^/]+)/pull/(\d+)" url)]
    {:owner (nth m 1)
     :repo  (nth m 2)
     :pr    (Integer/parseInt (nth m 3))}))

(defn pr-id [{:keys [owner repo pr]}]
  (str owner "__" repo "__pr" pr))

;;; --- Repo management ---

(defn- default-branch-name
  "Resolve the default branch name from origin/HEAD, e.g. 'main' or 'master'."
  [dir]
  (let [r (sh {:dir dir} "git" "symbolic-ref" "--short" "refs/remotes/origin/HEAD")]
    (when (zero? (:exit r))
      ;; e.g. 'origin/main' → 'main'
      (-> r :out str/trim (str/replace #"^origin/" "")))))

(defn- ensure-shared-clone
  "Clone OWNER/REPO into `clones-dir` if not already present.  Returns the dir."
  [owner repo]
  (fs/create-dirs clones-dir)
  (let [name (str owner "__" repo)
        dir  (str clones-dir "/" name)
        url  (format "https://github.com/%s/%s.git" owner repo)]
    (when-not (fs/exists? dir)
      (log "  cloning shared" url)
      (shell {:dir clones-dir} "git" "clone" "--quiet" url name)
      (when-let [default (default-branch-name dir)]
        (sh {:dir dir} "git" "branch" "--force"
            default (str "origin/" default))))
    dir))

(defn ensure-repo
  "Return a worktree of OWNER/REPO with the PR head checked out.
The source clone is shared across all PRs of the same repo (under
`clones-dir`); each PR gets a lightweight worktree.  The PR head is
fetched into a uniquely-named branch (`eval-pr-N`) so multiple PRs
of the same repo never collide.

Reuses an existing worktree if present — this lets us iterate on
hutch's source and re-run the eval without re-cloning or re-fetching.
To force a fresh fetch, `rm -rf` the worktree dir manually.

Not safe to call concurrently against the same shared clone.  The
caller is expected to invoke this sequentially in a prep phase."
  [{:keys [owner repo pr] :as info}]
  (fs/create-dirs repo-cache)
  (let [clone-dir (ensure-shared-clone owner repo)
        wt-dir    (str repo-cache "/" (pr-id info))
        branch    (format "eval-pr-%d" pr)]
    (if (fs/exists? wt-dir)
      (do (log "  reuse" wt-dir) wt-dir)
      (do
        ;; No leftover branch from a prior run — be paranoid.
        (sh {:dir clone-dir} "git" "branch" "-D" branch)
        ;; Fetch PR head into the shared clone as a fresh branch, then
        ;; materialize a worktree pointing at it.
        (shell {:dir clone-dir} "git" "fetch" "--quiet" "origin"
               (format "pull/%d/head:%s" pr branch))
        (shell {:dir clone-dir} "git" "worktree" "add" "--quiet" wt-dir branch)
        wt-dir))))

;;; --- Run hutch ---

(def hutch-max-rounds
  "Per-PR round budget for the agent.  Big PRs (e.g. Keycloak) blow through
40; 80 is generous without enabling unbounded burn."
  80)

(defn- log-file-for [pr-url]
  (str logs-dir "/" (str/replace pr-url #"[^A-Za-z0-9._-]+" "_") ".log"))

(defn- parse-log-stats
  "Extract rounds and token counts from a per-PR hutch log file.
Returns {:rounds N :input-tokens X :output-tokens Y}.  Input tokens
are usually 0 — gptel doesn't expose them in tool-use callbacks."
  [log-path]
  (if-not (fs/exists? log-path)
    {:rounds 0 :input-tokens 0 :output-tokens 0}
    (let [content (slurp log-path)
          rounds  (count (re-seq #"tool-result" content))
          in      (->> (re-seq #"\+R(\d+)" content)
                       (map (comp #(Integer/parseInt %) second))
                       (apply +))
          out     (->> (re-seq #"\+W(\d+)" content)
                       (map (comp #(Integer/parseInt %) second))
                       (apply +))]
      {:rounds rounds :input-tokens in :output-tokens out})))

(defn run-hutch
  "Invoke hutch via emacs --batch in REPO-DIR.  Return seq of review_comments."
  [repo-dir pr-url]
  (let [out-file (str "/tmp/hutch-eval-out-" (System/currentTimeMillis) ".json")
        expr    (format
                 "(progn (require 'magit-hutch-eval)
                         (hutch-eval-batch-review %s %s %s %s %d))"
                 (pr-str repo-dir) (pr-str pr-url)
                 (pr-str hutch-model) (pr-str out-file)
                 hutch-max-rounds)
        result  (sh emacs-bin "--batch" "--load" emacs-init "--eval" expr)]
    (try
      ;; Always persist the emacs stderr (which includes [eval] lines and
      ;; [eval-log] *hutch-log* dump) to a per-PR file for inspection.
      (fs/create-dirs logs-dir)
      (let [log-file (log-file-for pr-url)]
        (spit log-file (or (:err result) ""))
        (log (format "  log → %s" log-file)))
      (cond
        (not (zero? (:exit result)))
        (do (log "  hutch failed (exit" (:exit result) ") — see log file")
            [])
        (not (fs/exists? out-file))
        (do (log "  hutch produced no output file — see log file")
            [])
        :else
        (-> out-file slurp (json/parse-string true) :review_comments vec))
      (finally
        (try (fs/delete out-file) (catch Exception _))))))

;;; --- Judge ---

(defn- env-or-fail [k]
  (or (System/getenv k) (throw (ex-info (str k " not set") {}))))

(defn- model-id [model]
  (second (str/split model #"_" 2)))

(defn- post-json [url headers body]
  (let [resp (http/post url {:headers headers
                             :body    (json/generate-string body)
                             :throw   false})]
    (if (= 200 (:status resp))
      (json/parse-string (:body resp) true)
      (let [body-preview (some-> resp :body
                                 (subs 0 (min 400 (count (:body resp)))))]
        (log "  judge error" (:status resp) ":" body-preview)))))

(defmulti judge-call
  "Dispatch by provider prefix in MODEL ('anthropic_...', 'openai_...')."
  (fn [model _system _user]
    (-> model (str/split #"_" 2) first keyword)))

(defmethod judge-call :anthropic [model system user]
  (-> (post-json "https://api.anthropic.com/v1/messages"
                 {"x-api-key"         (env-or-fail "ANTHROPIC_API_KEY")
                  "anthropic-version" "2023-06-01"
                  "content-type"      "application/json"}
                 {:model       (model-id model)
                  :max_tokens  512
                  :temperature 0
                  :system      system
                  :messages    [{:role "user" :content user}]})
      :content first :text))

(defmethod judge-call :openai [model system user]
  (-> (post-json "https://api.openai.com/v1/chat/completions"
                 {"Authorization" (str "Bearer " (env-or-fail "OPENAI_API_KEY"))
                  "Content-Type"  "application/json"}
                 {:model       (model-id model)
                  :temperature 0
                  :messages    [{:role "system" :content system}
                                {:role "user"   :content user}]})
      :choices first :message :content))

(def judge-system
  (str "You are evaluating a single finding produced by an automated code review tool.\n"
       "Given the finding and a list of golden (human-verified) review comments for the same PR, "
       "classify the finding as exactly one of:\n"
       "  BUG_HIT          — the finding identifies the same issue as one of the golden comments.\n"
       "  VALID_SUGGESTION — the finding is a real, correct issue not present in the golden set.\n"
       "  NOISE            — the finding is wrong, irrelevant, stylistic, or hallucinated.\n"
       "Respond with JSON only:\n"
       "  {\"classification\": \"BUG_HIT\"|\"VALID_SUGGESTION\"|\"NOISE\",\n"
       "   \"matched_index\": int|null,\n"
       "   \"reasoning\":     \"brief explanation\"}"))

(defn- format-golden [i g]
  (format "[%d] (severity=%s) %s" i (or (:severity g) "?") (:comment g)))

(defn judge-finding [model finding goldens]
  (let [user (format "FINDING:\nPath: %s\nLine: %s\nBody: %s\n\nGOLDEN COMMENTS:\n%s"
                     (or (:path finding) "?")
                     (or (:line finding) "?")
                     (or (:body finding) "")
                     (str/join "\n" (map-indexed format-golden goldens)))]
    (if-let [text (judge-call model judge-system user)]
      (try (json/parse-string text true)
           (catch Exception _
             {:classification "JUDGE_FAILED" :matched_index nil
              :reasoning "judge response parse error"}))
      {:classification "JUDGE_FAILED" :matched_index nil
       :reasoning "judge call failed"})))

;;; --- Metrics ---

(defn compute-metrics
  "CR-Bench-style metrics + standard precision/recall/F1."
  [judgments num-goldens]
  (let [cls         (map :classification judgments)
        bug-hits    (count (filter #(= "BUG_HIT"          %) cls))
        valid       (count (filter #(= "VALID_SUGGESTION" %) cls))
        noise       (count (filter #(= "NOISE"            %) cls))
        failed      (count (filter #(= "JUDGE_FAILED"     %) cls))
        ;; Total excludes failed — those are unjudged, not negative results.
        total       (+ bug-hits valid noise)
        useful      (+ bug-hits valid)
        goldens-hit (->> judgments
                         (filter #(= "BUG_HIT" (:classification %)))
                         (keep   :matched_index)
                         distinct
                         count)
        missed      (- num-goldens goldens-hit)
        precision   (if (pos? total)       (/ (double bug-hits)    total)       0.0)
        recall      (if (pos? num-goldens) (/ (double goldens-hit) num-goldens) 0.0)
        f1          (if (pos? (+ precision recall))
                      (/ (* 2 precision recall) (+ precision recall))
                      0.0)
        usefulness  (if (pos? total) (/ (double useful) total) 0.0)
        snr         (if (pos? noise) (/ (double useful) noise) (double useful))]
    {:bug-hits    bug-hits
     :valid       valid
     :noise       noise
     :failed      failed
     :total       total
     :goldens     num-goldens
     :goldens-hit goldens-hit
     :missed      missed
     :precision   precision
     :recall      recall
     :f1          f1
     :usefulness  usefulness
     :snr         snr}))

;;; --- Main pipeline ---

(defn prep-pr
  "Phase 1: parse URL and materialize the worktree.  Sequential.
Returns {:entry :src-url :repo-dir} or nil if unparseable or fetch fails."
  [entry]
  (let [src-url (pr-source-url entry)
        info    (parse-pr-url src-url)]
    (if-not info
      (do (log "  skip (URL not parseable):" src-url) nil)
      (do
        (log "Preparing:" src-url)
        (try
          {:entry    entry
           :src-url  src-url
           :repo-dir (ensure-repo info)}
          (catch Exception e
            (log "  clone/fetch failed for" src-url ":" (.getMessage e))
            nil))))))

(defn run-pr
  "Phase 2: run hutch + judge against a prepped worktree.  Safe to parallelize."
  [{:keys [judge-model]} {:keys [entry src-url repo-dir]}]
  (log "Processing:" src-url)
  (let [t0        (System/currentTimeMillis)
        findings  (run-hutch repo-dir src-url)
        t1        (System/currentTimeMillis)
        stats     (parse-log-stats (log-file-for src-url))
        _         (log (format "  [%s] findings: %d  rounds: %d  wall: %.1fs"
                               src-url (count findings)
                               (:rounds stats) (/ (- t1 t0) 1000.0)))
        goldens   (vec (:comments entry))
        judgments (mapv #(judge-finding judge-model % goldens) findings)
        metrics   (compute-metrics judgments (count goldens))]
    (log (format "  [%s] P=%.2f R=%.2f F1=%.2f Use=%.2f SNR=%.2f"
                 src-url
                 (:precision metrics) (:recall metrics) (:f1 metrics)
                 (:usefulness metrics) (:snr metrics)))
    {:pr_url        src-url
     :findings      findings
     :judgments     judgments
     :metrics       metrics
     :rounds        (:rounds stats)
     :input_tokens  (:input-tokens stats)
     :output_tokens (:output-tokens stats)
     :wall_ms       (- t1 t0)}))

;;; --- JSONL I/O ---

(defn write-jsonl!
  "Append RECORDS to PATH as JSONL.  Single-threaded; intended for use
after a parallel run has fully completed."
  [path records]
  (with-open [w (io/writer path :append true)]
    (doseq [rec records]
      (.write w (str (json/generate-string rec) "\n")))))

(defn load-done-urls [path]
  (if (fs/exists? path)
    (with-open [r (io/reader path)]
      (into #{}
            (keep (fn [line]
                    (when-not (str/blank? line)
                      (:pr_url (json/parse-string line true)))))
            (line-seq r)))
    #{}))

(defn print-summary [records]
  (let [n   (count records)
        sum (fn [k] (apply + (map #(get-in % [:metrics k]) records)))
        avg (fn [k] (if (zero? n) 0.0
                        (/ (apply + (map #(get-in % [:metrics k]) records)) (double n))))]
    (let [;; Top-level per-record fields (not under :metrics).
          sum-rec (fn [k] (apply + (keep #(get % k) records)))
          avg-rec (fn [k] (if (zero? n) 0.0
                              (/ (sum-rec k) (double n))))]
      (println "\n=== EVAL SUMMARY ===")
      (println (format "PRs evaluated:    %d" n))
      (println (format "Total findings:   %d  (hits %d, valid %d, noise %d)"
                       (sum :total) (sum :bug-hits) (sum :valid) (sum :noise)))
      (when (pos? (sum :failed))
        (println (format "Judge failures:   %d  (excluded from totals)" (sum :failed))))
      (println (format "Total goldens:    %d  (hit %d, missed %d)"
                       (sum :goldens) (sum :goldens-hit) (sum :missed)))
      (println (format "Avg Precision:    %.3f" (avg :precision)))
      (println (format "Avg Recall:       %.3f" (avg :recall)))
      (println (format "Avg F1:           %.3f" (avg :f1)))
      (println (format "Avg Usefulness:   %.3f" (avg :usefulness)))
      (println (format "Avg SNR:          %.3f" (avg :snr)))
      (println "")
      (println "--- Efficiency ---")
      (println (format "Avg rounds/PR:    %.1f" (avg-rec :rounds)))
      (println (format "Avg wall/PR:      %.1fs" (/ (avg-rec :wall_ms) 1000.0)))
      (println (format "Total output tok: %d  (avg %d/PR)"
                       (sum-rec :output_tokens) (int (avg-rec :output_tokens))))
      (when (pos? (sum-rec :input_tokens))
        (println (format "Total input tok:  %d  (avg %d/PR)"
                         (sum-rec :input_tokens) (int (avg-rec :input_tokens)))))
      (let [useful (+ (sum :bug-hits) (sum :valid))]
        (when (and (pos? useful) (pos? (sum-rec :output_tokens)))
          (println (format "Output tok/useful finding: %d"
                           (int (/ (sum-rec :output_tokens) useful)))))))))

;;; --- CLI ---

(def cli-options
  [["-l" "--limit N" "Max PRs to evaluate"
    :parse-fn #(Integer/parseInt %)
    :default nil]
   ["-r" "--repo NAME" "Restrict to one repo (e.g. keycloak, sentry, grafana, discourse, cal_dot_com)"
    :default nil]
   ["-j" "--judge MODEL" "Judge model: provider_modelid"
    :default default-judge]
   ["-p" "--parallel N" "Max concurrent workers in Phase 2 (default: pmap's CPU+2). Lower for rate-limited backends like Anthropic."
    :parse-fn #(Integer/parseInt %)
    :default nil]
   ["-o" "--out FILE" "Output JSONL file"
    :default "results.jsonl"]
   ["-h" "--help"]])

(defn cmd-fetch [_]
  (fetch-goldens))

(defn- prebuild-emacs! []
  (log "Pre-building emacs deps (one-time, single-process)...")
  (let [result (sh emacs-bin "--batch"
                   "--load" emacs-init
                   "--eval" "(progn (require 'magit-hutch) (require 'magit-hutch-eval))")]
    (if (zero? (:exit result))
      (log "Pre-build OK.")
      (do (binding [*out* *err*]
            (println "ERROR: pre-build failed (exit" (:exit result) "):")
            (println (or (:err result) "(empty)")))
          (System/exit 1)))))

(defn- validate-env! []
  (when-not emacs-bin
    (binding [*out* *err*]
      (println "ERROR: emacs binary not found.")
      (println "Set the EMACS env var to the binary path, e.g.:")
      (println (format "  export EMACS=%s" macos-emacs-bundle))
      (println "Or install emacs into PATH (`brew install emacs`)."))
    (System/exit 1))
  (when-not (fs/exists? emacs-init)
    (binding [*out* *err*]
      (println (format "ERROR: emacs init file not found: %s" emacs-init))
      (println "Set the EMACS_INIT env var to an init file that loads magit-hutch."))
    (System/exit 1)))

(defn cmd-prep
  "Phase 1 only: clone + worktree.  Lets you pre-prep PRs while you
do something else, then run `bb run` later to evaluate them."
  [args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)
        {:keys [limit repo help]}        options]
    (cond
      help   (println summary)
      errors (do (binding [*out* *err*] (run! println errors)) (System/exit 1))
      :else
      (let [entries (cond->> (load-goldens repo) limit (take limit))]
        (log (format "Repo:     %s" (or repo "(all)")))
        (log (format "Prepping %d PRs..." (count entries)))
        (let [prepped (doall (keep prep-pr entries))]
          (log (format "Prepared %d/%d worktrees." (count prepped) (count entries)))
          (log "Done."))))))

(defn cmd-run [args]
  (let [{:keys [options errors summary]}             (parse-opts args cli-options)
        {:keys [limit repo judge parallel out help]} options]
    (cond
      help   (println summary)
      errors (do (binding [*out* *err*] (run! println errors)) (System/exit 1))
      :else
      (do
        (validate-env!)
        (let [out-path (str (System/getProperty "user.dir") "/" out)
              done     (load-done-urls out-path)
              _        (log (format "Emacs:    %s" emacs-bin))
              _        (log (format "Judge:    %s" judge))
              _        (log (format "Parallel: %s" (or parallel "default")))
              _        (log (format "Repo:     %s" (or repo "(all)")))
              _        (log (format "Resuming: %d done → %s" (count done) out-path))
              ;; Loud warning: if the existing file has records and we're about
              ;; to add to it, the user might be mixing backends/configs.
              _        (when (pos? (count done))
                         (log "")
                         (log "  ⚠  appending to existing results.")
                         (log "  ⚠  if you're using a different backend/judge,")
                         (log "  ⚠  pass --out results-NAME.jsonl to keep runs separate.")
                         (log ""))
              entries  (cond->> (load-goldens repo)
                         (seq done) (remove #(done (pr-source-url %)))
                         limit      (take limit))]
          (log (format "Evaluating %d PRs..." (count entries)))
          (let [opts    {:judge-model judge}
                ;; Phase 1: sequential worktree prep.
                _       (log "Phase 1: preparing worktrees...")
                prepped (doall (keep prep-pr entries))
                _       (log (format "Prepared %d/%d worktrees." (count prepped) (count entries)))
                ;; Pre-build magit-hutch once so parallel workers don't all race
                ;; on straight's build-cache.el lock.
                _       (prebuild-emacs!)
                ;; Phase 2: parallel hutch + judge.
                _       (log "Phase 2: running hutch + judge in parallel...")
                worker  (fn [p]
                          (try (run-pr opts p)
                               (catch Exception e
                                 (log "  ERROR for" (:src-url p) ":" (.getMessage e))
                                 nil)))
                recs    (->> (if parallel
                               (bounded-pmap parallel worker prepped)
                               (pmap worker prepped))
                             (remove nil?)
                             doall)
                _       (write-jsonl! out-path recs)]
            (print-summary recs)
            (log "Done.")))))))

(defn cmd-summary
  "Print the summary table for an existing results.jsonl (no re-running).
Useful for post-hoc analysis after a run completes or after editing the file."
  [args]
  (let [path (or (first args) "results.jsonl")]
    (if-not (fs/exists? path)
      (do (binding [*out* *err*]
            (println (format "No such file: %s" path)))
          (System/exit 1))
      (let [records (->> (str/split-lines (slurp path))
                         (remove str/blank?)
                         (mapv #(json/parse-string % true)))]
        (log (format "Loaded %d records from %s" (count records) path))
        (print-summary records)))))

(defn -main [& [cmd & args]]
  (case cmd
    "fetch"   (cmd-fetch   args)
    "prep"    (cmd-prep    args)
    "run"     (cmd-run     args)
    "summary" (cmd-summary args)
    nil       (println "Usage: bb eval.clj <fetch|prep|run|summary> [options]")
    (println "Unknown command:" cmd "(expected: fetch | prep | run | summary)")))

(apply -main *command-line-args*)
