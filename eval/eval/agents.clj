(ns eval.agents
  "Invoke hutch (via emacs --batch) and the LLM judge.  Hutch writes a
Perfetto trace as its single artifact; findings and stats come from
that trace via `eval.trace`.  The judge stays HTTP JSON."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :refer [sh]]
            [cheshire.core :as json]
            [clojure.string :as str]
            [eval.config :as cfg :refer [log env-or-fail]]
            [eval.trace :as trace]))

;;; --- Hutch invocation ---

(def hutch-max-rounds 80)

(defn log-file-for [pr-url]
  (str cfg/logs-dir "/" (str/replace pr-url #"[^A-Za-z0-9._-]+" "_") ".log"))

(defn trace-dir-for [pr-url]
  (str cfg/traces-dir "/" (str/replace pr-url #"[^A-Za-z0-9._-]+" "_")))

(defn latest-trace-file
  "Return the newest `hutch-trace-*.json' in DIR, or nil."
  [dir]
  (when (fs/exists? dir)
    (->> (fs/list-dir dir)
         (filter #(re-find #"hutch-trace-.*\.json$" (str %)))
         (sort-by fs/last-modified-time)
         last
         (some-> str))))

(defn- hutch-elisp-expr
  "Single elisp expression sent to emacs --batch.  Sets trace-dir then
runs the review; the Perfetto trace at TRACE-DIR is the artifact."
  [repo-dir trace-dir]
  (format "(progn (require 'hutch-eval)
                  (setq hutch-trace-dir %s)
                  (make-directory hutch-trace-dir t)
                  (hutch-eval-batch-review %s %d))"
          (pr-str trace-dir)
          (pr-str repo-dir)
          hutch-max-rounds))

(defn run-hutch
  "Run hutch on REPO-DIR via emacs --batch, then extract findings + stats
from the Perfetto trace it wrote.  Persists stderr to a per-PR log
file.  Returns {:findings [...] :rounds N :input-tokens N
:output-tokens N}.  On failure returns the same shape with empty
findings and zero stats."
  [repo-dir pr-url]
  (fs/create-dirs cfg/logs-dir)
  (let [trace-dir (trace-dir-for pr-url)
        ;; Clear stale traces from prior runs so `latest-trace-file'
        ;; picks up only this invocation's output.
        _         (when (fs/exists? trace-dir) (fs/delete-tree trace-dir))
        _         (fs/create-dirs trace-dir)
        result    (sh cfg/emacs-bin "--batch" "--load" cfg/emacs-init
                      "--eval" (hutch-elisp-expr repo-dir trace-dir))
        log-file  (log-file-for pr-url)
        empty     {:findings [] :rounds 0 :input-tokens 0 :output-tokens 0}]
    (spit log-file (or (:err result) ""))
    (log "  log →" log-file)
    (cond
      (not (zero? (:exit result)))
      (do (log "  hutch failed (exit" (:exit result) ")") empty)

      :else
      (if-let [trace-file (latest-trace-file trace-dir)]
        (try
          (log "  trace →" trace-file)
          (trace/extract-from-file trace-file)
          (catch Exception e
            (log "  trace extract failed:" (.getMessage e))
            empty))
        (do (log "  hutch produced no trace file at" trace-dir) empty)))))

;;; --- Judge: provider dispatch ---

(defn- model-id [model]
  (second (str/split model #"_" 2)))

(defn- post-json
  "POST a JSON body, return parsed body on 200 or nil on error (logs the error)."
  [url headers body]
  (let [resp (http/post url {:headers headers
                             :body    (json/generate-string body)
                             :throw   false})]
    (if (= 200 (:status resp))
      (json/parse-string (:body resp) true)
      (let [preview (some-> resp :body (subs 0 (min 400 (count (:body resp)))))]
        (log "  judge error" (:status resp) ":" preview)))))

(defmulti judge-call
  "Dispatch by provider prefix ('anthropic_...', 'openai_...')."
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

(defmethod judge-call :openrouter [model system user]
  ;; OpenRouter is OpenAI-compatible.  Model ids look like 'provider/model'
  ;; (e.g. 'deepseek/deepseek-chat'), and the slash survives our split.
  (-> (post-json "https://openrouter.ai/api/v1/chat/completions"
                 {"Authorization" (str "Bearer " (env-or-fail "OPENROUTER_API_KEY"))
                  "Content-Type"  "application/json"
                  "HTTP-Referer"  "https://github.com/adjaecent/magit-hutch"
                  "X-Title"       "magit-hutch eval"}
                 {:model       (model-id model)
                  :temperature 0
                  :messages    [{:role "system" :content system}
                                {:role "user"   :content user}]})
      :choices first :message :content))

;;; --- Judge: prompt + classify ---

(def ^:private judge-system
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

(defn- finding-body [finding]
  (let [title (or (:title finding) "")
        desc  (or (:desc finding) "")
        patch (:patch finding)]
    (if patch
      (format "%s\n\n%s\n\nProposed patch:\n```diff\n%s\n```" title desc patch)
      (format "%s\n\n%s" title desc))))

(defn- judge-prompt [finding goldens]
  (format "FINDING:\nPath: %s\nLine: %s\nBody: %s\n\nGOLDEN COMMENTS:\n%s"
          (or (:file finding) "?")
          (or (:lines finding) "?")
          (finding-body finding)
          (str/join "\n" (map-indexed format-golden goldens))))

(def ^:private failed-judgment
  {:classification "JUDGE_FAILED" :matched_index nil :reasoning "judge call/parse failed"})

(defn judge-finding [model finding goldens]
  (if-let [text (judge-call model judge-system (judge-prompt finding goldens))]
    (try (json/parse-string text true)
         (catch Exception _ failed-judgment))
    failed-judgment))
