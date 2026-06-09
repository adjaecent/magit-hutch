(ns eval.agents
  "Invoke hutch (via emacs --batch) and the LLM judge.  Both produce or
consume JSON; this namespace wraps the HTTP/process plumbing."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :refer [sh]]
            [cheshire.core :as json]
            [clojure.string :as str]
            [eval.config :as cfg :refer [log env-or-fail]]))

;;; --- Hutch invocation ---

(def hutch-max-rounds 80)

(defn log-file-for [pr-url]
  (str cfg/logs-dir "/" (str/replace pr-url #"[^A-Za-z0-9._-]+" "_") ".log"))

(defn- tmp-output-path []
  (str "/tmp/hutch-eval-out-" (System/currentTimeMillis) ".json"))

(defn- hutch-elisp-expr
  "The single elisp expression sent to emacs --batch.  Just calls the
canonical entry point in magit-hutch-eval.el."
  [repo-dir pr-url out-file]
  (format "(progn (require 'magit-hutch-eval)
                  (hutch-eval-batch-review %s %s %s %s %d))"
          (pr-str repo-dir) (pr-str pr-url)
          (pr-str cfg/hutch-model) (pr-str out-file)
          hutch-max-rounds))

(defn- read-findings
  "Slurp + parse + cleanup the JSON file hutch wrote."
  [out-file]
  (try
    (-> out-file slurp (json/parse-string true) :review_comments vec)
    (finally (try (fs/delete out-file) (catch Exception _)))))

(defn run-hutch
  "Run hutch on REPO-DIR via emacs --batch.  Persists stderr to a per-PR
log file.  Returns seq of review_comments (possibly empty)."
  [repo-dir pr-url]
  (fs/create-dirs cfg/logs-dir)
  (let [out-file (tmp-output-path)
        result   (sh cfg/emacs-bin "--batch" "--load" cfg/emacs-init
                     "--eval" (hutch-elisp-expr repo-dir pr-url out-file))
        log-file (log-file-for pr-url)]
    (spit log-file (or (:err result) ""))
    (log "  log →" log-file)
    (cond
      (not (zero? (:exit result))) (do (log "  hutch failed (exit" (:exit result) ")") [])
      (not (fs/exists? out-file))  (do (log "  hutch produced no output file") [])
      :else                         (read-findings out-file))))

(defn- sum-pattern [content pattern]
  (->> (re-seq pattern content)
       (map (comp #(Integer/parseInt %) second))
       (apply +)))

(defn parse-log-stats
  "Extract rounds and token counts from a per-PR hutch log file.
Input tokens are usually 0 — gptel doesn't expose them in tool-use callbacks."
  [pr-url]
  (let [log-path (log-file-for pr-url)]
    (if-not (fs/exists? log-path)
      {:rounds 0 :input-tokens 0 :output-tokens 0}
      (let [content (slurp log-path)]
        {:rounds        (count (re-seq #"tool-result" content))
         :input-tokens  (sum-pattern content #"\+R(\d+)")
         :output-tokens (sum-pattern content #"\+W(\d+)")}))))

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

(defn- judge-prompt [finding goldens]
  (format "FINDING:\nPath: %s\nLine: %s\nBody: %s\n\nGOLDEN COMMENTS:\n%s"
          (or (:path finding) "?")
          (or (:line finding) "?")
          (or (:body finding) "")
          (str/join "\n" (map-indexed format-golden goldens))))

(def ^:private failed-judgment
  {:classification "JUDGE_FAILED" :matched_index nil :reasoning "judge call/parse failed"})

(defn judge-finding [model finding goldens]
  (if-let [text (judge-call model judge-system (judge-prompt finding goldens))]
    (try (json/parse-string text true)
         (catch Exception _ failed-judgment))
    failed-judgment))
