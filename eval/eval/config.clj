(ns eval.config
  "Constants, logging setup, JSONL I/O, bounded concurrency.
Imported by every other namespace in the harness."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as timbre]))

;;; --- Constants ---

(def default-judge "openai_gpt-5.2")
(def martian-repo "withmartian/code-review-benchmark")
(def martian-goldens-path "offline/golden_comments")
(def goldens-dir "golden_comments")
(def logs-dir    "logs")
(def repo-cache  "/tmp/hutch-eval-repos")
(def clones-dir  (str repo-cache "/_clones"))
(def emacs-bin   "/Applications/Emacs.app/Contents/MacOS/Emacs")
(def emacs-init  (str (System/getenv "HOME") "/.emacs.d/init.el"))
(def hutch-model "magit-hutch")

;;; --- Logging ---

;; Timbre with a tiny stderr appender — bare output, no level/ns/timestamp
;; prefix, thread-safe at the appender level.
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

;;; --- Env helpers ---

(defn env-or-fail [k]
  (or (System/getenv k) (throw (ex-info (str k " not set") {}))))

;;; --- Bounded concurrency ---

(defn bounded-pmap
  "Like pmap but caps in-flight work at N.  Smooth flow rather than chunked
burst-then-wait, important for rate-limited backends."
  [n f coll]
  (let [sem (java.util.concurrent.Semaphore. n)]
    (->> coll
         (mapv (fn [x]
                 (.acquire sem)
                 (future (try (f x) (finally (.release sem))))))
         (mapv deref))))

;;; --- JSONL I/O ---

(defn write-jsonl!
  "Append RECORDS to PATH as JSONL.  Caller must ensure single-writer."
  [path records]
  (with-open [w (io/writer path :append true)]
    (doseq [rec records]
      (.write w (str (json/generate-string rec) "\n")))))

(defn load-done-urls
  "Read PR URLs already present in PATH.  Returns a set."
  [path]
  (if-not (fs/exists? path)
    #{}
    (with-open [r (io/reader path)]
      (into #{}
            (keep #(when-not (str/blank? %)
                     (:pr_url (json/parse-string % true))))
            (line-seq r)))))

(defn read-jsonl
  "Parse PATH into a vector of records."
  [path]
  (->> (str/split-lines (slurp path))
       (remove str/blank?)
       (mapv #(json/parse-string % true))))
