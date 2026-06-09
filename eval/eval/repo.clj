(ns eval.repo
  "Martian goldens fetch/load + shared git clones and per-PR worktrees."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :refer [sh shell]]
            [cheshire.core :as json]
            [clojure.string :as str]
            [eval.config :as cfg :refer [log]]))

;;; --- Goldens ---

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
  (fs/create-dirs cfg/goldens-dir)
  (doseq [{:keys [name download_url]} (gh-list-dir cfg/martian-repo cfg/martian-goldens-path)
          :when (str/ends-with? name ".json")]
    (log "  fetching" name)
    (spit (str cfg/goldens-dir "/" name) (:body (http/get download_url))))
  (log "Goldens written to" cfg/goldens-dir))

(defn load-goldens
  "Read PR entries from goldens-dir.  With REPO, only <REPO>.json."
  [& [repo]]
  (let [files (if repo
                [(str cfg/goldens-dir "/" repo ".json")]
                (fs/glob cfg/goldens-dir "*.json"))]
    (mapcat #(json/parse-string (slurp (str %)) true) files)))

;;; --- PR URL helpers ---

(defn pr-source-url
  "Prefer original_url, else url."
  [entry]
  (or (:original_url entry) (:url entry)))

(defn parse-pr-url
  "Parse 'github.com/OWNER/REPO/pull/N' → {:owner :repo :pr}."
  [url]
  (when-let [m (re-find #"github\.com/([^/]+)/([^/]+)/pull/(\d+)" url)]
    {:owner (nth m 1)
     :repo  (nth m 2)
     :pr    (Integer/parseInt (nth m 3))}))

(defn- pr-id [{:keys [owner repo pr]}]
  (str owner "__" repo "__pr" pr))

;;; --- Git / worktree ---

(defn- default-branch-name
  "Resolve origin/HEAD's target ref, e.g. 'main' or 'master'."
  [dir]
  (let [r (sh {:dir dir} "git" "symbolic-ref" "--short" "refs/remotes/origin/HEAD")]
    (when (zero? (:exit r))
      (-> r :out str/trim (str/replace #"^origin/" "")))))

(defn- ensure-shared-clone
  "Clone OWNER/REPO once into clones-dir.  Idempotent."
  [owner repo]
  (fs/create-dirs cfg/clones-dir)
  (let [name (str owner "__" repo)
        dir  (str cfg/clones-dir "/" name)
        url  (format "https://github.com/%s/%s.git" owner repo)]
    (when-not (fs/exists? dir)
      (log "  cloning shared" url)
      (shell {:dir cfg/clones-dir} "git" "clone" "--quiet" url name)
      (when-let [default (default-branch-name dir)]
        (sh {:dir dir} "git" "branch" "--force" default (str "origin/" default))))
    dir))

(defn- materialize-worktree
  "Fetch PR head into a branch in the shared clone and add a worktree
pointing at it.  Caller must ensure the worktree dir doesn't exist yet."
  [clone-dir wt-dir pr branch]
  (sh    {:dir clone-dir} "git" "branch" "-D" branch)
  (shell {:dir clone-dir} "git" "fetch" "--quiet" "origin"
         (format "pull/%d/head:%s" pr branch))
  (shell {:dir clone-dir} "git" "worktree" "add" "--quiet" wt-dir branch))

(defn ensure-repo
  "Return a worktree of OWNER/REPO with the PR head checked out.
Reuses existing worktrees; not safe for concurrent same-clone calls."
  [{:keys [pr owner repo] :as info}]
  (fs/create-dirs cfg/repo-cache)
  (let [clone-dir (ensure-shared-clone owner repo)
        wt-dir    (str cfg/repo-cache "/" (pr-id info))
        branch    (format "eval-pr-%d" pr)]
    (if (fs/exists? wt-dir)
      (do (log "  reuse" wt-dir) wt-dir)
      (do (materialize-worktree clone-dir wt-dir pr branch)
          wt-dir))))
