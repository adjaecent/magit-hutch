;;; hutch.el --- AI code review for magit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 adjaecent
;;
;; Author: adjaecent
;; URL: https://github.com/adjaecent/magit-hutch
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit "4.0") (llm "0.20"))

;;; Commentary:

;; Hutch provides AI-powered code review integrated with magit.
;; It reviews staged changes, unpushed commits, and branch diffs
;; using an LLM with tool-calling for codebase context.

;;; Code:

(require 'magit)
(require 'llm)
(require 'seq)
(require 'json)

;;; --- User-facing options ---

(defvar hutch-provider nil
  "LLM provider for hutch code reviews.
Set this to an `llm' provider instance, e.g.:
  (setq hutch-provider
    (make-llm-claude :key (getenv \"ANTHROPIC_API_KEY\")
                     :chat-model \"claude-sonnet-4-5-20250929\"))")

(defvar hutch-system-prompt
  "You are a precise code reviewer. Your job is to identify substantive issues \
in code diffs: bugs, logic errors, edge cases, security vulnerabilities, race \
conditions, and correctness problems.

Rules:
- Do NOT provide general feedback, summaries, explanations of changes, or praise.
- Focus solely on specific, objective issues based on the diff context.
- Do not make broad comments about potential system impacts or question intentions.
- If a file has no issues, respond with a single entry with lgtm: true for that file."
  "System prompt for hutch code review.")

(defvar hutch-review-template
  "Review this diff and respond with a JSON array of findings.

Each element must have these fields:
- \"file\": the file path
- \"lines\": a string like \"21-22\" or \"45\" for the line range in the new file
- \"title\": short issue title
- \"description\": explanation of the issue
- \"patch\": a complete unified diff patch that can be applied with `git apply`, or null if no fix
- \"lgtm\": boolean, true only if the file has no issues

The patch field MUST be a valid unified diff with --- a/ and +++ b/ headers and @@ hunk headers.
Use the correct line numbers from the original diff context.

If a file is clean, include one entry: {\"file\": \"path\", \"lgtm\": true}

Diff to review:

%s"
  "Prompt template for hutch review. Expects a single %s for the diff.")

(defvar hutch-reasoning 'medium
  "Reasoning level for the review prompt.
One of nil, `none', `light', `medium', or `maximum'.")

(defconst hutch--review-example
  (cons "Review this diff:

--- a/utils.py
+++ b/utils.py
@@ -20,3 +20,3 @@
 def add(x, y):
-    retrn x + y
+    return x + y

--- a/config.py
+++ b/config.py
@@ -1,3 +1,3 @@
 DEBUG = True"
        "[{\"file\": \"utils.py\", \"lines\": \"21-22\", \"title\": \"Typo causes NameError\", \
\"description\": \"retrn is not a valid Python keyword.\", \
\"patch\": \"--- a/utils.py\\n+++ b/utils.py\\n@@ -20,3 +20,3 @@\\n def add(x, y):\\n-    retrn x + y\\n+    return x + y\", \
\"lgtm\": false}, {\"file\": \"config.py\", \"lgtm\": true}]")
  "Few-shot example for hutch review. Cons of (input . expected-output).")

;;; --- Scopes ---
;;
;; A scope is a plist describing one layer of diffing:
;;   :scope  -- keyword (:staged, :unpushed, :branch)
;;   :head   -- the head ref (e.g. "HEAD", or nil for staged)
;;   :base   -- the base ref (e.g. "main", "origin/feat", or nil for staged)
;;   :desc   -- human-readable description (derived)
;;   :diff   -- the diff string
;;   :hash   -- sha256 of :diff (for caching)

(defconst hutch--valid-scopes '(:staged :unpushed :branch)
  "Valid scope keywords.")

(defun hutch--make-scope (scope head base diff)
  "Create a scope plist for SCOPE with HEAD ref, BASE ref, and DIFF string.
SCOPE must be one of `hutch--valid-scopes'. Computes sha256 of DIFF."
  (unless (memq scope hutch--valid-scopes)
    (error "Invalid scope %s, must be one of %s" scope hutch--valid-scopes))
  (list :scope scope
        :head  head
        :base  base
        :diff  diff
        :hash  (secure-hash 'sha256 diff)
        :desc  (if (and head base) (format "%s..%s" base head) "")))

;;; --- Git helpers ---

(defun hutch--symbolic-ref ()
  "Return the symbolic ref for origin/HEAD."
  (magit-git-string "symbolic-ref" "refs/remotes/origin/HEAD"))

(defun hutch--merge-base (a b)
  "Return the merge-base commit between A and B."
  (magit-git-string "merge-base" a b))

(defun hutch--default-remote-ref ()
  "Return the default branch name from origin/HEAD."
  (when-let ((ref (hutch--symbolic-ref)))
    (string-remove-prefix "refs/remotes/origin/" ref)))

(defconst hutch--default-branch-names '("main" "master")
  "Common default branch names to try, in order.")

(defun hutch--default-common-ref ()
  "Return the first branch from `hutch--default-branch-names' that exists locally."
  (seq-find #'magit-branch-p hutch--default-branch-names))

(defun hutch--default-branch ()
  "Return the default branch name. Tries origin/HEAD, then common names."
  (or (hutch--default-remote-ref)
      (hutch--default-common-ref)))

;;; --- Diff collection ---

(defun hutch--git-diff (&rest args)
  "Run git diff with ARGS, return the output string or nil if empty."
  (let ((diff (with-temp-buffer
                (apply #'magit-git-insert "diff" args)
                (buffer-string))))
    (and diff (not (string-empty-p diff)) diff)))

(defun hutch--collect-branch ()
  "Collect the branch scope, or nil if on default branch."
  (let ((current (magit-get-current-branch))
        (default (hutch--default-branch)))
    (when (and current default (not (string= current default)))
      (let* ((base (or (hutch--merge-base default current) default))
             (diff (hutch--git-diff base "HEAD")))
        (when diff
          (hutch--make-scope :branch "HEAD" base diff))))))

(defun hutch--collect-unpushed ()
  "Collect the unpushed scope, or nil if no upstream or no diff."
  (when-let ((current (magit-get-current-branch)))
    (when-let ((upstream (magit-get-upstream-branch current)))
      (when-let ((diff (hutch--git-diff upstream "HEAD")))
        (hutch--make-scope :unpushed "HEAD" upstream diff)))))

(defun hutch--collect-staged ()
  "Collect the staged scope, or nil if nothing staged."
  (when-let ((diff (hutch--git-diff "--cached")))
    (hutch--make-scope :staged nil nil diff)))

(defun hutch-collect-scopes ()
  "Collect all available scopes. Returns a list of scope plists."
  (seq-filter #'identity
              (list (hutch--collect-branch)
                    (hutch--collect-unpushed)
                    (hutch--collect-staged))))

;;; --- Response parsing ---

(defun hutch--strip-code-fences (str)
  "Remove markdown code fences from STR if present."
  (if (string-prefix-p "```" str)
      (replace-regexp-in-string
       "^```[a-z]*\n?" ""
       (replace-regexp-in-string "\n?```$" "" str))
    str))

(defun hutch--parse-json (str)
  "Parse STR as JSON, return list of plists or nil on error."
  (condition-case err
      (let ((json-object-type 'plist)
            (json-array-type 'list)
            (json-key-type 'keyword))
        (json-read-from-string str))
    (error
     (message "hutch: failed to parse JSON: %s" (error-message-string err))
     nil)))

(defconst hutch--valid-finding-types '(lgtm suggestion comment)
  "Valid finding type symbols.")

(defun hutch--make-finding (type file lines title desc patch)
  "Create a finding plist of TYPE for FILE.
TYPE must be one of `hutch--valid-finding-types'."
  (unless (memq type hutch--valid-finding-types)
    (error "Invalid finding type %s, must be one of %s" type hutch--valid-finding-types))
  (list :type type
        :file file
        :lines lines
        :title title
        :desc desc
        :patch patch))

(defun hutch--normalize-finding (raw)
  "Normalize a RAW finding plist into a typed finding."
  (let ((file  (or (plist-get raw :file) "unknown"))
        (lgtm  (plist-get raw :lgtm))
        (patch (or (plist-get raw :patch)
                   (plist-get raw :fix)))
        (lines (or (plist-get raw :lines) "?"))
        (title (or (plist-get raw :title) "Issue"))
        (desc  (or (plist-get raw :description) "")))
    (cond
     ((eq lgtm t) (hutch--make-finding 'lgtm file nil nil nil nil))
     (patch       (hutch--make-finding 'suggestion file lines title desc patch))
     (t           (hutch--make-finding 'comment file lines title desc nil)))))

(defun hutch--parse-response (response)
  "Parse RESPONSE string into a list of normalized finding plists."
  (when-let ((raw (thread-last response
                               string-trim
                               hutch--strip-code-fences
                               hutch--parse-json)))
    (mapcar #'hutch--normalize-finding raw)))

;;; --- Debug log ---

(defconst hutch--log-buffer "*hutch-log*"
  "Buffer name for hutch debug output.")

(defun hutch--log (tag fmt &rest args)
  "Log a message to the hutch debug buffer.
TAG is a short label (e.g. \"tool\", \"llm\", \"parse\").
FMT and ARGS are passed to `format'."
  (let ((msg (apply #'format fmt args))
        (ts (format-time-string "%H:%M:%S")))
    (with-current-buffer (get-buffer-create hutch--log-buffer)
      (goto-char (point-max))
      (insert (format "[%s] [%s] %s\n" ts tag msg)))))

(defun hutch-show-log ()
  "Display the hutch debug log buffer."
  (interactive)
  (display-buffer (get-buffer-create hutch--log-buffer)))

;;; --- Tools ---

(defun hutch--tool-search-codebase (pattern &optional file-glob)
  "Search the codebase for PATTERN using git grep. Optionally filter by FILE-GLOB."
  (let ((default-directory (magit-toplevel)))
    (hutch--log "tool" "search_codebase: %s %s" pattern (or file-glob ""))
    (let ((result (with-temp-buffer
                    (apply #'magit-git-insert "grep" "-n" "-e" pattern "--"
                           (if file-glob (list file-glob) (list ".")))
                    (buffer-string))))
      (hutch--log "tool" "search_codebase: %d chars returned" (length result))
      (if (string-empty-p result) "No matches found." result))))

(defun hutch--tool-read-file (path &optional start-line end-line)
  "Read PATH relative to repo root. Optionally restrict to START-LINE..END-LINE."
  (let* ((default-directory (magit-toplevel))
         (full-path (expand-file-name path default-directory)))
    (hutch--log "tool" "read_file: %s [%s-%s]" path
                (or start-line "1") (or end-line "end"))
    (if (not (file-exists-p full-path))
        (format "File not found: %s" path)
      (with-temp-buffer
        (insert-file-contents full-path)
        (let* ((lines (split-string (buffer-string) "\n"))
               (start (max 0 (1- (or start-line 1))))
               (end (min (length lines) (or end-line (length lines))))
               (slice (seq-subseq lines start end)))
          (hutch--log "tool" "read_file: %d lines returned" (length slice))
          (string-join slice "\n"))))))

(defun hutch--tool-git-log (path &optional max-count)
  "Show commit history for PATH. Returns up to MAX-COUNT entries (default 10)."
  (let ((default-directory (magit-toplevel))
        (n (number-to-string (or max-count 10))))
    (hutch--log "tool" "git_log: %s (max %s)" path n)
    (let ((result (with-temp-buffer
                    (magit-git-insert "log" "--oneline" "--follow"
                                      (format "-n%s" n) "--" path)
                    (buffer-string))))
      (hutch--log "tool" "git_log: %d chars returned" (length result))
      (if (string-empty-p result) "No history found." result))))

(defun hutch--tool-git-blame (path &optional start-line end-line)
  "Show git blame for PATH. Optionally restrict to START-LINE..END-LINE."
  (let ((default-directory (magit-toplevel)))
    (hutch--log "tool" "git_blame: %s [%s-%s]" path
                (or start-line "1") (or end-line "end"))
    (let ((result (with-temp-buffer
                    (apply #'magit-git-insert "blame" "--porcelain"
                           (append (when (and start-line end-line)
                                     (list (format "-L%d,%d" start-line end-line)))
                                   (list "--" path)))
                    (buffer-string))))
      (hutch--log "tool" "git_blame: %d chars returned" (length result))
      (if (string-empty-p result) "No blame data found." result))))

(defvar hutch--tools
  (list
   (llm-make-tool
    :function #'hutch--tool-search-codebase
    :name "search_codebase"
    :description "Search the codebase for a pattern using git grep. \
Returns matching lines with file paths and line numbers. \
Use this to find callers, references, imports, or any text pattern."
    :args (list '(:name "pattern"
                  :type string
                  :description "Regex pattern to search for")
                '(:name "file_glob"
                  :type string
                  :description "Optional glob to filter files (e.g. \"*.py\", \"*.el\")"
                  :optional t)))
   (llm-make-tool
    :function #'hutch--tool-read-file
    :name "read_file"
    :description "Read the contents of a file in the repository. \
Optionally read only a specific line range. \
Use this to inspect type definitions, full function implementations, \
or surrounding context."
    :args (list '(:name "path"
                  :type string
                  :description "File path relative to repo root")
                '(:name "start_line"
                  :type integer
                  :description "First line to read (1-indexed)"
                  :optional t)
                '(:name "end_line"
                  :type integer
                  :description "Last line to read (1-indexed, inclusive)"
                  :optional t)))
   (llm-make-tool
    :function #'hutch--tool-git-log
    :name "git_log"
    :description "Show recent commit history for a file. \
Use this to understand why code looks the way it does \
and what recent changes were made."
    :args (list '(:name "path"
                  :type string
                  :description "File path relative to repo root")
                '(:name "max_count"
                  :type integer
                  :description "Max number of commits to return (default 10)"
                  :optional t)))
   (llm-make-tool
    :function #'hutch--tool-git-blame
    :name "git_blame"
    :description "Show git blame for a file, optionally for a \
specific line range. \
Use this to see who last modified lines and in what commit."
    :args (list '(:name "path"
                  :type string
                  :description "File path relative to repo root")
                '(:name "start_line"
                  :type integer
                  :description "First line to blame (1-indexed)"
                  :optional t)
                '(:name "end_line"
                  :type integer
                  :description "Last line to blame (1-indexed, inclusive)"
                  :optional t))))
  "Tools available to the hutch review agent.")

;;; --- LLM review ---

(defun hutch--make-prompt (scope)
  "Build an llm-chat-prompt for SCOPE with tools attached."
  (llm-make-chat-prompt
   (format hutch-review-template (plist-get scope :diff))
   :context hutch-system-prompt
   :examples (list hutch--review-example)
   :tools hutch--tools
   :reasoning hutch-reasoning
   :response-format 'json))

(defconst hutch--valid-result-statuses '(:ok :error)
  "Valid result status keywords.")

(defun hutch--response-text (response)
  "Extract text from RESPONSE, which may be a string or multi-output plist."
  (if (stringp response) response (plist-get response :text)))

(defun hutch--make-result (status scope findings emsg)
  "Create a result plist with STATUS for SCOPE.
STATUS must be one of `hutch--valid-result-statuses'."
  (unless (memq status hutch--valid-result-statuses)
    (error "Invalid result status %s, must be one of %s"
           status hutch--valid-result-statuses))
  (list :status   status
        :scope    (plist-get scope :scope)
        :desc     (plist-get scope :desc)
        :hash     (plist-get scope :hash)
        :findings findings
        :emsg     emsg))

(defun hutch--make-success-result (scope response)
  "Build a success result plist from SCOPE and RESPONSE."
  (let ((text (hutch--response-text response)))
    (hutch--log "llm" "success for %s: %d chars"
                (plist-get scope :scope) (length (or text "")))
    (hutch--make-result :ok
                        scope
                        (when text (hutch--parse-response text))
                        nil)))

(defun hutch--make-error-result (scope type msg)
  "Build an error result plist from SCOPE, error TYPE, and MSG."
  (hutch--log "llm" "error for %s: %s: %s"
              (plist-get scope :scope) type msg)
  (hutch--make-result :error
                      scope
                      nil
                      (format "%s: %s" type msg)))

(defun hutch-review-scope (scope callback)
  "Review SCOPE asynchronously with tool use. Call CALLBACK with a result plist."
  (unless hutch-provider
    (error "hutch-provider is not set"))
  (hutch--log "llm" "starting review for %s %s"
              (plist-get scope :scope) (plist-get scope :desc))
  (llm-chat-async
   hutch-provider
   (hutch--make-prompt scope)
   (lambda (response)
     (funcall callback (hutch--make-success-result scope response)))
   (lambda (type msg)
     (funcall callback (hutch--make-error-result scope type msg)))
   t))

(defun hutch-review (callback)
  "Review all available scopes. Call CALLBACK with each result plist as it arrives."
  (let ((scopes (hutch-collect-scopes)))
    (if (null scopes)
        (message "hutch: no changes to review")
      (hutch--log "review" "found %d scopes" (length scopes))
      (dolist (scope scopes)
        (hutch--log "review" "dispatching %s %s"
                    (plist-get scope :scope) (plist-get scope :desc))
        (hutch-review-scope scope callback)))))

(provide 'hutch)

;;; hutch.el ends here
