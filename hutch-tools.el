;;; hutch-tools.el --- Tool functions for the review agent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Akshay Gupta
;;
;; Author: Akshay Gupta
;; URL: https://github.com/adjaecent/magit-hutch

;;; Commentary:

;; Tool implementations exposed to the LLM: search_codebase, read_file,
;; surrounding_context, and submit_review.  Submission uses a closure
;; over a result-box so the agent loop can collect findings without
;; relying on global state.

;;; Code:

(require 'magit)
(require 'gptel-request)
(require 'hutch-utils)
(require 'hutch-git)
(require 'hutch-treesit)

;;; --- Tools ---

(defmacro hutch--with-cur-dir (root &rest body)
  "Run BODY with `default-directory' bound to ROOT."
  (declare (indent 1) (debug (form body)))
  `(let ((default-directory ,root))
     ,@body))

(defun hutch--tool-search-codebase (root pattern &optional file-glob)
  "Search ROOT for PATTERN using git grep.  Optionally filter by FILE-GLOB.
PATTERN is extended POSIX regex (use \\=`|\\=' for alternation,
\\=`()\\=' for grouping).  FILE-GLOB is a git pathspec; if it contains
\\=`**\\=' magic-glob syntax is applied automatically."
  (hutch--with-cur-dir
   root
   (hutch--log "tool" "search_codebase: %s %s" pattern (or file-glob ""))
   (let* ((pathspec (cond
                     ((null file-glob) ".")
                     ;; Plain `**/*.java' fails as a literal pathspec;
                     ;; wrap in `:(glob)' magic so it actually matches.
                     ((and (string-match-p "\\*\\*" file-glob)
                           (not (string-prefix-p ":(" file-glob)))
                      (concat ":(glob)" file-glob))
                     (t file-glob)))
          (raw (with-temp-buffer
                 (magit-git-insert "grep" "-n" "-E" "-e" pattern "--" pathspec)
                 (buffer-string)))
          ;; Cap output so a too-broad regex doesn't blow up the model's
          ;; context or drown the per-PR log.  Return up to ~80 matches
          ;; and tell the model to narrow if it wants more.
          (lines (split-string raw "\n" t))
          (max-lines 80)
          (truncated? (> (length lines) max-lines))
          (kept (seq-take lines max-lines))
          (result (string-join kept "\n"))
          (result (if truncated?
                      (format "%s\n[... truncated: %d/%d matches shown. Narrow with a more specific pattern or a file-glob.]"
                              result max-lines (length lines))
                    result)))
     (hutch--log "tool" "search_codebase: %d chars returned (%d/%d matches)"
                 (length result) (length kept) (length lines))
     (if (string-empty-p result) "No matches found." result))))

(defun hutch--tool-read-file (root scope path &optional start-line end-line)
  "Read PATH at the revision for SCOPE relative to ROOT.
For staged scope reads the index version; for branch/unpushed reads at HEAD.
Optionally restrict to START-LINE..END-LINE."
  (hutch--with-cur-dir
   root
   (let* ((head   (plist-get scope :head))
          (staged (and (null head) (null (plist-get scope :base))))
          (ref    (if staged (format ":%s" path) (format "%s:%s" (or head "HEAD") path))))
     (hutch--log "tool" "read_file: %s [%s-%s]" path (or start-line "1") (or end-line "end"))
     (if (not (magit-git-success "cat-file" "-e" ref))
         (format "File not found: %s" path)
       (let* ((content (with-temp-buffer
                         (magit-git-insert "show" ref)
                         (buffer-string)))
              (lines (split-string content "\n"))
              (start (max 0 (1- (or start-line 1))))
              (end   (min (length lines) (or end-line (length lines))))
              (slice (seq-subseq lines start end)))
         (hutch--log "tool" "read_file: %d lines returned" (length slice))
         (string-join slice "\n"))))))

(defun hutch--tool-git-log (root path &optional max-count)
  "Show commit history for PATH within ROOT.
Return up to MAX-COUNT entries (default 10)."
  (hutch--with-cur-dir
   root
   (let* ((n (number-to-string (or max-count 10)))
          (result (hutch--git-log path n)))
     (hutch--log "tool" "git_log: %s (max %s), %d chars returned" path n (length result))
     (if (string-empty-p result) "No history found." result))))

(defun hutch--tool-git-blame (root path &optional start-line end-line)
  "Show git blame for PATH within ROOT.
Optionally restrict to START-LINE..END-LINE."
  (hutch--with-cur-dir
   root
   (hutch--log "tool" "git_blame: %s [%s-%s]" path (or start-line "1") (or end-line "end"))
   (let ((result (hutch--git-blame path start-line end-line)))
     (hutch--log "tool" "git_blame: %d chars returned" (length result))
     (if (string-empty-p result) "No blame data found." result))))

(defun hutch--tool-verify-patch-for-scope (root scope patch)
  "Dry-run PATCH with git apply --check for SCOPE within ROOT.
Returns \"OK\" on success or \"FAIL: <git error>\" on failure."
  (hutch--with-cur-dir
   root
   (let* ((result (hutch--patch-apply scope patch t))
          (ok    (car result))
          (out   (cdr result)))
     (hutch--log "tool" "verify_patch: %d chars [%s] → %s"
                 (length patch) (plist-get scope :scope) (if ok "OK" out))
     (if ok
         "OK — patch applies cleanly."
       (format "FAIL — git apply: %s" out)))))

(defun hutch--tool-surrounding-context (root path line &optional depth)
  "Return enclosing definitions around LINE in PATH within ROOT using Tree-sitter.
Returns up to DEPTH levels (default 1), innermost first."
  (hutch--with-cur-dir
   root
   (let ((full-path (expand-file-name path default-directory)))
     (hutch--log "tool" "surrounding_context: %s:%d (depth %s)" path line (or depth 1))
     (if (not (file-exists-p full-path))
         (format "File not found: %s" path)
       (let ((lang (hutch--treesit-lang-for-file path)))
         (if (null lang)
             (format "No tree-sitter grammar for %s" (file-name-extension path))
           (or (hutch--treesit-enclosing-definition lang full-path line depth)
               (format "No enclosing definition found at %s:%d" path line))))))))

(defun hutch--tool-read-diff-for-scope (root scope path)
  "Return the full diff for PATH within SCOPE rooted at ROOT."
  (hutch--with-cur-dir
   root
   (hutch--log "tool" "read_diff: %s [%s]" path (plist-get scope :scope))
   (or (hutch--git-diff scope path) "No diff found.")))

;;; --- Tool definitions ---

(defcustom hutch-enabled-tools '(search-codebase surrounding-context)
  "Optional tools available to the review agent.
The core tools (read-diff, verify-patch, submit-review) are always included."
  :type '(set (const :tag "Search codebase" search-codebase)
              (const :tag "Git log" git-log)
              (const :tag "Git blame" git-blame)
              (const :tag "Surrounding context (Tree-sitter)" surrounding-context)
              (const :tag "Read contents of a file" read-file))
  :group 'hutch)

(defun hutch--make-tools (scope result-box)
  "Return gptel tools for a review, with SCOPE and RESULT-BOX captured.
Always includes core tools; optional tools filtered by `hutch-enabled-tools'."
  (let ((root (magit-toplevel)))
    (append
     (list
      (gptel-make-tool
       :function (lambda (path) (hutch--tool-read-diff-for-scope root scope path))
      :name "read_diff"
      :description "Read the full diff for a specific file in the current review scope. \
Use this to inspect the actual changes before submitting findings."
      :args '((:name "path"
                     :type string
                     :description "File path from the manifest")))
     (gptel-make-tool
      :function (lambda (patch) (hutch--tool-verify-patch-for-scope root scope patch))
      :name "verify_patch"
      :description "Dry-run a unified diff patch with git apply --check. \
Returns \"OK\" if the patch applies cleanly, or the git error if not. \
Call this before submit_review for any finding that includes a patch. \
For new files, the patch source must be \"--- /dev/null\", not \"--- a/file\"."
      :args '((:name "patch"
                     :type string
                     :description "Unified diff patch to verify")))
     (gptel-make-tool
      :function (lambda (findings)
                  (hutch--log "tool" "submit_review: %d findings" (length findings))
                  (hutch--result-box-set result-box (append findings nil))
                  "Review submitted.")
      :name "submit_review"
      :description "Submit your final code review findings. You MUST call this \
exactly once when your review is complete. Every review must end with this call."
      :args '((:name "findings"
                     :type array
                     :description "Array of review findings"
                     :items (:type object
                                   :properties
                                   (:file (:type string :description "File path")
                                          :lines (:type string
                                                        :description "Line range of the actual changed \
lines (the + or - lines), not the hunk header start. \
E.g. if @@ -21,6 +21,6 @@ and the changed line is 3rd in the hunk, \
the line number is 23, not 21. \
Never use line numbers from read_file or search_codebase.")
                                          :title (:type string :description "Short issue title, max 80 chars")
                                          :description (:type string
                                                              :description "Explanation of the issue, max 1000 chars")
                                          :patch (:type string
                                                        :description "Unified diff patch applicable with git apply, or null")
                                          :lgtm (:type boolean :description "true if file has no issues")))))))
     (when (memq 'read-file hutch-enabled-tools)
       (list (gptel-make-tool
              :function (lambda (path &optional start-line end-line)
                          (hutch--tool-read-file root scope path start-line end-line))
              :name "read_file"
              :description "Read the contents of a file, optionally restricted to a line range. \
Use this to inspect context outside the diff when a changed line references \
something not visible in the diff itself."
              :args '((:name "path"
                             :type string
                             :description "File path relative to repo root")
                      (:name "start_line"
                             :type integer
                             :description "First line to read (1-indexed)"
                             :optional t)
                      (:name "end_line"
                             :type integer
                             :description "Last line to read (1-indexed, inclusive)"
                             :optional t)))))
     (when (memq 'search-codebase hutch-enabled-tools)
       (list (gptel-make-tool
              :function (lambda (pattern &optional file-glob)
                          (hutch--tool-search-codebase root pattern file-glob))
              :name "search_codebase"
              :description "Search the codebase using git grep (extended regex). \
Returns matching lines with file paths and line numbers. \
Use this to find callers, references, imports, or any text pattern."
              :args '((:name "pattern"
                             :type string
                             :description "Extended regex pattern. Use | for alternation, () for grouping, \\( to escape literals. Example: 'foo|bar|baz'.")
                      (:name "file_glob"
                             :type string
                             :description "Optional file filter. Examples: '*.py', '*.java', '**/*.el'. Recursive globs (**) are handled automatically."
                             :optional t)))))
     (when (memq 'git-log hutch-enabled-tools)
       (list (gptel-make-tool
              :function (lambda (path &optional max-count)
                          (hutch--tool-git-log root path max-count))
              :name "git_log"
              :description "Show recent commit history for a file. \
Use this to understand why code looks the way it does \
and what recent changes were made."
              :args '((:name "path"
                             :type string
                             :description "File path relative to repo root")
                      (:name "max_count"
                             :type integer
                             :description "Max number of commits to return (default 10)"
                             :optional t)))))
     (when (memq 'git-blame hutch-enabled-tools)
       (list (gptel-make-tool
              :function (lambda (path &optional start-line end-line)
                          (hutch--tool-git-blame root path start-line end-line))
              :name "git_blame"
              :description "Show git blame for a file, optionally for a \
specific line range. \
Use this to see who last modified lines and in what commit."
              :args '((:name "path"
                             :type string
                             :description "File path relative to repo root")
                      (:name "start_line"
                             :type integer
                             :description "First line to blame (1-indexed)"
                             :optional t)
                      (:name "end_line"
                             :type integer
                             :description "Last line to blame (1-indexed, inclusive)"
                             :optional t)))))
     (when (memq 'surrounding-context hutch-enabled-tools)
       (list (gptel-make-tool
              :function (lambda (path line &optional depth)
                          (hutch--tool-surrounding-context root path line depth))
              :name "surrounding_context"
              :description "Get enclosing definitions around a line using tree-sitter. \
Use this to understand the full context of a changed line \
without reading the entire file. \
Pass depth to get multiple levels (e.g. function + class), but avoid going above 2."
              :args '((:name "path"
                             :type string
                             :description "File path relative to repo root")
                      (:name "line"
                             :type integer
                             :description "Line number to find context for (1-indexed)")
                      (:name "depth"
                             :type integer
                             :description "Number of enclosing definitions to return (default 1)"
                             :optional t))))))))

(provide 'hutch-tools)

;;; hutch-tools.el ends here
