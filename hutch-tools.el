;;; hutch-tools.el --- Tool functions for the review agent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Akshay Gupta
;;
;; Author: Akshay Gupta <mail@kitallis.in>
;; Assisted-by: Claude:claude-opus-4-7
;; URL: https://github.com/adjaecent/magit-hutch
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tool implementations exposed to the LLM.  Core tools (always on):
;; read_diff, verify_block, submit_review.  Optional tools gated by
;; `hutch-enabled-tools': search_codebase, read_file, surrounding_context,
;; git_log, git_blame.  Submission uses a closure over a result-box so
;; the agent loop can collect findings without relying on global state.
;; Suggestions arrive as SEARCH/REPLACE blocks (`old_lines'/`new_lines');
;; the unified diff is generated server-side by `hutch--gate-patch'.

;;; Code:

(require 'magit)
(require 'gptel-request)
(require 'hutch-utils)
(require 'hutch-git)
(require 'hutch-treesit)
(require 'hutch-instrument)

;;; --- Tools ---

(defmacro hutch--with-cur-dir (root &rest body)
  "Run BODY with `default-directory' bound to ROOT."
  (declare (indent 1) (debug (form body)))
  `(let ((default-directory ,root))
     ,@body))

(defun hutch--glob-to-pathspec (file-glob)
  "Translate FILE-GLOB to a git pathspec.
Wraps `**' globs in `:(glob)' magic; leaves explicit pathspecs alone.
Returns \".\" (match everything) when FILE-GLOB is nil."
  (cond ((null file-glob) ".")
        ((and (string-match-p "\\*\\*" file-glob)
              (not (string-prefix-p ":(" file-glob)))
         (concat ":(glob)" file-glob))
        (t file-glob)))

(defun hutch--tool-search-codebase (root pattern &optional file-glob)
  "Search ROOT for PATTERN using git grep.  Optionally filter by FILE-GLOB.
PATTERN is extended POSIX regex (use \\=`|\\=' for alternation,
\\=`()\\=' for grouping).  FILE-GLOB is a git pathspec; if it contains
\\=`**\\=' magic-glob syntax is applied automatically."
  (hutch--with-cur-dir
   root
   (let* ((pathspec  (hutch--glob-to-pathspec file-glob))
          (lines     (magit-git-lines "grep" "-n" "-E" "-e" pattern "--" pathspec))
          (max-lines 80)
          (truncated (> (length lines) max-lines))
          (kept      (seq-take lines max-lines))
          (body      (string-join kept "\n")))
     (hutch--log "tool" "search_codebase: %s %s -> %d/%d matches"
                 pattern (or file-glob "") (length kept) (length lines))
     (cond
      ((null lines) "No matches found.")
      (truncated
       (format "%s\n[... truncated: %d/%d matches shown. Narrow with a more specific pattern or a file-glob.]"
               body max-lines (length lines)))
      (t body)))))

(defun hutch--tool-read-file (root scope path &optional start-line end-line)
  "Read PATH at the revision for SCOPE relative to ROOT.
For staged scope reads the index version; for branch/unpushed reads at HEAD.
Optionally restrict to START-LINE..END-LINE."
  (hutch--with-cur-dir
   root
   (hutch--log "tool" "read_file: %s [%s-%s]" path (or start-line "1") (or end-line "end"))
   (if-let* ((content (hutch--read-scope-file scope path)))
       (let* ((lines (split-string content "\n"))
              (start (max 0 (1- (or start-line 1))))
              (end   (min (length lines) (or end-line (length lines))))
              (slice (seq-subseq lines start end)))
         (hutch--log "tool" "read_file: %d lines returned" (length slice))
         (string-join slice "\n"))
     (format "File not found: %s" path))))

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

(defun hutch--tool-verify-block (root scope file old-lines)
  "Verify OLD-LINES uniquely matches in FILE within SCOPE rooted at ROOT.
Returns one of:
  OK: text found uniquely at line N in FILE
  NOT_FOUND in FILE.  Re-read the file and verify exact contents.
  AMBIGUOUS in FILE: matches N times.  Add more surrounding lines."
  (hutch--with-cur-dir
   root
   (let* ((source (hutch--read-scope-file scope file)))
     (cond
      ((null source)
       (let ((msg (format "File not found at scope ref: %s" file)))
         (hutch--log "tool" "verify_block: %s" msg)
         msg))
      (t
       (let ((hits (hutch--find-occurrences source old-lines)))
         (cond
          ((= 1 (length hits))
           (let ((msg (format "OK: text found uniquely at line %d in %s" (car hits) file)))
             (hutch--log "tool" "verify_block: %s" msg)
             msg))
          ((zerop (length hits))
           (let ((msg (format "NOT_FOUND in %s. Re-read the file and verify exact contents (including whitespace)." file)))
             (hutch--log "tool" "verify_block: not found in %s" file)
             msg))
          (t
           (let ((msg (format "AMBIGUOUS in %s: matches %d times. Add more surrounding lines to disambiguate."
                              file (length hits))))
             (hutch--log "tool" "verify_block: %d matches in %s" (length hits) file)
             msg)))))))))

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
The core tools (read-diff, verify-block, submit-review) are always included."
  :type '(set (const :tag "Search codebase" search-codebase)
              (const :tag "Git log" git-log)
              (const :tag "Git blame" git-blame)
              (const :tag "Surrounding context (Tree-sitter)" surrounding-context)
              (const :tag "Read contents of a file" read-file))
  :group 'hutch)

(defun hutch--make-tools (scope result-box)
  "Return gptel tools for a review, with SCOPE and RESULT-BOX captured.
Always includes core tools; optional tools filtered by `hutch-enabled-tools'."
  (let ((root      (magit-toplevel))
        (scope-id  (plist-get scope :shash)))
    (append
     (list
      (gptel-make-tool
       :function (lambda (path)
                   (hutch--with-trace "read_diff" "tool" scope-id `(:p ,path)
                     (hutch--tool-read-diff-for-scope root scope path)))
      :name "read_diff"
      :description "Read the full diff for a specific file in the current review scope. \
Use this to inspect the actual changes before submitting findings."
      :args '((:name "path"
                     :type string
                     :description "File path from the manifest")))
     (gptel-make-tool
      :function (lambda (file old_lines)
                  (hutch--with-trace "verify_block" "tool" scope-id `(:f ,file :l ,old_lines)
                    (hutch--tool-verify-block root scope file old_lines)))
      :name "verify_block"
      :description "Check that OLD_LINES uniquely matches in FILE at the \
scope's reference (index for staged, HEAD for branch/unpushed). \
Returns:
  OK: unique match at line N
  NOT_FOUND with closest-candidate context
  AMBIGUOUS with all matching lines and surrounding context
Use this freely during iteration to confirm a block is unique before \
including it in submit_review.  Cheap; no git/network involved."
      :args '((:name "file"
                     :type string
                     :description "File path relative to repo root")
              (:name "old_lines"
                     :type string
                     :description "Exact verbatim text from the file to verify is uniquely matchable")))
     (gptel-make-tool
      :function (lambda (findings)
                  (let ((n (length findings)))
                    (hutch--with-trace "submit_review" "tool" scope-id `(:n ,n)
                      (hutch--log "tool" "submit_review: %d findings" n)
                      (hutch--result-box-set result-box (append findings nil))
                      (hutch--trace-instant "parse-response" "tool" scope-id
                                            `(:findings ,(vconcat findings)))))
                  "Review submitted.")
      :name "submit_review"
      :description "Submit your final code review findings. You MUST call this \
exactly once when your review is complete. Every review must end with this call. \
Suggestions use SEARCH/REPLACE blocks: OLD_LINES is the exact verbatim text from \
the file (must match uniquely — use verify_block first if unsure); NEW_LINES is \
the replacement text. The unified diff is generated server-side; you do not \
author diff syntax."
      :args '((:name "findings"
                     :type array
                     :description "Array of review findings"
                     :items (:type object
                                   :properties
                                   (:file (:type string :description "File path relative to repo root")
                                          :title (:type string :description "Short issue title, max 80 chars")
                                          :description (:type string
                                                              :description "Explanation of the issue, max 1000 chars")
                                          :old_lines (:type string
                                                            :description "Exact verbatim text to replace. \
MUST appear in the file exactly once.  Copy whitespace and indentation verbatim. \
If your text matches multiple locations, include more surrounding lines for uniqueness. \
Omit for comment-only findings.")
                                          :new_lines (:type string
                                                            :description "Replacement text.  Omit for comment-only findings.")
                                          :lines (:type string
                                                        :description "Line anchor for comment-only findings, e.g. \"42\" or \"42-50\" \
or comma-separated for multiple spots like \"42, 90, 120\".  Omit when submitting \
old_lines/new_lines — :lines is auto-derived from the match position.")
                                          :lgtm (:type boolean :description "true if file has no issues")))))))
     (when (memq 'read-file hutch-enabled-tools)
       (list (gptel-make-tool
              :function (lambda (path &optional start-line end-line)
                          (hutch--with-trace "read_file" "tool" scope-id `(:p ,path :s ,start-line :e ,end-line)
                            (hutch--tool-read-file root scope path start-line end-line)))
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
                          (hutch--with-trace "search_codebase" "tool" scope-id `(:pt ,pattern :fg ,file-glob)
                            (hutch--tool-search-codebase root pattern file-glob)))
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
                          (hutch--with-trace "git_log" "tool" scope-id `(:p ,path)
                            (hutch--tool-git-log root path max-count)))
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
                          (hutch--with-trace "git_blame" "tool" scope-id `(:p ,path)
                            (hutch--tool-git-blame root path start-line end-line)))
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
                          (hutch--with-trace "surrounding_context" "tool" scope-id `(:p ,path :l ,line)
                            (hutch--tool-surrounding-context root path line depth)))
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
