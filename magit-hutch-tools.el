;;; magit-hutch-tools.el --- Tool functions for the review agent -*- lexical-binding: t; -*-

;;; Code:

(require 'magit)
(require 'llm)
(require 'magit-hutch-debug)
(require 'magit-hutch-git)

;;; --- Tool functions ---

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

;;; --- Tree-sitter context ---

(defvar hutch--treesit-lang-alist
  '(("el" . elisp) ("py" . python) ("rb" . ruby) ("rs" . rust)
    ("js" . javascript) ("ts" . typescript) ("tsx" . tsx)
    ("go" . go) ("java" . java) ("c" . c) ("cpp" . cpp)
    ("clj" . clojure) ("ex" . elixir) ("exs" . elixir))
  "Map file extensions to tree-sitter language symbols.")

(defvar hutch--treesit-definition-types
  '("function_definition" "function_declaration" "method_definition"
    "method_declaration" "class_definition" "class_declaration"
    "defun" "special_form" "module_definition" "impl_item"
    "function_item" "struct_item")
  "Tree-sitter node types that represent definitions.")

(defun hutch--treesit-lang-for-file (path)
  "Return the tree-sitter language symbol for PATH, or nil."
  (let ((ext (file-name-extension path)))
    (alist-get ext hutch--treesit-lang-alist nil nil #'equal)))

(defun hutch--treesit-enclosing-definition (lang full-path line)
  "Find the enclosing definition node at LINE in FULL-PATH for LANG.
Returns the node text with line numbers, or nil."
  (when (treesit-language-available-p lang)
    (with-temp-buffer
      (insert-file-contents full-path)
      (let* ((parser (treesit-parser-create lang))
             (pos (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- line))
                    (point)))
             (node (treesit-node-at pos))
             (def (treesit-parent-until
                   node
                   (lambda (n)
                     (member (treesit-node-type n)
                             hutch--treesit-definition-types)))))
        (when def
          (let* ((start-line (line-number-at-pos (treesit-node-start def)))
                 (text (treesit-node-text def)))
            (format "Lines %d-%d:\n%s"
                    start-line
                    (+ start-line (length (split-string text "\n" t)) -1)
                    text)))))))

(defun hutch--tool-surrounding-context (path line)
  "Return the enclosing definition around LINE in PATH using tree-sitter."
  (let* ((default-directory (magit-toplevel))
         (full-path (expand-file-name path default-directory)))
    (hutch--log "tool" "surrounding_context: %s:%d" path line)
    (if (not (file-exists-p full-path))
        (format "File not found: %s" path)
      (let ((lang (hutch--treesit-lang-for-file path)))
        (if (null lang)
            (format "No tree-sitter grammar for %s" (file-name-extension path))
          (or (hutch--treesit-enclosing-definition lang full-path line)
              (format "No enclosing definition found at %s:%d" path line)))))))

;;; --- Read diff ---

(defun hutch--tool-read-diff-for-scope (scope path)
  "Return the full diff for PATH within SCOPE."
  (let ((default-directory (magit-toplevel)))
    (hutch--log "tool" "read_diff: %s [%s]" path (plist-get scope :scope))
    (or (hutch--git-diff scope path) "No diff found.")))

;;; --- Submit review ---

(defvar hutch--submitted-findings nil
  "Findings from the last `submit_review' tool call.
Set by the tool function, consumed by the review loop.")

(defun hutch--tool-submit-review (findings)
  "Receive FINDINGS from the LLM and store them.
FINDINGS is a vector of alists, each with keys like
`file', `title', `description', `lines', `patch', `lgtm'."
  (hutch--log "tool" "submit_review: %d findings" (length findings))
  (setq hutch--submitted-findings (append findings nil))
  "Review submitted.")

;;; --- Tool definitions ---

(defun hutch--make-read-diff-tool (scope)
  "Return a read_diff tool with SCOPE captured in its closure."
  (llm-make-tool
   :function (lambda (path) (hutch--tool-read-diff-for-scope scope path))
   :name "read_diff"
   :description "Read the full diff for a specific file in the current review scope. \
Use this to inspect the actual changes before submitting findings."
   :args (list '(:name "path"
                 :type string
                 :description "File path from the manifest"))))

(defun hutch--tools-for-scope (scope)
  "Return the tool list with a read_diff tool bound to SCOPE."
  (cons (hutch--make-read-diff-tool scope) hutch--tools))

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
                  :optional t)))
   (llm-make-tool
    :function #'hutch--tool-surrounding-context
    :name "surrounding_context"
    :description "Get the enclosing function or class definition around a line \
using tree-sitter. Use this to understand the full context of a changed line \
without reading the entire file."
    :args (list '(:name "path"
                  :type string
                  :description "File path relative to repo root")
                '(:name "line"
                  :type integer
                  :description "Line number to find context for (1-indexed)")))
   (llm-make-tool
    :function #'hutch--tool-submit-review
    :name "submit_review"
    :description "Submit your final code review findings. You MUST call this \
exactly once when your review is complete. Every review must end with this call."
    :args (list
           `(:name "findings"
             :type array
             :description "Array of review findings"
             :items (:type object
                     :properties
                     (:file (:type string :description "File path")
                      :lines (:type string :description "Line range from the diff hunk headers, e.g. \"21-22\" or \"45\". Must reference actual changed lines, not the full file.")
                      :title (:type string :description "Short issue title, max 80 chars")
                      :description (:type string :description "1-2 sentence explanation, max 300 chars")
                      :patch (:type string :description "Unified diff patch applicable with git apply, or null")
                      :lgtm (:type boolean :description "true if file has no issues")))))))
  "Tools available to the hutch review agent.")

(provide 'magit-hutch-tools)

;;; magit-hutch-tools.el ends here
