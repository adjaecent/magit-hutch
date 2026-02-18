;;; magit-hutch-tools.el --- Tool functions for the review agent -*- lexical-binding: t; -*-

;;; Code:

(require 'magit)
(require 'llm)
(require 'magit-hutch-debug)

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

;;; --- Tool definitions ---

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

(provide 'magit-hutch-tools)

;;; magit-hutch-tools.el ends here
