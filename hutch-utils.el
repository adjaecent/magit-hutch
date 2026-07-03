;;; hutch-utils.el --- Utility functions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Akshay Gupta
;;
;; Author: Akshay Gupta <mail@kitallis.in>
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

;; Small utilities shared across hutch modules: a mutable result-box
;; for closure-based result collection, string helpers, and logging.

;;; Code:

;;; --- Result box ---

(defun hutch--make-result-box ()
  "Create a mutable box holding nil."
  (cons nil nil))

(defun hutch--result-box-set (box val)
  "Set BOX contents to VAL."
  (setcar box val))

(defun hutch--result-box-get (box)
  "Get current contents of BOX."
  (car box))

;;; --- JSON helpers ---

(defun hutch--jsonify-value (v)
  "Coerce gptel's JSON sentinels in V to values `json-serialize' accepts.
Walks lists and vectors recursively.  Handles `:json-false' → `:false'
and `:json-null' → `:null'.  Other values pass through unchanged.

Called at boundaries where gptel-parsed JSON enters hutch (currently
the `submit_review' tool handler) so downstream code — result-box,
gates, cache, trace, UI — never sees the sentinels."
  (cond ((eq v :json-false) :false)
        ((eq v :json-null)  :null)
        ((consp v)          (mapcar #'hutch--jsonify-value v))
        ((vectorp v)        (vconcat (mapcar #'hutch--jsonify-value v)))
        (t                  v)))

;;; --- Text helpers ---

(defun hutch--str-truncate (str max)
  "Truncate STR to MAX chars, appending ellipsis if needed."
  (if (> (length str) max)
      (concat (substring str 0 (- max 1)) "\u2026")
    str))

(defun hutch--wrap-text (text width prefix)
  "Wrap TEXT to WIDTH, prefixing each line with PREFIX."
  (with-temp-buffer
    (insert text)
    (let ((fill-column (- width (length prefix)))
          (fill-prefix prefix))
      (fill-region (point-min) (point-max))
      (goto-char (point-min))
      (insert prefix)
      (buffer-string))))

(defun hutch--parse-lines (lines)
  "Parse LINES (a finding's :lines string) into a (START . END) cons.
Single \"45\" yields (45 . 45)."
  (when (and lines (stringp lines))
    (if (string-match "\\`\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?\\'" lines)
        (let ((start (string-to-number (match-string 1 lines)))
              (end   (match-string 2 lines)))
          (cons start (if end (string-to-number end) start))))))

;;; --- Logging ---

(defconst hutch--log-buffer "*hutch-log*"
  "Buffer name for hutch debug output.")

(defun hutch--log (tag fmt &rest args)
  "Log a message to the hutch debug buffer.
TAG is a short label (e.g. \"tool\", \"llm\", \"parse\").
FMT and ARGS are passed to `format'."
  (let ((msg (apply #'format fmt args))
        (ts (format-time-string "%T")))
    (with-current-buffer (get-buffer-create hutch--log-buffer)
      (goto-char (point-max))
      (insert (format "[%s] [%s] %s\n" ts tag msg)))))

;;;###autoload
(defun hutch-show-log ()
  "Display the hutch debug log buffer."
  (interactive)
  (display-buffer (get-buffer-create hutch--log-buffer)))

;;; --- Reload ---

(defconst hutch--source-directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory where magit-hutch source files live.")

;;;###autoload
(defun hutch-reload ()
  "Reload all magit-hutch source files."
  (interactive)
  (mapc #'load-file
        (file-expand-wildcards
         (expand-file-name "*.el" hutch--source-directory)))
  (message "Hutch: reloaded from %s" hutch--source-directory))

(provide 'hutch-utils)

;;; hutch-utils.el ends here
