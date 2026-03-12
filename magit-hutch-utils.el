;;; magit-hutch-utils.el --- Utility functions -*- lexical-binding: t; -*-

;;; Commentary:
;;

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

;;; --- String helpers ---

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

;;; --- Logging ---

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

;;; --- Reload ---

(defconst hutch--source-directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory where magit-hutch source files live.")

(defun hutch-reload ()
  "Reload all magit-hutch source files."
  (interactive)
  (mapc #'load-file
        (file-expand-wildcards
         (expand-file-name "*.el" hutch--source-directory)))
  (message "Hutch: reloaded from %s" hutch--source-directory))

(provide 'magit-hutch-utils)

;;; magit-hutch-utils.el ends here
