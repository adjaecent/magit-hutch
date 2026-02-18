;;; hutch-debug.el --- Debug logging -*- lexical-binding: t; -*-

;;; Code:

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

(provide 'hutch-debug)

;;; hutch-debug.el ends here
