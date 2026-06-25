;;; hutch-instrument.el --- Emit perfetto traces -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Akshay Gupta
;;
;; Author: Akshay Gupta
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

;; Emits Perfetto Chrome trace event JSON for review runs.  Disabled
;; by default; set `hutch-trace-dir' to enable.  Traces can be opened
;; at https://ui.perfetto.dev to inspect agent timing and tool calls.

;;; Code:

(require 'subr-x)

(defvar hutch--trace-events nil
  "Vector of trace events collected for the current review.")
(defvar hutch--trace-t0 nil
  "Reference time (from `float-time') at which the current trace started.")
(defvar hutch--trace-pid nil
  "Synthetic process id used for the current trace.")
(defvar hutch--trace-file nil
  "Base filename for the current trace, written under `hutch-trace-dir'.")

(defcustom hutch-trace-dir nil
  "Directory where hutch perfetto traces are written.
When nil, tracing is disabled."
  :type '(choice (const :tag "Disabled" nil) directory)
  :group 'hutch)

(defun hutch--trace-ts ()
  "Return microseconds elapsed since the current trace started."
  (truncate (* 1e6 (- (float-time) hutch--trace-t0))))

(defun hutch--trace-emit (evt tid)
  "Append EVT to the current trace under thread id TID.
EVT is a plist representing one Chrome trace event; ts/pid/tid are
filled in here.  No-op unless tracing is active.

Caller invariant: every value inside EVT (including nested values
under `:args') must be JSON-safe — strings, numbers, vectors, plists
with keyword keys, t, nil, :false, or :null.  In particular, bare
symbols and keywords used as VALUES (not keys) will trip
`json-serialize' at flush time; stringify them at the emission site
before calling this function."
  (when (and hutch--trace-t0 hutch-trace-dir)
    (let ((evt (thread-first evt
                             (plist-put :ts (hutch--trace-ts))
                             (plist-put :pid hutch--trace-pid)
                             (plist-put :tid tid)
                             (vector))))
      (setq hutch--trace-events
            (vconcat hutch--trace-events evt)))))

(defun hutch--trace-counter (name tid args)
  "Emit a counter event for NAME on thread TID.
ARGS is a plist of named series."
  (hutch--trace-emit `(:name ,name :ph "C" :args ,args) tid))

(defun hutch--trace-instant (name cat tid &optional args)
  "Emit an instant (point-in-time) event for NAME under CAT on thread TID.
Optional ARGS is a plist attached to the event."
  (hutch--trace-emit `(:name ,name :cat ,cat :ph "i" :args ,args) tid))

(defun hutch--trace-meta-thread-name (tid label)
  "Label thread TID as LABEL in the Perfetto view."
  (hutch--trace-emit
   `(:name "thread_name" :ph "M" :args (:name ,label)) tid))

(defun hutch--trace-slice-beg (name cat tid &optional args)
  "Emit a slice-begin event for NAME under category CAT and thread TID.
Optional ARGS plist is attached to the event."
  (hutch--trace-emit `(:name ,name :cat ,cat :ph "B" :args ,args) tid))

(defun hutch--trace-slice-end (name cat tid &optional args)
  "Emit a slice-end event for NAME under category CAT and thread TID.
Optional ARGS plist is attached to the event."
  (hutch--trace-emit `(:name ,name :cat ,cat :ph "E" :args ,args) tid))

(defmacro hutch--with-trace (name cat tid args &rest body)
  "Run BODY as a B/E slice for NAME under CAT and TID.
ARGS is a plist attached to the begin event.  The end event fires
in an `unwind-protect' cleanup so the slice closes even if BODY
signals."
  (declare (indent 4) (debug (form form form form body)))
  `(progn
     (hutch--trace-slice-beg ,name ,cat ,tid ,args)
     (unwind-protect
         (progn ,@body)
       (hutch--trace-slice-end ,name ,cat ,tid nil))))

(defun hutch--trace-begin ()
  "Start a new trace session.
No-op unless `hutch-trace-dir' is set."
  (when hutch-trace-dir
    (let ((ts (format-time-string "%s")))
      (setq hutch--trace-t0 (float-time)
            hutch--trace-events nil
            hutch--trace-pid (string-to-number ts)
            hutch--trace-file (format "hutch-trace-%s.json" ts)))))

(defun hutch--trace-write (dir-path)
  "Write the accumulated trace events to DIR-PATH as Chrome trace JSON."
  (let* ((dir  (expand-file-name dir-path))
         (path (expand-file-name hutch--trace-file dir))
         (coding-system-for-write 'utf-8))
    (make-directory dir t)
    (with-temp-file path
      (insert (json-serialize `(:traceEvents ,(vconcat hutch--trace-events)))))))

(defun hutch--trace-end ()
  "Flush the current trace to `hutch-trace-dir' and reset state."
  (when hutch--trace-t0
    (when hutch-trace-dir
      (hutch--trace-write hutch-trace-dir))
    (setq hutch--trace-t0 nil
          hutch--trace-events nil
          hutch--trace-pid nil
          hutch--trace-file nil)))

(provide 'hutch-instrument)

;;; hutch-instrument.el ends here
