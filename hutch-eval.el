;;; hutch-eval.el --- Headless entry points for batch evaluation -*- lexical-binding: t; -*-

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

;; Headless API for running hutch outside the magit UI.  Intended for
;; batch evaluation, CI integration, and benchmarking workflows.
;;
;; The Perfetto trace is the single artifact.  Callers set
;; `hutch-trace-dir' before invoking a review; findings, rounds, and
;; token counts are then extracted from the trace by downstream tooling
;; (see eval/eval/trace.clj).

;;; Code:

(require 'seq)
(require 'hutch-agent)
(require 'hutch-git)
(require 'hutch-instrument)
(require 'hutch-utils)

(defun hutch-review-headless (root scopes on-result)
  "Run a review of SCOPES under ROOT, calling ON-RESULT per scope.
ON-RESULT receives one result plist per scope as each completes.
No buffer, no rendering, no progress reporting.
Returns the list of cancel functions from `hutch--review'."
  (let ((default-directory root))
    (hutch--review scopes on-result (lambda (_s _c _m) nil))))

(defun hutch-review-sync (root scopes &optional timeout)
  "Run a review on ROOT and block until all SCOPES complete.
Returns the result plists in the order they completed.
TIMEOUT in seconds (default 600).  Signals an error if not all
scopes complete within the deadline.

Drives the event loop via `accept-process-output' so this works
under `emacs --batch' where no loop is otherwise running."
  (let ((results  '())
        (total    (length scopes))
        (deadline (+ (float-time) (or timeout 600))))
    (hutch-review-headless
     root scopes
     (lambda (result) (push result results)))
    (while (and (< (length results) total)
                (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (when (< (length results) total)
      (error "Hutch-review-sync timeout: %d/%d scopes completed"
             (length results) total))
    (nreverse results)))

;;; --- Batch entry point (used by the eval harness) ---

(defun hutch-eval-batch-review (repo-dir max-rounds)
  "Run a hutch review on REPO-DIR targeting the `:branch' scope.
Disables caching, raises the round budget to MAX-ROUNDS.

The Perfetto trace at `hutch-trace-dir' is the return artifact —
findings and stats are extracted from it downstream.  Emits
diagnostics to stderr (visible in batch mode) including backend
identity, scopes collected, per-scope status, and the full
`*hutch-log*' contents — useful for post-hoc inspection."
  (require 'hutch)
  (unless hutch-trace-dir
    (error "Hutch-eval-batch-review requires `hutch-trace-dir' to be set"))
  (let* ((default-directory       repo-dir)
         (hutch-cache-enabled     nil)
         (hutch-max-tool-rounds   max-rounds)
         (all-scopes              (hutch-collect-scopes))
         (scopes                  (seq-filter
                                   (lambda (s) (eq (plist-get s :scope) :branch))
                                   all-scopes)))
    (message "[eval] backend=%S model=%S"
             (and hutch-backend (gptel-backend-name hutch-backend))
             (and hutch-backend (car (gptel-backend-models hutch-backend))))
    (message "[eval] scopes-collected=%d kinds=%S branch-matches=%d"
             (length all-scopes)
             (mapcar (lambda (s) (plist-get s :scope)) all-scopes)
             (length scopes))
    (hutch--trace-begin)
    (unwind-protect
        (let ((results (hutch-review-sync repo-dir scopes 900)))
          (dolist (r results)
            (message "[eval] scope=%s status=%s findings=%d emsg=%S"
                     (plist-get r :scope)
                     (plist-get r :status)
                     (length (plist-get r :findings))
                     (plist-get r :emsg)))
          (with-current-buffer (get-buffer-create "*hutch-log*")
            (message "[eval-log]\n%s" (buffer-string)))
          ;; Also dump *gptel-log* if it exists — has the actual API
          ;; request/response bodies, essential for diagnosing HTTP errors.
          (when-let* ((gbuf (get-buffer "*gptel-log*")))
            (with-current-buffer gbuf
              (message "[gptel-log]\n%s" (buffer-string)))))
      (hutch--trace-end))))

(provide 'hutch-eval)

;;; hutch-eval.el ends here
