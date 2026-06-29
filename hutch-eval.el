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
;; Typical usage from `emacs --batch':
;;
;;   (setq hutch-cache-enabled nil)
;;   (let* ((default-directory "/path/to/repo")
;;          (scopes  (hutch-collect-scopes))
;;          (results (hutch-review-sync default-directory scopes))
;;          (export  (hutch-eval-export-results results
;;                                              "https://github.com/org/repo/pull/42"
;;                                              "magit-hutch")))
;;     (with-temp-file "review.json"
;;       (insert (json-encode export))))

;;; Code:

(require 'json)
(require 'seq)
(require 'hutch-agent)
(require 'hutch-git)
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

(defun hutch-eval--finding-line (finding)
  "Return the start line of FINDING as an integer, or nil."
  (let ((parsed (hutch--parse-lines (plist-get finding :lines))))
    (and parsed (car parsed))))

(defun hutch-eval--finding-body (finding)
  "Render FINDING as the body string a judge sees.
Includes the proposed patch (when present) so the judge can recognize
issues by the code change, not just the prose description."
  (let* ((title (plist-get finding :title))
         (desc  (or (plist-get finding :desc) ""))
         (patch (plist-get finding :patch)))
    (if patch
        (format "%s\n\n%s\n\nProposed patch:\n```diff\n%s\n```"
                title desc patch)
      (format "%s\n\n%s" title desc))))

(defun hutch-eval--export-finding (finding)
  "Convert FINDING to an alist matching benchmark review-comment shape."
  `((path       . ,(plist-get finding :file))
    (line       . ,(hutch-eval--finding-line finding))
    (body       . ,(hutch-eval--finding-body finding))
    (has_patch  . ,(if (plist-get finding :patch) t :json-false))
    (created_at . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))))

(defun hutch-eval--exportable-p (finding)
  "Return non-nil if FINDING should appear in the benchmark export."
  (and (memq (plist-get finding :type) '(suggestion comment))
       (plist-get finding :file)
       (hutch-eval--finding-line finding)))

(defun hutch-eval-export-results (results pr-url tool-name)
  "Convert RESULTS to a benchmark-shaped alist.
RESULTS is a list of scope result plists from `hutch-review-sync'.
PR-URL identifies the PR; TOOL-NAME identifies the reviewer
\(e.g. \"magit-hutch\").

Drops lgtm findings and any findings missing :file or :lines.
Suggestions and comments both export.  Returns an alist suitable
for `json-encode'."
  (let* ((all-findings (mapcan (lambda (r) (copy-sequence (plist-get r :findings)))
                               results))
         (exportable   (seq-filter #'hutch-eval--exportable-p all-findings))
         (comments     (mapcar #'hutch-eval--export-finding exportable)))
    `((tool            . ,tool-name)
      (pr_url          . ,pr-url)
      (review_comments . ,(vconcat comments)))))

;;; --- Batch entry point (used by the eval harness) ---

(defun hutch-eval-batch-review (repo-dir pr-url tool-name out-file max-rounds)
  "Run a hutch review on REPO-DIR for PR-URL and write JSON export to OUT-FILE.
Targets the `:branch' scope, disables caching, raises the round budget
to MAX-ROUNDS for the duration of the run.  TOOL-NAME identifies the
calling harness in the export payload.

Emits diagnostics to stderr (visible in batch mode) including backend
identity, scopes collected, per-scope status, and the full `*hutch-log*'
contents — useful for post-hoc inspection.

This is the canonical batch entry point used by the Babashka eval
harness (eval/eval.clj); it exists here so eval-side callers don't
need to inline elisp."
  (require 'hutch)
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
    (let ((results (hutch-review-sync repo-dir scopes 900)))
      (dolist (r results)
        (message "[eval] scope=%s status=%s findings=%d emsg=%S"
                 (plist-get r :scope)
                 (plist-get r :status)
                 (length (plist-get r :findings))
                 (plist-get r :emsg)))
      (with-temp-file out-file
        (insert (json-encode
                 (hutch-eval-export-results results pr-url tool-name))))
      (with-current-buffer (get-buffer-create "*hutch-log*")
        (message "[eval-log]\n%s" (buffer-string)))
      ;; Also dump *gptel-log* if it exists — has the actual API
      ;; request/response bodies, essential for diagnosing HTTP errors.
      (when-let* ((gbuf (get-buffer "*gptel-log*")))
        (with-current-buffer gbuf
          (message "[gptel-log]\n%s" (buffer-string)))))))

(provide 'hutch-eval)

;;; hutch-eval.el ends here
