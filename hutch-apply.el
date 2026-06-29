;;; hutch-apply.el --- Bulk-apply harness -*- lexical-binding: t; -*-

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
;;
;; Bulk-apply harness — owns the data orchestration AND the git I/O.
;; The UI just identifies the scope at point and calls `hutch--apply-bulk'.
;;
;; Flow:
;;   1. Filter findings to :state 'queued
;;   2. Sort bottom-up per file (so earlier applies don't shift line
;;      numbers for later ones)
;;   3. For each: verify, apply, revert any visiting buffer
;;   4. Flip :state to 'applied (success) or 'invalid (failure)
;;
;; Callers should hash-check the scope (see `hutch--scope-hash-current' in
;; git.el) before invoking bulk apply, to guard against the index changing
;; between review and apply.

;;; Code:

(require 'seq)
(require 'hutch-utils)
(require 'hutch-git)
(require 'hutch-findings)

(defun hutch--apply-candidates (findings)
  "Return FINDINGS with :state `queued', sorted for bottom-up application.

Within each file, candidates are sorted by descending start line so earlier
applies don't shift line numbers for later ones.  Across files, order is
unspecified."
  (seq-sort-by
   (lambda (f)
     (cons (plist-get f :file)
           (- (car (or (hutch--parse-lines (plist-get f :lines))
                       '(0 . 0))))))
   (lambda (a b)
     (if (string= (car a) (car b))
         (< (cdr a) (cdr b))
       (string< (car a) (car b))))
   (seq-filter
    (lambda (f) (eq (plist-get f :state) 'queued))
    findings)))

(defun hutch--apply-bulk (findings scope root)
  "Apply each queued finding in FINDINGS bottom-up per file under ROOT for SCOPE.

Mutates findings in place: `queued' → `applied' on success, `queued' →
`invalid' on failure.  Returns FINDINGS for chaining.

Does not revert visiting buffers; if the user had the file open with unsaved
edits, Emacs will warn at next save."
  (let ((default-directory root))
    (dolist (f (hutch--apply-candidates findings))
      (let* ((result (hutch--patch-apply scope (plist-get f :patch)))
             (ok    (car result))
             (out   (cdr result)))
        (if ok
            (hutch--finding-state-transition f 'applied)
          (hutch--finding-state-transition f 'invalid)
          (hutch--log "apply" "invalid %s:%s — %s"
                      (plist-get f :file) (plist-get f :lines) out)))))
  findings)

(provide 'hutch-apply)

;;; hutch-apply.el ends here
