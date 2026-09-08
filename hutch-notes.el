;;; hutch-notes.el --- Persist staged reviews under refs/hutch -*- lexical-binding: t; -*-

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

;; Promotes a successful :staged review to a durable git ref at
;; commit time, iff the committed tree's numstat manifest matches
;; what was reviewed.  Storage: one blob per review, addressed by
;; manifest hash, referenced from refs/hutch/reviews/<hash>.
;;
;; Only :staged reviews are eligible.
;;
;; The "last staged review" is remembered in-memory per gitdir for
;; the current Emacs session.  Close Emacs between review and commit
;; and nothing is promoted -- silent, no error.

;;; Code:

(require 'subr-x)
(require 'magit)
(require 'hutch-git)
(require 'hutch-utils)

(defcustom hutch-notes-enabled t
  "When non-nil, promote staged reviews to refs/hutch/reviews/<hash>.
Promotion happens after a commit finishes, iff the committed tree's
numstat manifest matches the last successful :staged review."
  :type 'boolean
  :group 'hutch)

(defvar hutch--notes-last-staged nil
  "Alist of (GITDIR . (HASH . RESULT)).
Holds the last successful :staged review this session, per repo.
Consumed by the post-commit promotion hook.")

;;; --- Ref plumbing ---

(defun hutch--notes-ref (hash)
  "Return the ref name for HASH."
  (format "refs/hutch/reviews/%s" hash))

(defun hutch--notes-write-blob (result)
  "Write RESULT (a plist) as a git blob.
Return the blob OID string, or nil on failure."
  (let ((payload (prin1-to-string result)))
    (with-temp-buffer
      (let ((exit (apply #'call-process-region
                         payload nil
                         magit-git-executable
                         nil t nil
                         (append magit-git-global-arguments
                                 '("hash-object" "-w" "--stdin")))))
        (when (zerop exit)
          (string-trim (buffer-string)))))))

(defun hutch--notes-store (hash result)
  "Store RESULT under the namespaced ref for HASH.  Return t on success."
  (when-let* ((oid (hutch--notes-write-blob result)))
    (magit-git-success "update-ref" (hutch--notes-ref hash) oid)))

;;; --- Session state ---

(defun hutch--notes-remember-staged (scope result)
  "If SCOPE is a successful :staged review, record it for later promotion."
  (when (and hutch-notes-enabled
             (eq (plist-get scope :scope) :staged)
             (eq (plist-get result :status) :ok))
    (let ((gitdir (magit-gitdir))
          (hash   (plist-get scope :hash)))
      (setf (alist-get gitdir hutch--notes-last-staged nil nil #'equal)
            (cons hash result))
      (hutch--log "notes" "remembered staged review %s for %s" hash gitdir))))

(defun hutch--notes-forget-staged ()
  "Drop the remembered :staged review for the current repo, if any."
  (when-let* ((gitdir (magit-gitdir)))
    (setq hutch--notes-last-staged
          (assoc-delete-all gitdir hutch--notes-last-staged #'equal))))

(defun hutch--notes-remember-callback (scope callback)
  "Return a callback that remembers a successful :staged review.
The wrapped CALLBACK is invoked after the remember step.
For non-staged scopes, returns CALLBACK unchanged."
  (if (not (eq (plist-get scope :scope) :staged))
      callback
    (lambda (result)
      (hutch--notes-remember-staged scope result)
      (funcall callback result))))

;;; --- Post-commit promotion ---

(defun hutch--notes-empty-tree-oid ()
  "Return the empty-tree OID in this repo's native object format.
`git mktree' with empty stdin resolves it in whichever hash algorithm
the repository uses (SHA-1 or SHA-256) and creates the object if missing."
  (with-temp-buffer
    (let ((exit (apply #'call-process-region
                       "" nil magit-git-executable
                       nil t nil
                       (append magit-git-global-arguments '("mktree")))))
      (when (zerop exit)
        (string-trim (buffer-string))))))

(defun hutch--notes-commit-hash ()
  "Return the content hash for HEAD's just-created commit, or nil on failure.
Uses the same `hutch--diff-hash' primitive as scope creation so that
review-time and commit-time hashes agree on identical content.  For
initial commits (no HEAD~1) diffs against this repo's empty tree."
  (let ((base (if (magit-git-success "rev-parse" "--verify" "--quiet" "HEAD~1")
                  "HEAD~1"
                (hutch--notes-empty-tree-oid))))
    (and base (hutch--diff-hash base "HEAD"))))

(defun hutch--notes-consume-last-staged (gitdir)
  "Return and clear the last-staged entry for GITDIR."
  (let ((entry (alist-get gitdir hutch--notes-last-staged nil nil #'equal)))
    (setq hutch--notes-last-staged
          (assoc-delete-all gitdir hutch--notes-last-staged #'equal))
    entry))

(defun hutch--notes-post-commit-promote ()
  "Promote the last :staged review to a ref iff its hash matches HEAD's tree."
  (when hutch-notes-enabled
    (let* ((gitdir (magit-gitdir))
           (entry  (and gitdir (hutch--notes-consume-last-staged gitdir))))
      (when entry
        (let ((review-hash (car entry))
              (result      (cdr entry))
              (commit-hash (hutch--notes-commit-hash)))
          (cond
           ((null commit-hash)
            (hutch--log "notes" "could not compute commit manifest; not preserved"))
           ((equal review-hash commit-hash)
            (if (hutch--notes-store commit-hash result)
                (hutch--log "notes" "promoted staged review under %s"
                            (hutch--notes-ref commit-hash))
              (message "hutch: failed to write review ref for %s" commit-hash)))
           (t
            (message "hutch: staged review didn't match commit contents; not preserved"))))))))

(provide 'hutch-notes)

;;; hutch-notes.el ends here
