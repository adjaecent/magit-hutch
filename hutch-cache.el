;;; hutch-cache.el --- Cache review in .git -*- lexical-binding: t; -*-

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

;; Persistent review cache keyed by manifest SHA256.  Stores results
;; under the repository's gitdir so they survive Emacs restarts but
;; never leak into commits.

;;; Code:

(require 'subr-x)
(require 'magit)

(defgroup hutch nil
  "AI code review integrated with Magit."
  :group 'magit
  :prefix "hutch-")

(defcustom hutch-cache-enabled t
  "Whether to consult and update the on-disk review cache.
When non-nil (the default), successful review results are cached in
`.git/hutch-review-cache' keyed by the diff manifest hash, and reused
on subsequent reviews of unchanged diffs.

Set to nil to force fresh reviews on every invocation.  Useful for
evaluation runs, prompt iteration, or any workflow where cached
results would mask current model behavior."
  :type 'boolean
  :group 'hutch)

(defun hutch--cache-file ()
  "Return the path to the review cache file in .git/."
  (expand-file-name "hutch/cache.eld" (magit-gitdir)))

(defun hutch--cache-lookup (hash)
  "Look up HASH in the cache.  Return the result plist or nil on miss.
Returns nil when `hutch-cache-enabled' is nil."
  (when hutch-cache-enabled
    (let ((cache-file (hutch--cache-file)))
      (when (file-exists-p cache-file)
        (let ((alist (with-temp-buffer
                       (insert-file-contents cache-file)
                       (ignore-errors (read (current-buffer))))))
          (cdr (assoc hash alist)))))))

(defun hutch--cache-store (hash result &optional cache-file)
  "Store RESULT under HASH in the cache, replacing any existing entry."
  (let* ((new-entry (cons hash result))
         (cache-file (or cache-file (hutch--cache-file)))
         (alist (if (file-exists-p cache-file)
                    (with-temp-buffer
                      (insert-file-contents cache-file)
                      (ignore-errors (read (current-buffer))))
                  nil))
         (new-list (thread-last alist
                                (assoc-delete-all hash)
                                (cons new-entry))))
    (make-directory (file-name-directory cache-file) t)
    (with-temp-file cache-file (prin1 new-list (current-buffer)))))

(defun hutch--cache-evict (hash)
  "Remove the cache entry for HASH, if any."
  (let ((cache-file (hutch--cache-file)))
    (when (file-exists-p cache-file)
      (let* ((alist (with-temp-buffer
                      (insert-file-contents cache-file)
                      (ignore-errors (read (current-buffer)))))
             (pruned (assoc-delete-all hash alist)))
        (with-temp-file cache-file (prin1 pruned (current-buffer)))))))

(defun hutch--write-through-cache-callback (hash callback)
  "Return a callback that caches a successful result against HASH.
The wrapped CALLBACK is invoked after the write-through.
Returns CALLBACK unchanged when `hutch-cache-enabled' is nil."
  (if (not hutch-cache-enabled)
      callback
    (let ((cache-file (hutch--cache-file)))
      (lambda (result)
        (hutch--log "cache" "status: %s hash: %s" (plist-get result :status) hash)
        (when (eq (plist-get result :status) :ok)
          (hutch--cache-store hash result cache-file))
        (funcall callback result)))))

(provide 'hutch-cache)

;;; hutch-cache.el ends here
