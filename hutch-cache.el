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

;; One cache file per scope keyword, keyed by manifest SHA256.  Each
;; file holds a single (HASH . RESULT) cons -- the most recent review
;; for that scope.  Per-scope files sidestep any read-modify-write
;; races between concurrent scope completions.

;;; Code:

(require 'subr-x)
(require 'magit)

(defgroup hutch nil
  "AI code review integrated with Magit."
  :group 'magit
  :prefix "hutch-")

(defcustom hutch-cache-enabled t
  "Whether to consult and update the on-disk review cache.
When non-nil (the default), the most recent successful review per
scope is cached in `.git/hutch/cache-<scope>.eld' keyed by the diff
manifest hash, and reused when the same diff is reviewed again.

Set to nil to force fresh reviews on every invocation.  Useful for
evaluation runs, prompt iteration, or any workflow where cached
results would mask current model behavior."
  :type 'boolean
  :group 'hutch)

(defun hutch--cache-file (kw)
  "Return the cache file path for scope KW."
  (expand-file-name (format "hutch/cache-%s.eld" (substring (symbol-name kw) 1))
                    (magit-gitdir)))

(defun hutch--cache-read (kw)
  "Return the (HASH . RESULT) cons for scope KW, or nil if absent/unreadable."
  (let ((f (hutch--cache-file kw)))
    (when (file-exists-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (ignore-errors (read (current-buffer)))))))

(defun hutch--cache-lookup (kw hash)
  "Return the cached result for KW + HASH, or nil on miss / disabled cache."
  (when hutch-cache-enabled
    (let ((entry (hutch--cache-read kw)))
      (when (and (consp entry) (equal (car entry) hash))
        (cdr entry)))))

(defun hutch--cache-store (kw hash result)
  "Store RESULT under KW + HASH, overwriting any prior entry for KW."
  (let ((f (hutch--cache-file kw)))
    (make-directory (file-name-directory f) t)
    (with-temp-file f (prin1 (cons hash result) (current-buffer)))))

(defun hutch--cache-evict (kw hash)
  "Remove the cache entry for KW + HASH, if it is the currently stored one."
  (let ((entry (hutch--cache-read kw)))
    (when (and (consp entry) (equal (car entry) hash))
      (delete-file (hutch--cache-file kw)))))

(defun hutch--write-through-cache-callback (kw hash callback)
  "Return a callback that caches a successful result against KW + HASH.
The wrapped CALLBACK is invoked after the write-through.
Returns CALLBACK unchanged when `hutch-cache-enabled' is nil."
  (if (not hutch-cache-enabled)
      callback
    (lambda (result)
      (hutch--log "cache" "status: %s hash: %s" (plist-get result :status) hash)
      (when (eq (plist-get result :status) :ok)
        (hutch--cache-store kw hash result))
      (funcall callback result))))

(provide 'hutch-cache)

;;; hutch-cache.el ends here
