;;; magit-hutch-cache.el --- Cache review in .git -*- lexical-binding: t; -*-

;;; Code:

(defun hutch--cache-file ()
  "Return the path to the review cache file in .git/."
  (expand-file-name "hutch-review-cache" (magit-git-dir)))

(defun hutch--cache-lookup (hash)
  "Look up HASH in the cache. Return the result plist or nil on miss."
  (let ((cache-file (hutch--cache-file)))
    (when (file-exists-p cache-file)
      (let ((alist (with-temp-buffer
                      (insert-file-contents cache-file)
                      (ignore-errors (read (current-buffer))))))
        (cdr (assoc hash alist))))))

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
    (with-temp-file cache-file (prin1 new-list (current-buffer)))))

(defun hutch--write-through-cache-callback (hash callback)
  "Return a callback that caches successful results before calling CALLBACK."
  (let ((cache-file (hutch--cache-file)))
    (lambda (result)
      (hutch--log "cache" "status: %s hash: %s" (plist-get result :status) hash)
      (when (eq (plist-get result :status) :ok)
        (hutch--cache-store hash result cache-file))
      (funcall callback result))))

(provide 'magit-hutch-cache)

;;; magit-hutch-cache.el ends here
