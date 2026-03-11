;;; magit-hutch-git.el --- Git helpers, scopes, and diff collection -*- lexical-binding: t; -*-

;;; Code:

(require 'magit)
(require 'seq)

;;; --- Scopes ---
;;
;; A scope is a plist describing one layer of diffing:
;;   :scope  -- keyword (:staged, :unpushed, :branch)
;;   :head   -- the head ref (e.g. "HEAD", or nil for staged)
;;   :base   -- the base ref (e.g. "main", "origin/feat", or nil for staged)
;;   :desc   -- human-readable description (derived)
;;   :manifest -- formatted file change summary string
;;   :hash     -- sha256 of :manifest (for caching)

(defconst hutch--valid-scopes '(:staged :unpushed :branch)
  "Valid scope keywords.")

(defun hutch--make-scope (scope head base manifest)
  "Create a scope plist for SCOPE with HEAD ref, BASE ref, and MANIFEST string.
SCOPE must be one of `hutch--valid-scopes'."
  (unless (memq scope hutch--valid-scopes)
    (error "Invalid scope %s, must be one of %s" scope hutch--valid-scopes))
  (list :scope    scope
        :head     head
        :base     base
        :manifest manifest
        :hash     (secure-hash 'sha256 manifest)
        :desc     (if (and head base) (format "%s..%s" base head) "")))

;;; --- Helpers ---

(defun hutch--symbolic-ref ()
  "Return the symbolic ref for origin/HEAD."
  (magit-git-string "symbolic-ref" "refs/remotes/origin/HEAD"))

(defun hutch--merge-base (a b)
  "Return the merge-base commit between A and B."
  (magit-git-string "merge-base" a b))

(defun hutch--default-remote-ref ()
  "Return the default branch name from origin/HEAD."
  (when-let ((ref (hutch--symbolic-ref)))
    (string-remove-prefix "refs/remotes/origin/" ref)))

(defconst hutch--default-branch-names '("main" "master")
  "Common default branch names to try, in order.")

(defun hutch--default-common-ref ()
  "Return the first branch from `hutch--default-branch-names' that exists locally."
  (seq-find #'magit-branch-p hutch--default-branch-names))

(defun hutch--default-branch ()
  "Return the default branch name. Tries origin/HEAD, then common names."
  (or (hutch--default-remote-ref)
      (hutch--default-common-ref)))

;;; --- Diff collection ---

(defun hutch--git-diff-numstat (&rest args)
  "Run git diff --numstat with ARGS. Return formatted manifest string or nil."
  (let* ((output (with-temp-buffer
                   (apply #'magit-git-insert "diff" "--numstat" args)
                   (buffer-string)))
         (lines (split-string output "\n" t)))
    (when lines
      (mapconcat (lambda (line)
                   (let ((parts (split-string line "\t")))
                     (if (>= (length parts) 3)
                         (format "%s (+%s -%s)"
                                 (nth 2 parts) (nth 0 parts) (nth 1 parts))
                       "")))
                 lines "\n"))))

(defun hutch--git-diff (scope &optional path)
  "Run git diff for SCOPE, optionally limited to PATH.
Returns the diff string or nil if empty."
  (let* ((head (plist-get scope :head))
         (base (plist-get scope :base))
         (args (append (if (and (null head) (null base))
                           '("--cached")
                         (list base head))
                       (when path (list "--" path))))
         (diff (with-temp-buffer
                 (apply #'magit-git-insert "diff" args)
                 (buffer-string))))
    (and diff (not (string-empty-p diff)) diff)))

(defun hutch--collect-branch ()
  "Collect the branch scope, or nil if on default branch."
  (let ((current (magit-get-current-branch))
        (default (hutch--default-branch)))
    (when (and current default (not (string= current default)))
      (let* ((base (or (hutch--merge-base default current) default))
             (diff (hutch--git-diff-numstat base "HEAD")))
        (when diff
          (hutch--make-scope :branch "HEAD" base diff))))))

(defun hutch--collect-unpushed ()
  "Collect the unpushed scope, or nil if no upstream or no diff."
  (when-let ((current (magit-get-current-branch)))
    (when-let ((upstream (magit-get-upstream-branch current)))
      (when-let ((diff (hutch--git-diff-numstat upstream "HEAD")))
        (hutch--make-scope :unpushed "HEAD" upstream diff)))))

(defun hutch--collect-staged ()
  "Collect the staged scope, or nil if nothing staged."
  (when-let ((diff (hutch--git-diff-numstat "--cached")))
    (hutch--make-scope :staged nil nil diff)))

(defun hutch-collect-scopes ()
  "Collect all available scopes. Returns a list of scope plists."
  (seq-filter #'identity
              (list (hutch--collect-branch)
                    (hutch--collect-unpushed)
                    (hutch--collect-staged))))

(provide 'magit-hutch-git)

;;; magit-hutch-git.el ends here
