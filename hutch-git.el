;;; hutch-git.el --- Git helpers, scopes, and diff collection -*- lexical-binding: t; -*-

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
;;
;;
;; --- Scopes ---
;;
;; A scope is a plist describing one layer of diffing:
;;   :scope    -- keyword (:staged, :unpushed, :branch)
;;   :head     -- the head ref (e.g. "HEAD", or nil for staged)
;;   :base     -- the base ref (e.g. "main", "origin/feat", or nil for staged)
;;   :desc     -- human-readable description (derived)
;;   :manifest -- formatted file change summary string
;;   :hash     -- sha256 of :manifest (for caching)

;;; Code:

(require 'magit)
(require 'seq)

(defconst hutch--valid-scopes '(:staged :unpushed :branch)
  "Valid scope keywords.")

(defcustom hutch-scopes '(:staged :branch)
  "Which scopes to include in a review.
Each symbol corresponds to a diff layer:
  :staged   — changes staged for the next commit
  :unpushed — commits not yet pushed to the upstream
  :branch   — all commits on this branch vs its base"
  :type '(set (const :tag "Staged changes only" :staged)
              (const :tag "Unpushed commits for current branch" :unpushed)
              (const :tag "Unmerged changed between current branch and default branch" :branch))
  :group 'hutch)

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

(defun hutch--symbolic-ref ()
  "Return the symbolic ref for origin/HEAD."
  (magit-git-string "symbolic-ref" "refs/remotes/origin/HEAD"))

(defun hutch--merge-base (a b)
  "Return the merge-base commit between A and B."
  (magit-git-string "merge-base" a b))

(defun hutch--default-remote-ref ()
  "Return the default branch name from origin/HEAD."
  (when-let* ((ref (hutch--symbolic-ref)))
    (string-remove-prefix "refs/remotes/origin/" ref)))

(defconst hutch--default-branch-names '("main" "master")
  "Common default branch names to try, in order.")

(defun hutch--default-common-ref ()
  "Return the first branch from `hutch--default-branch-names' that exists locally."
  (seq-find #'magit-branch-p hutch--default-branch-names))

(defun hutch--default-branch ()
  "Return the default branch name.  Try origin/HEAD, then common names."
  (or (hutch--default-remote-ref)
      (hutch--default-common-ref)))

(defun hutch--git-diff-numstat (&rest args)
  "Run git diff --numstat with ARGS.  Return formatted manifest string or nil."
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
Return the diff string or nil if empty."
  (let* ((head (plist-get scope :head))
         (base (plist-get scope :base))
         (staged (and (null head) (null base)))
         (args (append (if staged
                           '("--cached")
                         (list base head))
                       (when path (list "--" path))))
         (diff (with-temp-buffer
                 (apply #'magit-git-insert "diff" args)
                 (buffer-string))))
    (and diff (not (string-empty-p diff)) diff)))

(defun hutch--patch-apply (scope patch &optional check)
  "Apply PATCH for SCOPE.  When CHECK is non-nil, dry-run (git apply --check).
For staged scope:
  - verification (CHECK non-nil) uses --cached, against the index only,
    so unrelated unstaged edits in the working tree don't fail the check;
  - real apply uses --index, applying to both index and working tree
    (which requires a clean working tree for the affected file).
For branch/unpushed scopes, applies to the working tree only.
Return (OK . OUTPUT) where OUTPUT is the stderr from git on failure."
  (let* ((head (plist-get scope :head))
         (base (plist-get scope :base))
         (staged (and (null head) (null base)))
         (normalized (if (string-suffix-p "\n" patch) patch (concat patch "\n")))
         (temp-patch (make-temp-file "hutch-patch-" nil ".diff"))
         (args (delq nil
                     (list "apply"
                           (when check  "--check")
                           (when staged (if check "--cached" "--index"))
                           "--"
                           temp-patch))))
    (with-temp-file temp-patch (insert normalized))
    (prog1
        (with-temp-buffer
          (let ((exit (apply #'process-file "git" nil (list t t) nil args)))
            (cons (zerop exit) (string-trim (buffer-string)))))
      (delete-file temp-patch))))

(defun hutch--read-scope-file (scope path)
  "Read PATH at the revision for SCOPE and return its content.
For staged scope reads the index version (`git show :PATH'); for branch
and unpushed reads at HEAD (`git show HEAD:PATH').  Returns nil if the
file does not exist at the scope's ref."
  (let* ((head   (plist-get scope :head))
         (staged (and (null head) (null (plist-get scope :base))))
         (ref    (if staged (format ":%s" path) (format "%s:%s" (or head "HEAD") path))))
    (when (magit-git-success "cat-file" "-e" ref)
      (with-temp-buffer
        (magit-git-insert "show" ref)
        (buffer-string)))))

(defun hutch--find-occurrences (haystack needle)
  "Return 1-based line numbers of each NEEDLE start in HAYSTACK.
Empty NEEDLE returns nil."
  (unless (or (null needle) (string-empty-p needle))
    (let ((pos 0) (positions '()))
      ;; Pass 1: collect 0-based positions via string-search.
      (while (setq pos (string-search needle haystack pos))
        (push pos positions)
        (setq pos (1+ pos)))
      (when positions
        (setq positions (nreverse positions))
        ;; Pass 2: walk haystack once, counting newlines.  For each
        ;; position (in sorted order), record the current line.
        (let ((line 1) (idx 0) (lines '()))
          (dolist (p positions)
            (while (< idx p)
              (when (eq (aref haystack idx) ?\n)
                (setq line (1+ line)))
              (setq idx (1+ idx)))
            (push line lines))
          (nreverse lines))))))

(defun hutch--diff-strings (file before after)
  "Generate a unified diff for FILE between BEFORE and AFTER strings.
Returns the diff text in standard `--- a/FILE` / `+++ b/FILE` form."
  (let ((before-tmp (make-temp-file "hutch-before-"))
        (after-tmp  (make-temp-file "hutch-after-")))
    (unwind-protect
        (progn
          (with-temp-file before-tmp (insert before))
          (with-temp-file after-tmp  (insert after))
          (with-temp-buffer
            (let ((exit (call-process "diff" nil t nil "-u"
                                     "--label" (format "a/%s" file)
                                     "--label" (format "b/%s" file)
                                     before-tmp after-tmp)))
              (if (>= exit 2)
                  (error "diff failed with exit code %d" exit)
                (buffer-string)))))
      (delete-file before-tmp)
      (delete-file after-tmp))))

(defun hutch--git-log (path max-count)
  "Run git log for PATH, limiting entries to MAX-COUNT.
Return the output of `git log' as a string."
  (with-temp-buffer
    (magit-git-insert "log" "--oneline" "--follow" (format "-n%s" max-count) "--" path)
    (buffer-string)))

(defun hutch--git-blame (path &optional start-line end-line)
  "Run git blame for a PATH between START-LINE and END-LINE.
Return the output of git blame"
  (with-temp-buffer
    (apply #'magit-git-insert "blame" "--porcelain"
           (append (when (and start-line end-line)
                     (list (format "-L%d,%d" start-line end-line)))
                   (list "--" path)))
    (buffer-string)))

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
  (when-let* ((current  (magit-get-current-branch))
              (upstream (magit-get-upstream-branch current))
              (diff     (hutch--git-diff-numstat upstream "HEAD")))
    (hutch--make-scope :unpushed "HEAD" upstream diff)))

(defun hutch--collect-staged ()
  "Collect the staged scope, or nil if nothing staged."
  (when-let* ((diff (hutch--git-diff-numstat "--cached")))
    (hutch--make-scope :staged nil nil diff)))

(defun hutch-collect-scopes ()
  "Collect scopes enabled by `hutch-scopes'.  Return a list of scope plists."
  (seq-filter #'identity
              (list (when (memq :branch   hutch-scopes) (hutch--collect-branch))
                    (when (memq :unpushed hutch-scopes) (hutch--collect-unpushed))
                    (when (memq :staged   hutch-scopes) (hutch--collect-staged)))))

(defun hutch--scope-hash-current (scope)
  "Recollect SCOPE and return its current :hash (or nil if scope no longer exists)."
  (let ((fresh (pcase (plist-get scope :scope)
                 (:staged   (hutch--collect-staged))
                 (:unpushed (hutch--collect-unpushed))
                 (:branch   (hutch--collect-branch)))))
    (and fresh (plist-get fresh :hash))))

(provide 'hutch-git)

;;; hutch-git.el ends here
