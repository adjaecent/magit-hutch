;;; hutch-pr.el --- Review GitHub pull requests in a worktree -*- lexical-binding: t; -*-

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
;;
;; `hutch-review-pr' lists recent open pull requests with the `gh' CLI,
;; checks the chosen one out into its own worktree next to the repo,
;; and reviews its `:branch' scope.
;;
;; The PR head and base are fetched from the PR's repository into
;; refs/hutch/pr/<number>/{head,base}, so no remote name is assumed.
;; The base ref is stored as `branch.review/pr<number>.hutchBase',
;; which `hutch--default-branch' prefers over the default branch.

;;; Code:

(require 'let-alist)
(require 'magit)
(require 'hutch-git)
(require 'hutch-ui)

(defcustom hutch-pr-limit 10
  "Number of recent open pull requests offered by `hutch-review-pr'."
  :type 'integer
  :group 'hutch)

(defun hutch--pr-run (program &rest args)
  "Run PROGRAM with ARGS in `default-directory', which may be remote.
Return the trimmed stdout.  Signal an error with stderr on failure.
Over TRAMP gh sees a tty: it colors JSON and draws a spinner on
stderr, hence NO_COLOR and the separate stderr file."
  (let ((stderr (make-temp-file "hutch-pr"))
        (process-environment (cons "NO_COLOR=1" process-environment)))
    (unwind-protect
        (with-temp-buffer
          (unless (zerop (apply #'process-file program nil (list t stderr) nil args))
            (error "%s %s: %s" program (car args)
                   (with-temp-buffer
                     (insert-file-contents stderr)
                     (string-trim (buffer-string)))))
          (string-trim (buffer-string)))
      (delete-file stderr))))

(defun hutch--pr-read ()
  "Pick one of the recent open pull requests.
Return (NUMBER BASE URL) for the chosen pull request."
  (let* ((prs   (json-parse-string
                 (hutch--pr-run "gh" "pr" "list"
                                "--limit" (number-to-string hutch-pr-limit)
                                "--json" "number,title,author,headRefName,baseRefName,updatedAt,url")
                 :object-type 'alist :array-type 'list))
         (cands (or (mapcar (lambda (pr)
                              (cons (format "#%d %s" (alist-get 'number pr) (alist-get 'title pr))
                                    pr))
                            prs)
                    (user-error "Hutch: no open pull requests")))
         (width (+ 2 (apply #'max (mapcar (lambda (c) (string-width (car c))) cands))))
         (annotate
          (lambda (cand)
            (let-alist (cdr (assoc cand cands))
              (concat (propertize " " 'display `(space :align-to ,width))
                      (propertize (format "%-14s %s → %s  %s"
                                          .author.login .headRefName .baseRefName
                                          (substring .updatedAt 0 10))
                                  'face 'completions-annotations)))))
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      `(metadata (annotation-function . ,annotate)
                                 (display-sort-function . identity))
                    (complete-with-action action cands str pred))))
         (pr    (cdr (assoc (completing-read "Review PR: " table nil t) cands))))
    (let-alist pr (list .number .baseRefName .url))))

;;;###autoload
(defun hutch-review-pr (number base url)
  "Review pull request NUMBER against branch BASE in its own worktree.
URL is the pull request's web URL; its repository is fetched from.
The worktree is `<repo>-pr<NUMBER>' next to the repo.  An existing
worktree is reused as is, without fetching again."
  (interactive (hutch--pr-read))
  (let* ((repo   (string-remove-suffix (format "/pull/%d" number) url))
         (refs   (format "refs/hutch/pr/%d/" number))
         (branch (format "review/pr%d" number))
         (tree   (format "%s-pr%d/" (directory-file-name (magit-toplevel)) number)))
    (unless (file-directory-p tree)
      (hutch--pr-run "git" "fetch" repo
                     (format "+refs/heads/%s:%sbase" base refs)
                     (format "+refs/pull/%d/head:%shead" number refs))
      (hutch--pr-run "git" "worktree" "add" "-b" branch
                     (file-local-name tree) (concat refs "head"))
      (hutch--pr-run "git" "config" (format "branch.%s.hutchBase" branch)
                     (concat refs "base")))
    (let ((default-directory tree)
          (hutch-scopes '(:branch)))
      (magit-status-setup-buffer tree)
      (hutch-magit-review))))

(provide 'hutch-pr)

;;; hutch-pr.el ends here
