;;; hutch-ui.el --- Magit-section UI for hutch reviews -*- lexical-binding: t; -*-

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

;; Renders hutch review results as magit sections with
;; accept/reject actions for suggestions.

;;; Code:

(require 'cl-lib)
(require 'hutch-agent)
(require 'hutch-apply)
(require 'hutch-instrument)
(require 'magit-section)
(require 'svg-lib)

;;; --- Mode and keymaps ---

(defvar-keymap hutch-mode-map
  :parent   magit-section-mode-map
  "q"       #'quit-window
  "A"       #'hutch-bulk-apply
  "D"       #'hutch-discard-review
  "C-c C-k" #'hutch-cancel-review)

(define-derived-mode hutch-mode magit-section-mode "Hutch"
  "Major mode for displaying hutch code review results."
  :interactive nil)

(defvar-local hutch--scopes nil "Scopes being reviewed.")
(defvar-local hutch--results nil "Alist of (LABEL . RESULT).")
(defvar-local hutch--cancel-fns nil "List of cancel functions for in-progress reviews.")
(defvar-local hutch--progress nil "Alist of (SCOPE-LABEL . (CURRENT . MAX)) for in-flight scopes.")
(defvar-local hutch--discarded nil "When non-nil, review is discarded — all commands refuse.")

;; Magit's auto-lookup for section keymaps uses `magit-TYPE-section-map',
;; which would force a `magit-' prefix we cannot adopt.  Instead we name
;; these with the package prefix and attach them explicitly via the
;; section's `keymap' slot inside the inserters below.
(defvar-keymap hutch-suggestion-section-map
  :parent magit-section-mode-map
  "m" #'hutch-suggestion-queue
  "x" #'hutch-suggestion-reject)

(defvar-keymap hutch-comment-section-map
  :parent magit-section-mode-map
  "x" #'hutch-suggestion-reject)

;;; --- Display helpers ---

(defun hutch--badge (label face)
  "Return an SVG badge image for LABEL styled with FACE foreground."
  (svg-lib-tag label
               nil
               :padding 1
               :radius 2
               :font-size 14
               :foreground (face-foreground face nil t)
               :background (face-background face nil t)))

(defun hutch--badge-image (label face)
  "Return a propertized string displaying an SVG badge for LABEL using FACE.
Prefix with an invisible zero-width character so it interacts cleanly
with transient hiding."
  (concat "\u200b" (propertize " " 'display (hutch--badge label face))))

(defun hutch--diff-line-face (line)
  "Return the face for a diff LINE based on its prefix."
  (cond
   ((string-prefix-p "+" line)  'magit-diff-added)
   ((string-prefix-p "-" line)  'magit-diff-removed)
   ((string-prefix-p "@@" line) 'magit-diff-hunk-heading)
   (t 'default)))

(defun hutch--diff-header-p (line)
  "Return t if LINE is a diff file header (--- or +++)."
  (or (string-prefix-p "---" line)
      (string-prefix-p "+++" line)))

(defun hutch--insert-patch-lines (patch)
  "Insert colorized diff lines from PATCH string, skipping file headers."
  (insert "\n")
  (dolist (line (split-string patch "\n"))
    (unless (hutch--diff-header-p line)
      (let ((face (hutch--diff-line-face line))
            (beg  (point)))
        (insert (format "    %s\n" line))
        (unless (eq face 'default)
          (let ((ov (make-overlay beg (point))))
            (overlay-put ov 'face face)
            (overlay-put ov 'priority 10))))))
  (insert "\n"))

(defun hutch--insert-desc (desc)
  "Insert DESC wrapped to 80 chars with 2-space indent."
  (insert (hutch--wrap-text desc 80 "  ") "\n"))

(defface hutch-finding-applied
  '((t :strike-through t))
  "Face for applied suggestion titles.")

(defface hutch-finding-dismissed
  '((t :foreground "#666" :slant italic))
  "Face for dismissed finding titles.")

(defface hutch-finding-queued
  '((t :weight bold :foreground "#e5c07b"))
  "Face for queued finding titles (marked for bulk apply).")

(defface hutch-finding-invalid
  '((t :strike-through t :foreground "#e06c75"))
  "Face for invalid finding titles (apply failed).")

(defun hutch--state-face (state)
  "Return the face symbol for finding STATE, or nil for unknown states."
  (pcase state
    ('applied   'hutch-finding-applied)
    ('dismissed 'hutch-finding-dismissed)
    ('queued    'hutch-finding-queued)
    ('invalid   'hutch-finding-invalid)))

;;; --- Section inserters ---

(defconst hutch--finding-locator-format "%s:%s"
  "Format string for the file:lines locator on each finding.")

(defconst hutch-badge-labels
  '(suggestion "SUGGESTION" comment "COMMENT" lgtm "LGTM"))

(defface hutch-badge-suggestion
  '((t :foreground "#ffffff" :background "#e06c75"))
  "Face for suggestion badges.")

(defface hutch-badge-comment
  '((t :foreground "#ffffff" :background "#61afef"))
  "Face for comment badges.")

(defface hutch-badge-lgtm
  '((t :foreground "#ffffff" :background "#98c379"))
  "Face for lgtm badges.")

(defun hutch--longest-finding-locator (findings)
  "Return the width of the longest locator + badge label across FINDINGS."
  (seq-max
   (mapcar (lambda (f)
             (let* ((file         (plist-get f :file))
                    (lines        (plist-get f :lines))
                    (type         (plist-get f :type))
                    (label        (plist-get hutch-badge-labels type))
                    (label-length (length label)))
               (if (and file lines)
                   (+  (length (format hutch--finding-locator-format file lines))
                       label-length)
                 0)))
           findings)))

(defun hutch--insert-lgtm (finding)
  "Insert a LGTM section for FINDING."
  (magit-insert-section (review-lgtm (plist-get finding :file))
    (magit-insert-heading
      (hutch--badge-image (plist-get hutch-badge-labels 'lgtm) 'hutch-badge-lgtm)
      (propertize (format " %s" (plist-get finding :file))
                  'font-lock-face 'magit-diff-file-heading))))

(defun hutch--insert-suggestion (finding longest-width)
  "Insert a FINDING suggestion section (has a patch).
LONGEST-WIDTH is the column at which the title should be aligned."
  (let* ((state (plist-get finding :state))
         (badge-label (plist-get hutch-badge-labels 'suggestion))
         (badge-label (if (eq state 'queued) (concat badge-label " / " "Queued") badge-label))
         (width (- longest-width (length badge-label))))
    (magit-insert-section sec (review-suggestion finding t)
      (oset sec keymap 'hutch-suggestion-section-map)
      (magit-insert-heading
        (hutch--badge-image badge-label 'hutch-badge-suggestion)
        (propertize (format (format " %%-%ds    %%s" width)
                            (format hutch--finding-locator-format
                                    (plist-get finding :file)
                                    (plist-get finding :lines))
                            (plist-get finding :title))
                    'font-lock-face (delq nil (list 'magit-diff-file-heading
                                                    (hutch--state-face state)))))
      (hutch--insert-desc (plist-get finding :desc))
      (hutch--insert-patch-lines (plist-get finding :patch)))))

(defun hutch--insert-comment (finding longest-width)
  "Insert a FINDING:comment section (no patch).  Press `x' to dismiss.
LONGEST-WIDTH is the column at which the title should be aligned."
  (let* ((badge-label (plist-get hutch-badge-labels 'comment))
         (width (- longest-width (length badge-label))))
    (magit-insert-section sec (review-comment finding t)
      (oset sec keymap 'hutch-comment-section-map)
      (magit-insert-heading
        (hutch--badge-image badge-label 'hutch-badge-comment)
        (propertize (format (format " %%-%ds    %%s" width)
                            (format hutch--finding-locator-format
                                    (plist-get finding :file)
                                    (plist-get finding :lines))
                            (plist-get finding :title))
                    'font-lock-face 'magit-diff-file-heading))
      (hutch--insert-desc (plist-get finding :desc)))))

(defun hutch--insert-finding (finding longest-width)
  "Insert FINDING based on its :type.
LONGEST-WIDTH is the column at which the title should be aligned."
  (pcase (plist-get finding :type)
    ('lgtm       (hutch--insert-lgtm finding))
    ('suggestion (hutch--insert-suggestion finding longest-width))
    ('comment    (hutch--insert-comment finding longest-width))))

(defun hutch--insert-findings (findings display-label section-label result)
  "Insert FINDINGS under DISPLAY-LABEL heading, keyed by SECTION-LABEL.
RESULT is the per-scope plist; used to surface token counts in the header."
  (magit-insert-section (review-group section-label)
    (magit-insert-heading
      (propertize display-label 'font-lock-face 'magit-section-heading)
      " "
      (or (when-let* ((tokens (hutch--format-tokens result)))
            (hutch--badge-image tokens 'hutch-badge-tokens))
          ""))
    (if (null findings)
        (insert "No issues found.")
      (let ((longest-width (hutch--longest-finding-locator findings)))
        (dolist (finding findings)
          (hutch--insert-finding finding longest-width))))
    (insert "\n")))

;;; --- Queue / dismiss ---

(defun hutch-suggestion-reject ()
  "Dismiss the suggestion at point.
Only pending findings can be dismissed; once queued, the only exits
are `applied' or `invalid' via `hutch-bulk-apply'."
  (interactive)
  (hutch--ensure-active)
  (when-let* ((section (magit-current-section))
              (value (oref section value))
              (state (plist-get value :state)))
    (if (not (eq state 'pending))
        (message "Already %s; cannot dismiss" state)
      (hutch--finding-state-transition value 'dismissed)
      (hutch--render-buffer (current-buffer))
      (message "Suggestion dismissed."))))

(defun hutch-suggestion-queue ()
  "Toggle the queued state of the suggestion at point.
Pending ↔ queued.  Bulk apply (A) consumes queued findings."
  (interactive)
  (hutch--ensure-active)
  (when-let* ((section (magit-current-section))
              (value (oref section value))
              (state (plist-get value :state)))
    (pcase state
      ('pending
       (hutch--finding-state-transition value 'queued)
       (hutch--render-buffer (current-buffer))
       (message "Queued."))
      ('queued
       (hutch--finding-state-transition value 'pending)
       (hutch--render-buffer (current-buffer))
       (message "Unqueued."))
      (_ (message "Already %s; cannot queue" state)))))

;;; --- Bulk apply ---

(defun hutch--scope-at-point ()
  "Return the scope plist for the review-group section enclosing point."
  (let ((section (magit-current-section)))
    (while (and section (not (eq (oref section type) 'review-group)))
      (setq section (oref section parent)))
    (when section
      (let ((label (oref section value)))
        (seq-find (lambda (s) (string= (hutch--scope-label s) label))
                  hutch--scopes)))))

(defun hutch-bulk-apply ()
  "Apply all queued findings in the scope at point.

Aborts if the scope's index has changed since the review was generated."
  (interactive)
  (hutch--ensure-active)
  (let* ((scope    (hutch--scope-at-point))
         (label    (and scope (hutch--scope-label scope)))
         (result   (and label (cdr (assoc label hutch--results #'equal))))
         (findings (and result (plist-get result :findings))))
    (cond
     ((null scope)
      (message "No review scope at point"))
     ((not findings)
      (message "No findings in this scope"))
     ((not (equal (plist-get result :hash)
                  (hutch--scope-hash-current scope)))
      (message "Staged content changed since review — please re-run hutch"))
     ((not (seq-some (lambda (f) (eq (plist-get f :state) 'queued)) findings))
      (message "Nothing queued; mark suggestions with `m' first"))
     (t
      (hutch--apply-bulk findings scope (magit-toplevel))
      ;; Our own applications legitimately change the index, so the next bulk-apply
      ;; would trip the hash guard.  Refresh to the post-apply hash so the
      ;; guard still detects EXTERNAL changes, not changes we just made.
      (when-let* ((new-hash (hutch--scope-hash-current scope)))
        (plist-put result :hash new-hash))
      (hutch--render-buffer (current-buffer))
      (let ((applied (seq-count (lambda (f) (eq (plist-get f :state) 'applied))
                                findings))
            (invalid (seq-count (lambda (f) (eq (plist-get f :state) 'invalid))
                                findings)))
        (message "Bulk apply: %d applied, %d invalid" applied invalid))))))

;;; --- Buffer and entry point ---

(defun hutch--ensure-active ()
  "Signal an error if the review is discarded.
Call from any interactive command that mutates state."
  (when hutch--discarded
    (user-error "Review is discarded — no further actions allowed")))

(defun hutch-discard-review ()
  "Mark this review as discarded.  Locks all further interaction.
Evicts any cached results for this review's scopes so a fresh review
runs next time."
  (interactive)
  (cond
   (hutch--discarded (message "Already discarded."))
   ((y-or-n-p "Discard this review (findings will be frozen for reference)? ")
    (setq hutch--discarded t)
    (dolist (scope hutch--scopes)
      (hutch--cache-evict (plist-get scope :hash)))
    (hutch--render-buffer (current-buffer))
    (message "Review discarded."))))

(defface hutch-badge-tokens
  '((t :foreground "#888888" :background "#2d2d2d"))
  "Face for token count badges.")

(defun hutch-cancel-review ()
  "Cancel all in-progress reviews."
  (interactive)
  (mapc #'funcall hutch--cancel-fns)
  (hutch--trace-end)
  (setq hutch--cancel-fns nil)
  (dolist (scope hutch--scopes)
    (let ((label (hutch--scope-label scope)))
      (unless (assoc label hutch--results #'equal)
        (push (cons label (list :status :cancelled
                                :scope  (plist-get scope :scope)
                                :desc   (plist-get scope :desc)))
              hutch--results))))
  (hutch--render-buffer (current-buffer))
  (message "Hutch: review(s) cancelled."))

(defun hutch--scope-label (scope)
  "Return a stable key for SCOPE used in result lookups."
  (format "%s %s" (plist-get scope :scope) (plist-get scope :desc)))

(defun hutch--format-tokens (result)
  "Format token couwhnts from RESULT as \"R1.2k/W3.4k\" or \"W3.4k\" or nil."
  (let ((in  (plist-get result :input-tokens))
        (out (plist-get result :output-tokens)))
    (cond
     ((and in out) (format "R%.1fk/W%.1fk" (/ in 1000.0) (/ out 1000.0)))
     (out          (format "W%.1fk" (/ out 1000.0)))
     (in           (format "R%.1fk" (/ in 1000.0))))))

(defun hutch--scope-name (scope)
  "Return the human-readable name for SCOPE."
  (let ((key (plist-get scope :scope))
        (desc (plist-get scope :desc)))
    (pcase key
      (:staged   "staged changes")
      (:unpushed "unpushed commits")
      (:branch   (format "branch %s" desc))
      (_         (format "%s %s" key desc)))))

(defun hutch--scope-emoji (scope)
  "Return the emoji for SCOPE."
  (pcase (plist-get scope :scope)
    (:staged   "📦")
    (:unpushed "⬆️")
    (:branch   "🌿")
    (_         "📋")))

(defun hutch--scope-display-label (scope &optional result)
  "Return a display heading for SCOPE with stats from RESULT."
  (let* ((findings (plist-get result :findings))
         (n (when findings (length findings))))
    (concat (hutch--scope-emoji scope)
            " "
            (hutch--scope-name scope)
            (when n (format " (%d)" n)))))

(defun hutch--setup-buffer ()
  "Create and prepare the review buffer.  Return it."
  (let ((buf (get-buffer-create "*magit-hutch: code review*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (hutch-mode))
    buf))

(defun hutch--render-buffer (buf)
  "Re-render BUF from `hutch--scopes' and `hutch--results'."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq magit-root-section nil)
      (magit-insert-section (review-root)
        (magit-insert-heading
          (propertize "magit-hutch: code review"
                      'font-lock-face 'magit-section-heading)
          (when hutch--discarded
            (propertize " [DISCARDED]" 'font-lock-face 'hutch-finding-invalid)))
        (dolist (scope hutch--scopes)
          (let* ((label (hutch--scope-label scope))
                 (entry (assoc label hutch--results #'equal))
                 (result (cdr entry))
                 (findings (plist-get result :findings))
                 (display (hutch--scope-display-label scope result)))
            (cond
             ((null result)
              (insert (format "%s Reviewing %s ⏳ "
                              (hutch--scope-emoji scope)
                              (hutch--scope-name scope)))
              (hutch--insert-progress label)
              (insert "\n"))
             ((eq (plist-get result :status) :cancelled)
              (insert (format "%s Reviewing %s — cancelled ❌.\n\n"
                              (hutch--scope-emoji scope)
                              (hutch--scope-name scope))))
             ((eq (plist-get result :status) :error)
              (insert (format "%s Reviewing %s — failed: %s\n\n"
                              (hutch--scope-emoji scope)
                              (hutch--scope-name scope)
                              (plist-get result :emsg))))
             (t
              (hutch--insert-findings findings display label result)))))))))

(defun hutch--render-result (buf result)
  "Store RESULT and re-render BUF."
  (let ((label (hutch--scope-label result)))
    (with-current-buffer buf
      (push (cons label result) hutch--results))
    (hutch--render-buffer buf)))

(defun hutch--insert-progress (label)
  "Insert the in-progress indicator for scope LABEL at point.
Renders an svg progress bar when a progress entry exists, falling back
to \"...\" before the first tick arrives."
  (when-let* ((p (cdr (assoc label hutch--progress #'equal))))
    (insert (propertize " " 'display
                        (svg-lib-progress-bar
                         (min 1.0 (/ (float (car p)) (cdr p))))))))

(defun hutch--render-progress (buf scope current max)
  "Record progress for SCOPE in BUF and re-render."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setf (alist-get (hutch--scope-label scope)
                       hutch--progress
                       nil
                       nil
                       #'equal)
            (cons current max))
      (hutch--render-buffer buf))))

(defun hutch--render-remove-progress (buf label)
  "Drop the progress entry for LABEL in BUF.  Caller re-renders."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq hutch--progress
            (assoc-delete-all label hutch--progress)))))

;;;###autoload
(defun hutch-magit-review ()
  "Review change with AI.  Show sections for branch, unpushed, and staged diffs."
  (interactive)
  (hutch--trace-begin)
  (let* ((scopes (hutch-collect-scopes))
         (remaining (length scopes)))
    (if (null scopes)
        (progn
          (message "Hutch: no changes to review")
          (hutch--trace-end))
      (let ((buf (hutch--setup-buffer)))
        (with-current-buffer buf
          (setq hutch--scopes scopes
                hutch--results nil))
        (hutch--render-buffer buf)
        (pop-to-buffer buf)
        (setq hutch--cancel-fns
              (hutch--review scopes
                             (lambda (result)
                               (hutch--render-remove-progress buf (hutch--scope-label result))
                               (hutch--render-result buf result)
                               (when (zerop (cl-decf remaining)) (hutch--trace-end)))
                             (lambda (scope current max)
                               (hutch--render-progress buf scope current max))))))))

(provide 'hutch-ui)

;;; hutch-ui.el ends here
