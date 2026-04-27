;;; magit-hutch-ui.el --- Magit-section UI for hutch reviews -*- lexical-binding: t; -*-

;;; Commentary:

;; Renders hutch review results as magit sections with
;; accept/reject actions for suggestions.

;;; Code:

(require 'magit-hutch-agent)
(require 'magit-section)
(require 'svg-lib)

;;; --- Mode and keymaps ---

(defvar-keymap hutch-mode-map
  :parent   magit-section-mode-map
  "q"       #'quit-window
  "C-c C-k" #'hutch-cancel-review)

(define-derived-mode hutch-mode magit-section-mode "Hutch"
  "Major mode for displaying hutch code review results."
  :interactive nil)

;; Magit looks up keymaps as `magit-TYPE-section-map' for section type TYPE
(defvar-keymap magit-review-suggestion-section-map
  :parent magit-section-mode-map
  "a" #'hutch-suggestion-accept
  "x" #'hutch-suggestion-reject)

(defvar-keymap magit-review-comment-section-map
  :parent magit-section-mode-map
  "x" #'hutch-suggestion-reject)

;;; --- Display helpers ---

(defun hutch--badge (label face)
  "Return an SVG badge image for LABEL styled with FACE foreground."
  (svg-lib-tag label nil
                 :padding 1
                 :radius 2
                 :font-size 14
                 :foreground (face-foreground face nil t)
                 :background (face-background face nil t)))

(defun hutch--badge-image (label face)
  "Return a propertized space displaying an SVG badge for LABEL."
  (propertize " " 'display (hutch--badge label face)))

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
      (insert (format "    %s\n"
                      (propertize line 'font-lock-face
                                  (hutch--diff-line-face line))))))
  (insert "\n"))

(defun hutch--insert-desc (desc)
  "Insert DESC wrapped to 80 chars with 4-space indent."
  (insert (hutch--wrap-text desc 80 "    ") "\n"))

;;; --- Section inserters ---

(defface hutch-badge-suggestion
  '((t :foreground "#ffffff" :background "#e06c75"))
  "Face for suggestion badges.")

(defface hutch-badge-comment
  '((t :foreground "#ffffff" :background "#61afef"))
  "Face for comment badges.")

(defface hutch-badge-lgtm
  '((t :foreground "#ffffff" :background "#98c379"))
  "Face for lgtm badges.")

(defun hutch--insert-lgtm (finding)
  "Insert a LGTM section for FINDING."
  (magit-insert-section (review-lgtm (plist-get finding :file))
    (magit-insert-heading
      (concat (hutch--badge-image "LGTM" 'hutch-badge-lgtm)
              (propertize (format " %s" (plist-get finding :file))
                          'font-lock-face 'magit-diff-file-heading)))))

(defun hutch--insert-suggestion (finding)
  "Insert a FINDING suggestion section (has a patch)."
  (magit-insert-section (review-suggestion finding t)
    (magit-insert-heading
      (concat (hutch--badge-image "SUGGESTION" 'hutch-badge-suggestion)
              (propertize (format " %s:%s -- %s"
                                  (plist-get finding :file)
                                  (plist-get finding :lines)
                                  (plist-get finding :title))
                          'font-lock-face 'magit-diff-file-heading)))
    (hutch--insert-desc (plist-get finding :desc))
    (hutch--insert-patch-lines (plist-get finding :patch))))

(defun hutch--insert-comment (finding)
  "Insert a FINDING:comment section (no patch).  Press `x' to dismiss."
  (magit-insert-section (review-comment finding t)
    (magit-insert-heading
      (concat (hutch--badge-image "COMMENT" 'hutch-badge-comment)
              (propertize (format " %s:%s -- %s"
                                  (plist-get finding :file)
                                  (plist-get finding :lines)
                                  (plist-get finding :title))
                          'font-lock-face 'magit-diff-file-heading)))
    (hutch--insert-desc (plist-get finding :desc))))

(defun hutch--insert-finding (finding)
  "Insert FINDING based on its :type."
  (pcase (plist-get finding :type)
    ('lgtm       (hutch--insert-lgtm finding))
    ('suggestion (hutch--insert-suggestion finding))
    ('comment    (hutch--insert-comment finding))))

(defun hutch--insert-findings (findings display-label section-label result)
  "Insert FINDINGS under DISPLAY-LABEL heading, keyed by SECTION-LABEL."
  (magit-insert-section (review-group section-label)
    (magit-insert-heading
      (propertize display-label 'font-lock-face 'magit-section-heading)
      " "
      (or (when-let ((tokens (hutch--format-tokens result)))
            (hutch--badge-image tokens 'hutch-badge-tokens))
          ""))
    (if (null findings)
        (insert "No issues found.")
      (dolist (finding findings)
        (hutch--insert-finding finding)))
    (insert "\n")))

;;; --- Accept / reject ---

(defun hutch--git-apply-check (patch directory)
  "Dry-run PATCH with git apply --check in DIRECTORY.  Return t if clean."
  (let ((default-directory directory))
    (zerop (with-temp-buffer
             (insert patch "\n")
             (call-process-region (point-min) (point-max)
                                  "git" nil t nil
                                  "apply" "--check --cached" "-")))))

(defun hutch--git-apply (patch directory)
  "Apply PATCH with git apply in DIRECTORY."
  (let ((default-directory directory))
    (with-temp-buffer
      (insert patch "\n")
      (call-process-region (point-min) (point-max)
                           "git" nil t nil
                           "apply" "--index" "-"))))

(defun hutch--revert-file-buffer (file directory)
  "Revert the buffer visiting FILE under DIRECTORY, if any."
  (when-let* ((full-path (expand-file-name file directory))
              (buf (find-buffer-visiting full-path)))
    (with-current-buffer buf
      (revert-buffer t t t))))

(defun hutch--hide-section (section buf)
  "Collapse SECTION in BUF."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (magit-section-hide section))))

(defun hutch-suggestion-accept ()
  "Accept the suggestion at point -- apply the patch via git apply."
  (interactive)
  (when-let* ((section (magit-current-section))
              (value (oref section value))
              (patch (plist-get value :patch))
              (file (plist-get value :file))
              (lines-str (plist-get value :lines))
              (root (magit-toplevel)))
    (cond
     ((not (hutch--git-apply-check patch root))
      (message "Patch does not apply cleanly to %s -- file may have changed" file))
     ((y-or-n-p (format "Apply fix to %s:%s? " file lines-str))
      (hutch--git-apply patch root)
      (hutch--revert-file-buffer file root)
      (hutch--hide-section section (current-buffer))
      (message "Applied fix to %s:%s" file lines-str)))))

(defun hutch-suggestion-reject ()
  "Reject the suggestion at point and collapse it."
  (interactive)
  (when-let* ((section (magit-current-section)))
    (hutch--hide-section section (current-buffer))
    (message "Suggestion dismissed.")))

;;; --- Buffer and entry point ---

(defvar-local hutch--scopes nil "Scopes being reviewed.")
(defvar-local hutch--results nil "Alist of (LABEL . RESULT).")
(defvar-local hutch--cancel-fns nil "List of cancel functions for in-progress reviews.")

(defface hutch-badge-tokens
  '((t :foreground "#888888" :background "#2d2d2d"))
  "Face for token count badges.")

(defun hutch-cancel-review ()
  "Cancel all in-progress reviews."
  (interactive)
  (mapc #'funcall hutch--cancel-fns)
  (setq hutch--cancel-fns nil)
  (message "Hutch: review(s) cancelled."))

(defun hutch--scope-label (scope)
  "Return a stable key for SCOPE used in result lookups."
  (format "%s %s" (plist-get scope :scope) (plist-get scope :desc)))

(defun hutch--format-tokens (result)
  "Format token counts from RESULT as \"R12k/W3k\" or nil if unavailable."
  (let ((in  (plist-get result :input-tokens))
        (out (plist-get result :output-tokens)))
    (when (or in out)
      (format "R%dk / W%dk"
              (/ (or in 0) 1000)
              (/ (or out 0) 1000)))))

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
      (magit-insert-section (review-root)
        (magit-insert-heading
          (propertize "magit-hutch: code review"
                      'font-lock-face 'magit-section-heading))
        (dolist (scope hutch--scopes)
          (let* ((label (hutch--scope-label scope))
                 (entry (assoc label hutch--results #'equal))
                 (result (cdr entry))
                 (findings (plist-get result :findings))
                 (display (hutch--scope-display-label scope result)))
            (cond
             ((null result)
              (insert (format "%s Reviewing %s...\n"
                              (hutch--scope-emoji scope)
                              (hutch--scope-name scope))))
             ((eq (plist-get result :status) :error)
              (insert (format "%s Reviewing %s — failed: %s\n\n"
                              (hutch--scope-emoji scope)
                              (hutch--scope-display-label scope)
                              (plist-get result :emsg))))
             (t
              (hutch--insert-findings findings display label result)))))))))

(defun hutch--render-result (buf result)
  "Store RESULT and re-render BUF."
  (let ((label (hutch--scope-label result)))
    (with-current-buffer buf
      (push (cons label result) hutch--results))
    (hutch--render-buffer buf)))

;;;###autoload
(defun hutch-magit-review ()
  "Review change with AI.  Show sections for branch, unpushed, and staged diffs."
  (interactive)
  (let ((scopes (hutch-collect-scopes)))
    (if (null scopes)
        (message "Hutch: no changes to review")
      (let ((buf (hutch--setup-buffer)))
        (with-current-buffer buf
          (setq hutch--scopes scopes
                hutch--results nil))
        (hutch--render-buffer buf)
        (pop-to-buffer buf)
        (setq hutch--cancel-fns
              (hutch--review scopes (lambda (result) (hutch--render-result buf result))))))))

(provide 'magit-hutch-ui)

;;; magit-hutch-ui.el ends here
