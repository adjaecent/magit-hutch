;;; hutch-ui.el --- Magit-section UI for hutch reviews -*- lexical-binding: t; -*-

;;; Commentary:

;; Renders hutch review results as magit sections with
;; accept/reject actions for suggestions.

;;; Code:

(require 'hutch)
(require 'magit-section)

;;; --- Mode and keymaps ---

(defvar-keymap hutch-mode-map
  :parent magit-section-mode-map
  "q" #'quit-window)

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

(defun hutch--wrap-text (text width prefix)
  "Wrap TEXT to WIDTH, prefixing each line with PREFIX."
  (with-temp-buffer
    (insert text)
    (let ((fill-column (- width (length prefix)))
          (fill-prefix prefix))
      (fill-region (point-min) (point-max))
      (goto-char (point-min))
      (insert prefix)
      (buffer-string))))

(defun hutch--insert-desc (desc)
  "Insert DESC wrapped to 80 chars with 4-space indent."
  (insert (hutch--wrap-text desc 80 "    ") "\n"))

;;; --- Section inserters ---

(defun hutch--insert-lgtm (finding)
  "Insert a LGTM section for FINDING."
  (magit-insert-section (review-lgtm (plist-get finding :file))
    (magit-insert-heading
      (propertize (format "  LGTM %s" (plist-get finding :file))
                  'font-lock-face 'magit-diff-file-heading))))

(defun hutch--insert-suggestion (finding)
  "Insert a suggestion section (has a patch)."
  (magit-insert-section (review-suggestion finding t)
    (magit-insert-heading
      (propertize (format "  %s:%s -- %s"
                          (plist-get finding :file)
                          (plist-get finding :lines)
                          (plist-get finding :title))
                  'font-lock-face 'magit-diff-hunk-heading))
    (hutch--insert-desc (plist-get finding :desc))
    (hutch--insert-patch-lines (plist-get finding :patch))))

(defun hutch--insert-comment (finding)
  "Insert a comment section (no patch). Press `x' to dismiss."
  (magit-insert-section (review-comment finding t)
    (magit-insert-heading
      (propertize (format "  %s:%s -- %s"
                          (plist-get finding :file)
                          (plist-get finding :lines)
                          (plist-get finding :title))
                  'font-lock-face 'magit-diff-hunk-heading))
    (hutch--insert-desc (plist-get finding :desc))))

(defun hutch--insert-finding (finding)
  "Insert FINDING based on its :type."
  (pcase (plist-get finding :type)
    ('lgtm       (hutch--insert-lgtm finding))
    ('suggestion (hutch--insert-suggestion finding))
    ('comment    (hutch--insert-comment finding))))

(defun hutch--insert-findings (findings section-label)
  "Insert FINDINGS as magit sections under SECTION-LABEL."
  (magit-insert-section (review-group section-label)
    (magit-insert-heading
      (propertize section-label 'font-lock-face 'magit-section-heading))
    (if (null findings)
        (insert "  No issues found.\n")
      (dolist (finding findings)
        (hutch--insert-finding finding)))
    (insert "\n")))

;;; --- Accept / reject ---

(defun hutch--git-apply-check (patch directory)
  "Dry-run PATCH with git apply --check in DIRECTORY. Return t if clean."
  (let ((default-directory directory))
    (zerop (with-temp-buffer
             (insert patch "\n")
             (call-process-region (point-min) (point-max)
                                  "git" nil t nil
                                  "apply" "--check" "-")))))

(defun hutch--git-apply (patch directory)
  "Apply PATCH with git apply in DIRECTORY."
  (let ((default-directory directory))
    (with-temp-buffer
      (insert patch "\n")
      (call-process-region (point-min) (point-max)
                           "git" nil t nil
                           "apply" "-"))))

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

(defun hutch--setup-buffer ()
  "Create and prepare the review buffer. Return it."
  (let ((buf (get-buffer-create "*hutch: code review*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (hutch-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (review-root)
          (insert (propertize "hutch: Code Review\n"
                              'font-lock-face 'magit-section-heading)))))
    buf))

(defun hutch--replace-status (buf old-text new-text)
  "In BUF, replace OLD-TEXT with NEW-TEXT."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (when (search-forward old-text nil t)
          (replace-match new-text t t))))))

(defun hutch--render-result (buf result)
  "Render a review RESULT into BUF."
  (let* ((scope (plist-get result :scope))
         (desc (plist-get result :desc))
         (label (format "%s %s" scope desc))
         (status-text (format "  Reviewing: %s..." (downcase label))))
    (if (eq (plist-get result :status) :error)
        (hutch--replace-status buf status-text
                               (format "  Review failed: %s"
                                       (plist-get result :emsg)))
      (hutch--replace-status buf status-text "  Review complete")
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (hutch--insert-findings (plist-get result :findings) label))))))

;;;###autoload
(defun hutch-magit-review ()
  "Review changes with AI. Shows sections for branch, unpushed, and staged diffs."
  (interactive)
  (let ((scopes (hutch-collect-scopes)))
    (if (null scopes)
        (message "hutch: no changes to review")
      (let ((buf (hutch--setup-buffer)))
        (pop-to-buffer buf)
        ;; Insert status lines
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (dolist (scope scopes)
              (let* ((label (format "%s %s"
                                    (plist-get scope :scope)
                                    (plist-get scope :desc))))
                (insert (format "  Reviewing: %s...\n"
                                (downcase label)))))))
        ;; Fire off reviews
        (dolist (scope scopes)
          (hutch-review-scope
           scope
           (lambda (result)
             (hutch--render-result buf result))))))))

(with-eval-after-load 'magit
  (transient-append-suffix 'magit-diff "d"
    '("R" "AI Review" hutch-magit-review)))

(provide 'hutch-ui)

;;; hutch-ui.el ends here
