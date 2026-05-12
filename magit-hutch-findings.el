;;; magit-hutch-findings.el --- Finding data types -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'magit-hutch-utils)

(defconst hutch--valid-finding-types '(lgtm suggestion comment)
  "Valid finding type symbols.")

(defun hutch--make-finding (type file lines title desc patch)
  "Create a finding plist of TYPE for FILE with LINES and PATCH.
TYPE must be one of `hutch--valid-finding-types'."
  (unless (memq type hutch--valid-finding-types)
    (error "Invalid finding type %s, must be one of %s" type hutch--valid-finding-types))
  (list :type type
        :file file
        :lines lines
        :title title
        :desc desc
        :patch patch))

(defun hutch--normalize-tool-finding (raw)
  "Normalize RAW plist finding from submit_review into a finding plist."
  (let* ((file  (or (plist-get raw :file) "unknown"))
         (lgtm  (eq (plist-get raw :lgtm) t))
         (patch (plist-get raw :patch))
         (lines (or (plist-get raw :lines) "?"))
         (title (hutch--str-truncate (or (plist-get raw :title) "Issue") 80))
         (desc  (hutch--str-truncate (or (plist-get raw :description) "") 1000)))
    (cond
     (lgtm (hutch--make-finding 'lgtm file nil nil nil nil))
     ((and patch (stringp patch) (not (string-empty-p patch)))
      (hutch--make-finding 'suggestion file lines title desc patch))
     (t (hutch--make-finding 'comment file lines title desc nil)))))

(provide 'magit-hutch-findings)

;;; magit-hutch-findings.el ends here
