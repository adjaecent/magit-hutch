;;; magit-hutch-findings.el --- Finding data types -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'magit-hutch-utils)

(defconst hutch--valid-finding-types '(lgtm suggestion comment)
  "Valid finding type symbols.")

;;     pending
;;      /   \
;;    /      \
;; applied  dismissed
(defconst hutch--valid-finding-states '(pending applied dismissed)
  "Valid finding state symbols.")

(defun hutch--finding-state-transition (finding state)
  "Transition FINDING to STATE.  Only 'pending findings can move ahead."
  (unless (memq state hutch--valid-finding-states)
    (error "Invalid finding state %s, must be one of %s" state hutch--valid-finding-states))
  (let ((cur-state (plist-get finding :state)))
    (when (and (eq cur-state 'pending)
               (or (eq state 'applied)
                   (eq state 'dismissed)))
      (setq finding (plist-put finding :state state)))
    finding))

(defun hutch--make-finding (type file lines title desc patch state)
  "Create a finding plist of TYPE for FILE with LINES and PATCH.
TYPE must be one of `hutch--valid-finding-types'."
  (unless (memq type hutch--valid-finding-types)
    (error "Invalid finding type %s, must be one of %s" type hutch--valid-finding-types))
  (list :type type
        :file file
        :lines lines
        :title title
        :desc desc
        :patch patch
        :state state))

(defun hutch--normalize-tool-finding (raw)
  "Normalize RAW plist finding from submit_review into a finding plist."
  (let* ((file  (or (plist-get raw :file) "unknown"))
         (lgtm  (eq (plist-get raw :lgtm) t))
         (patch (plist-get raw :patch))
         (lines (or (plist-get raw :lines) "?"))
         (title (hutch--str-truncate (or (plist-get raw :title) "Issue") 80))
         (desc  (hutch--str-truncate (or (plist-get raw :description) "") 1000)))
    (cond
     (lgtm (hutch--make-finding 'lgtm file nil nil nil nil 'applied))
     ((and patch (stringp patch) (not (string-empty-p patch)))
      (hutch--make-finding 'suggestion file lines title desc patch 'pending))
     (t (hutch--make-finding 'comment file lines title desc nil 'applied)))))

(provide 'magit-hutch-findings)

;;; magit-hutch-findings.el ends here
