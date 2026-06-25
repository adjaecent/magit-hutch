;;; hutch-findings.el --- Finding data types -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Akshay Gupta
;;
;; Author: Akshay Gupta
;; URL: https://github.com/adjaecent/magit-hutch

;;; Commentary:

;; Finding data structures and state transitions (pending → queued →
;; applied / rejected / invalid).  Findings are plists with a fixed
;; shape; this file owns construction, validation, and state changes.

;;; Code:

(require 'hutch-utils)

(defconst hutch--valid-finding-types '(lgtm suggestion comment)
  "Valid finding type symbols.")

;;
;;     pending
;;         ↑
;;       /  \
;;     /     \
;;    ↓       ↓
;;  queued    dismissed
;;   |    \
;;  |      \
;; |        \
;; ↓         ↓
;; applied  invalid
(defconst hutch--valid-finding-states '(pending queued applied dismissed invalid)
  "Valid finding state symbols.")

(defconst hutch--valid-finding-transitions
  '((pending   . (queued dismissed))
    (queued    . (pending applied invalid))
    (applied   . ())
    (invalid   . ())
    (dismissed . ()))
  "Allowed (CUR-STATE . (NEW-STATE ...)) transitions.")

(defun hutch--finding-state-transition (finding state)
  "Transition FINDING to STATE if the move is allowed.
Returns the finding (mutated if the transition is allowed; unchanged otherwise)."
  (unless (memq state hutch--valid-finding-states)
    (error "Invalid finding state %s, must be one of %s" state hutch--valid-finding-states))
  (let* ((cur-state (plist-get finding :state))
         (allowed   (alist-get cur-state hutch--valid-finding-transitions)))
    (when (memq state allowed)
      (setq finding (plist-put finding :state state)))
    finding))

(defun hutch--make-finding (type file lines title desc patch state)
  "Create a finding plist of TYPE for FILE with LINES and PATCH.
TITLE and DESC are short and long description text.  STATE is the
initial finding state (typically `pending').  TYPE must be one of
`hutch--valid-finding-types'."
  (unless (memq type hutch--valid-finding-types)
    (error "Invalid finding type %s, must be one of %s" type hutch--valid-finding-types))
  (list :id (gensym "f-")
        :type type
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

(provide 'hutch-findings)

;;; hutch-findings.el ends here
