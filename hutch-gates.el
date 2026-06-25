;;; hutch-gates.el --- Verifies last-mile work done by the agent at submission time to curb hallucinations in the output -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Akshay Gupta
;;
;; Author: Akshay Gupta
;; URL: https://github.com/adjaecent/magit-hutch

;;; Commentary:
;;
;;
;; --- Gates ---
;;
;; These gates are applied in order:
;;   file  -- verifies that the file exists in the repo root
;;   patch -- verifies patch applicability; downgrades suggestion to comment on failure
;;
;; Gates can be filters and/or transformers

;;; Code:

(require 'magit)
(require 'seq)
(require 'hutch-git)
(require 'subr-x)
(require 'hutch-utils)
(require 'hutch-findings)

(defun hutch--gate-file (findings)
  "Drop FINDINGS whose :file does not exist in the repo root."
  (let* ((root   (magit-toplevel))
         (result (seq-filter
                  (lambda (f)
                    (or (eq (plist-get f :type) 'lgtm)
                        (file-exists-p (expand-file-name (plist-get f :file) root))))
                  findings)))
    (hutch--log "gate" "file: %d → %d" (length findings) (length result))
    result))

(defun hutch--gate-patch (findings scope)
  "Downgrade suggestion FINDINGS whose patch fails git apply --check in SCOPE."
  (mapcar (lambda (f)
            (let ((type  (plist-get f :type))
                  (file  (plist-get f :file))
                  (lines (plist-get f :lines))
                  (patch (plist-get f :patch))
                  (title (plist-get f :title))
                  (desc  (plist-get f :desc)))
              (if (and (eq type 'suggestion)
                       (not (car (hutch--patch-apply scope patch t))))
                  (progn
                    (hutch--log "gate" "patch: downgrading %s:%s to comment" file lines)
                    (hutch--make-finding 'comment
                                         file
                                         lines
                                         title
                                         desc
                                         nil
                                         'applied))
                f)))
          findings))

(defun hutch--run-gates (scope findings)
  "Run all gates over FINDINGS for SCOPE."
  (thread-first findings
                (hutch--gate-file)
                (hutch--gate-patch scope)))

(provide 'hutch-gates)

;;; hutch-gates.el ends here
