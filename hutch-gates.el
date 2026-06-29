;;; hutch-gates.el --- Completes last-mile work done by the agent at submission time to curb hallucinations in the output -*- lexical-binding: t; -*-

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
;;
;; --- Gates ---
;;
;; These gates are applied in order:
;;   file  -- verifies that the file exists in the repo root
;;   patch -- patch creation; downgrades suggestion to comment on failure
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
  "Resolve SEARCH/REPLACE blocks on suggestion FINDINGS into unified diffs.
For each suggestion: read the file at SCOPE's ref, locate :old-lines, if
unique generate the patch and set :patch + :lines.  If not found, found
multiple times, or the file is missing, downgrade the finding to a
comment (the reason is logged via `hutch--log' but not surfaced in the
finding itself)."
  (mapcar
   (lambda (f)
     (if (not (eq (plist-get f :type) 'suggestion))
         f
       (let* ((file      (plist-get f :file))
              (title     (plist-get f :title))
              (desc      (plist-get f :desc))
              (old-lines (plist-get f :old-lines))
              (new-lines (plist-get f :new-lines))
              (source    (hutch--read-scope-file scope file)))
         (cond
          ((null source)
           (hutch--log "gate" "patch: %s missing at scope, downgrading" file)
           (hutch--make-finding 'comment file "?" title desc nil 'applied))
          (t
           (let ((hits (hutch--find-occurrences source old-lines)))
             (cond
              ((zerop (length hits))
               (hutch--log "gate" "patch: %s old-lines not found, downgrading" file)
               (hutch--make-finding 'comment file "?" title desc nil 'applied))
              ((> (length hits) 1)
               (hutch--log "gate" "patch: %s old-lines matches %d times, downgrading"
                           file (length hits))
               (hutch--make-finding 'comment file "?" title desc nil 'applied))
              (t
               (let* ((start-line (car hits))
                      (n-old      (length (string-lines old-lines)))
                      (end-line   (+ start-line n-old -1))
                      (new-source (string-replace old-lines new-lines source))
                      (patch      (hutch--diff-strings file source new-source))
                      (lines-str  (if (= start-line end-line)
                                      (number-to-string start-line)
                                    (format "%d-%d" start-line end-line))))
                 (hutch--make-finding 'suggestion file lines-str title desc patch 'pending
                                      old-lines new-lines))))))))))
          findings))

(defun hutch--run-gates (scope findings)
  "Run all gates over FINDINGS for SCOPE."
  (thread-first findings
                (hutch--gate-file)
                (hutch--gate-patch scope)))

(provide 'hutch-gates)

;;; hutch-gates.el ends here
