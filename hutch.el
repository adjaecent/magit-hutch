;;; hutch.el --- AI code review for magit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Akshay Gupta
;;
;; Author: Akshay Gupta <mail@kitallis.in>
;; URL: https://github.com/adjaecent/magit-hutch
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit "4.0") (gptel "0.9.9.3") (svg-lib "0.3"))
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

;; Hutch provides AI-powered code review integrated with magit.
;; It reviews staged changes, unpushed commits, and branch diffs
;; using an LLM (via gptel) with tool-calling for codebase context.
;;
;; Configure a gptel backend as `hutch-backend', then invoke a review
;; via `M-x hutch-magit-review'.
;;
;; To bind it inside the `magit-diff' transient, call
;; `hutch-add-review-binding' in your init file:
;;
;;   (with-eval-after-load 'magit
;;     (hutch-add-review-binding)) ;; default: `R'
;;     ;; or pick your own key:
;;     ;; (hutch-add-review-binding "K")
;;
;; That adds the suffix next to `magit-diff-dwim', so it appears under
;; the chosen key inside the diff popup.
;;
;; Example backends:
;;
;;   ;; Anthropic
;;   (setq hutch-backend
;;         (gptel-make-anthropic "Claude"
;;           :key (getenv "ANTHROPIC_API_KEY")
;;           :models '(claude-sonnet-4-6)))
;;
;;   ;; OpenAI
;;   (setq hutch-backend
;;         (gptel-make-openai "OpenAI"
;;           :key (getenv "OPENAI_API_KEY")
;;           :models '(gpt-5.5)))

;;; Code:

(require 'hutch-agent)
(require 'hutch-ui)

;;;###autoload
(defun hutch-add-review-binding (&optional key)
  "Add KEY as a `hutch-magit-review' suffix to the `magit-diff' transient.
KEY defaults to \"R\".  Inserts the suffix immediately after
`magit-diff-dwim', so it appears under KEY inside the diff popup.
Call from your init file, ideally inside `with-eval-after-load' for
`magit'."
  (require 'transient)
  (require 'magit)
  (transient-append-suffix 'magit-diff 'magit-diff-dwim
    `(,(or key "R") "Hutch code review" hutch-magit-review)))

(provide 'hutch)

;;; hutch.el ends here
