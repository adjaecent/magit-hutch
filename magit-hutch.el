;;; magit-hutch.el --- AI code review for magit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 adjaecent
;;
;; Author: adjaecent
;; URL: https://github.com/adjaecent/magit-hutch
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit "4.0") (llm "0.20"))

;;; Commentary:

;; Hutch provides AI-powered code review integrated with magit.
;; It reviews staged changes, unpushed commits, and branch diffs
;; using an LLM with tool-calling for codebase context.
;;
;; Usage:
;;   (setq hutch-provider
;;     (make-llm-claude :key (getenv "ANTHROPIC_API_KEY")
;;                      :chat-model "claude-sonnet-4-5-20250929"))

;;; Code:

(require 'magit-hutch-agent)
(require 'magit-hutch-ui)

(with-eval-after-load 'magit
  (transient-append-suffix 'magit-diff "d"
    '("R" "AI Review" hutch-magit-review)))

(provide 'magit-hutch)

;;; magit-hutch.el ends here
