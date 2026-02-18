;;; hutch.el --- AI code review for magit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 adjaecent
;;
;; Author: adjaecent
;; URL: https://github.com/adjaecent/magit-hutch
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit "4.0") (llm "0.20"))

;;; Commentary:

;; Hutch provides AI-powered code review integrated with magit.
;; It reviews staged changes, unpushed commits, and branch diffs
;; using an LLM with tool-calling for codebase context.

;;; Code:

(require 'llm)
(require 'json)
(require 'hutch-debug)
(require 'hutch-git)
(require 'hutch-prompts)
(require 'hutch-tools)

;;; --- User-facing options ---

(defvar hutch-provider nil
  "LLM provider for hutch code reviews.
Set this to an `llm' provider instance, e.g.:
  (setq hutch-provider
    (make-llm-claude :key (getenv \"ANTHROPIC_API_KEY\")
                     :chat-model \"claude-sonnet-4-5-20250929\"))")

(defvar hutch-reasoning 'medium
  "Reasoning level for the review prompt.
One of nil, `none', `light', `medium', or `maximum'.")

;;; --- Response parsing ---

(defun hutch--strip-code-fences (str)
  "Remove markdown code fences from STR if present."
  (if (string-prefix-p "```" str)
      (replace-regexp-in-string
       "^```[a-z]*\n?" ""
       (replace-regexp-in-string "\n?```$" "" str))
    str))

(defun hutch--parse-json (str)
  "Parse STR as JSON, return list of plists or nil on error."
  (condition-case err
      (let ((json-object-type 'plist)
            (json-array-type 'list)
            (json-key-type 'keyword))
        (json-read-from-string str))
    (error
     (message "hutch: failed to parse JSON: %s" (error-message-string err))
     nil)))

;;; --- Findings ---

(defconst hutch--valid-finding-types '(lgtm suggestion comment)
  "Valid finding type symbols.")

(defun hutch--make-finding (type file lines title desc patch)
  "Create a finding plist of TYPE for FILE.
TYPE must be one of `hutch--valid-finding-types'."
  (unless (memq type hutch--valid-finding-types)
    (error "Invalid finding type %s, must be one of %s" type hutch--valid-finding-types))
  (list :type type
        :file file
        :lines lines
        :title title
        :desc desc
        :patch patch))

(defun hutch--normalize-finding (raw)
  "Normalize a RAW finding plist into a typed finding."
  (let ((file  (or (plist-get raw :file) "unknown"))
        (lgtm  (plist-get raw :lgtm))
        (patch (or (plist-get raw :patch)
                   (plist-get raw :fix)))
        (lines (or (plist-get raw :lines) "?"))
        (title (or (plist-get raw :title) "Issue"))
        (desc  (or (plist-get raw :description) "")))
    (cond
     ((eq lgtm t) (hutch--make-finding 'lgtm file nil nil nil nil))
     (patch       (hutch--make-finding 'suggestion file lines title desc patch))
     (t           (hutch--make-finding 'comment file lines title desc nil)))))

(defun hutch--parse-response (response)
  "Parse RESPONSE string into a list of normalized finding plists."
  (when-let ((raw (thread-last response
                               string-trim
                               hutch--strip-code-fences
                               hutch--parse-json)))
    (mapcar #'hutch--normalize-finding raw)))

;;; --- Results ---

(defconst hutch--valid-result-statuses '(:ok :error)
  "Valid result status keywords.")

(defun hutch--response-text (response)
  "Extract text from RESPONSE, which may be a string or multi-output plist."
  (if (stringp response) response (plist-get response :text)))

(defun hutch--make-result (status scope findings emsg)
  "Create a result plist with STATUS for SCOPE.
STATUS must be one of `hutch--valid-result-statuses'."
  (unless (memq status hutch--valid-result-statuses)
    (error "Invalid result status %s, must be one of %s"
           status hutch--valid-result-statuses))
  (list :status   status
        :scope    (plist-get scope :scope)
        :desc     (plist-get scope :desc)
        :hash     (plist-get scope :hash)
        :findings findings
        :emsg     emsg))

(defun hutch--make-success-result (scope response)
  "Build a success result plist from SCOPE and RESPONSE."
  (let ((text (hutch--response-text response)))
    (hutch--log "llm" "success for %s: %d chars"
                (plist-get scope :scope) (length (or text "")))
    (hutch--make-result :ok
                        scope
                        (when text (hutch--parse-response text))
                        nil)))

(defun hutch--make-error-result (scope type msg)
  "Build an error result plist from SCOPE, error TYPE, and MSG."
  (hutch--log "llm" "error for %s: %s: %s"
              (plist-get scope :scope) type msg)
  (hutch--make-result :error
                      scope
                      nil
                      (format "%s: %s" type msg)))

;;; --- LLM review ---

(defun hutch--make-prompt (scope)
  "Build an llm-chat-prompt for SCOPE with tools attached."
  (llm-make-chat-prompt
   (format hutch-review-template (plist-get scope :diff))
   :context hutch-system-prompt
   :examples (list hutch--review-example)
   :tools hutch--tools
   :reasoning hutch-reasoning
   :response-format 'json))

(defun hutch-review-scope (scope callback)
  "Review SCOPE asynchronously with tool use. Call CALLBACK with a result plist."
  (unless hutch-provider
    (error "hutch-provider is not set"))
  (hutch--log "llm" "starting review for %s %s"
              (plist-get scope :scope) (plist-get scope :desc))
  (llm-chat-async
   hutch-provider
   (hutch--make-prompt scope)
   (lambda (response)
     (funcall callback (hutch--make-success-result scope response)))
   (lambda (type msg)
     (funcall callback (hutch--make-error-result scope type msg)))
   t))

(defun hutch-review (callback)
  "Review all available scopes. Call CALLBACK with each result plist as it arrives."
  (let ((scopes (hutch-collect-scopes)))
    (if (null scopes)
        (message "hutch: no changes to review")
      (hutch--log "review" "found %d scopes" (length scopes))
      (dolist (scope scopes)
        (hutch--log "review" "dispatching %s %s"
                    (plist-get scope :scope) (plist-get scope :desc))
        (hutch-review-scope scope callback)))))

(provide 'hutch)

;;; hutch.el ends here
