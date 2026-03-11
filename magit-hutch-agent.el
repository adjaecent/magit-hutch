;;; magit-hutch-agent.el --- LLM review agent -*- lexical-binding: t; -*-

;;; Code:

(require 'llm)
(require 'magit-hutch-debug)
(require 'magit-hutch-git)
(require 'magit-hutch-prompts)
(require 'magit-hutch-tools)

;;; --- User-facing options ---

(defvar hutch-provider nil
  "LLM provider for hutch code reviews.
Set this to an `llm' provider instance, e.g.:
  (setq hutch-provider
    (make-llm-claude :key (getenv \"ANTHROPIC_API_KEY\")
                     :chat-model \"claude-sonnet-4-5-20250929\"))")

(defvar hutch-reasoning 'none
  "Reasoning level for the review prompt.
One of nil, `none', `light', `medium', or `maximum'.")

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

(defun hutch--truncate (str max)
  "Truncate STR to MAX chars, appending ellipsis if needed."
  (if (> (length str) max)
      (concat (substring str 0 (- max 1)) "\u2026")
    str))

;;; --- Results ---

(defun hutch--make-result (status scope findings emsg)
  "Create a result plist with STATUS for SCOPE."
  (list :status   status
        :scope    (plist-get scope :scope)
        :desc     (plist-get scope :desc)
        :hash     (plist-get scope :hash)
        :findings findings
        :emsg     emsg))

(defun hutch--make-error-result (scope type msg)
  "Build an error result plist from SCOPE, error TYPE, and MSG."
  (hutch--log "llm" "error for %s: %s: %s"
              (plist-get scope :scope) type msg)
  (hutch--make-result :error scope nil (format "%s: %s" type msg)))

;;; --- LLM review ---

(defvar hutch-max-tokens 4096
  "Maximum output tokens for the LLM review response.")

(defun hutch--make-prompt (scope)
  "Build an llm-chat-prompt for SCOPE with tools attached."
  (llm-make-chat-prompt
   (format hutch-review-template (plist-get scope :manifest))
   :context hutch-system-prompt
   :tools (hutch--tools-for-scope scope)
   :reasoning hutch-reasoning))

(defvar hutch-max-tool-rounds 20
  "Maximum number of tool-use rounds before forcing a result.")

(defun hutch--normalize-tool-finding (raw)
  "Normalize RAW alist finding from submit_review into a finding plist.
RAW is an alist with symbol keys (file, title, description, etc.)."
  (let* ((file  (or (alist-get 'file raw) "unknown"))
         (lgtm  (eq (alist-get 'lgtm raw) t))
         (patch (alist-get 'patch raw))
         (lines (or (alist-get 'lines raw) "?"))
         (title (hutch--truncate (or (alist-get 'title raw) "Issue") 80))
         (desc  (hutch--truncate (or (alist-get 'description raw) "") 300)))
    (cond
     (lgtm  (hutch--make-finding 'lgtm file nil nil nil nil))
     ((and patch (stringp patch) (not (string-empty-p patch)))
      (hutch--make-finding 'suggestion file lines title desc patch))
     (t     (hutch--make-finding 'comment file lines title desc nil)))))

(defun hutch--review-loop (scope prompt callback &optional round)
  "Send PROMPT to the LLM for SCOPE, looping on tool-use rounds.
When the LLM returns text or submit_review is called, invoke CALLBACK.
ROUND tracks the current iteration (default 0)."
  (when (null round)
    (setq hutch--submitted-findings nil))
  (let ((round (or round 0)))
    (llm-chat-async
     hutch-provider
     prompt
     (lambda (response)
       (cond
        ;; submit_review was called — use its findings directly
        (hutch--submitted-findings
         (hutch--log "llm" "submit_review received for %s: %d findings"
                     (plist-get scope :scope)
                     (length hutch--submitted-findings))
         (let ((findings (mapcar #'hutch--normalize-tool-finding
                                 hutch--submitted-findings)))
           (funcall callback
                    (hutch--make-result :ok scope findings nil))))
        ;; Tool round (not submit_review) — continue looping
        ((and (listp response) (plist-get response :tool-results))
         (if (>= round hutch-max-tool-rounds)
             (progn
               (hutch--log "llm" "hit max tool rounds (%d) for %s"
                           hutch-max-tool-rounds (plist-get scope :scope))
               (funcall callback
                        (hutch--make-error-result
                         scope 'tool-loop
                         (format "exceeded %d tool rounds"
                                 hutch-max-tool-rounds))))
           (hutch--log "llm" "tool round %d for %s, continuing..."
                       (1+ round) (plist-get scope :scope))
           (hutch--review-loop scope prompt callback (1+ round))))
        ;; Text response — nudge and keep looping
        (t
         (if (>= round hutch-max-tool-rounds)
             (progn
               (hutch--log "llm" "hit max rounds (%d) with text for %s"
                           hutch-max-tool-rounds (plist-get scope :scope))
               (funcall callback
                        (hutch--make-error-result
                         scope 'no-submit
                         "model never called submit_review")))
           (hutch--log "llm" "text response round %d for %s, nudging..."
                       (1+ round) (plist-get scope :scope))
           (llm-chat-prompt-append-response prompt response 'assistant)
           (llm-chat-prompt-append-response
            prompt "You must call submit_review now. Do not respond with text.")
           (hutch--review-loop scope prompt callback (1+ round))))))
     (lambda (type msg)
       (funcall callback (hutch--make-error-result scope type msg)))
     t)))

(defun hutch-review-scope (scope callback)
  "Review SCOPE asynchronously with tool use. Call CALLBACK with a result plist."
  (unless hutch-provider
    (error "hutch-provider is not set"))
  (hutch--log "llm" "starting review for %s %s"
              (plist-get scope :scope) (plist-get scope :desc))
  (hutch--review-loop scope (hutch--make-prompt scope) callback))

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

(provide 'magit-hutch-agent)

;;; magit-hutch-agent.el ends here
