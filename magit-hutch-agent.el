;;; magit-hutch-agent.el --- LLM review agent -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'llm)
(require 'magit-hutch-utils)
(require 'magit-hutch-git)
(require 'magit-hutch-prompts)
(require 'magit-hutch-tools)
(require 'magit-hutch-cache)

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
  "Create a finding plist of TYPE for FILE with LINES and PATCH.
Summarized with TITLE AND DESC.
TYPE must be one of `hutch--valid-finding-types'."
  (unless (memq type hutch--valid-finding-types)
    (error "Invalid finding type %s, must be one of %s" type hutch--valid-finding-types))
  (list :type type
        :file file
        :lines lines
        :title title
        :desc desc
        :patch patch))

;;; --- Results ---

(defun hutch--make-result (status scope findings emsg &optional tokens)
  "Create a result plist with FINDINGS and STATUS for SCOPE.
TOKENS is an optional (input . output) cons cell."
  (list :status   status
        :scope    (plist-get scope :scope)
        :desc     (plist-get scope :desc)
        :hash     (plist-get scope :hash)
        :findings findings
        :emsg     emsg
        :input-tokens  (car tokens)
        :output-tokens (cdr tokens)))

(defun hutch--make-error-result (scope type msg)
  "Build an error result plist from SCOPE, error TYPE, and MSG."
  (hutch--log "llm" "error for %s: %s: %s" (plist-get scope :scope) type msg)
  (hutch--make-result :error scope nil (format "%s: %s" type msg)))

;;; --- LLM review ---

(defvar hutch-max-tokens 4096
  "Maximum output tokens for the LLM review response.")

(defun hutch--make-prompt (scope result-box)
  "Build an llm-chat-prompt for SCOPE with tools attached.
Pass a RESULT-BOX into the tools closure."
  (llm-make-chat-prompt
   (format hutch-review-template (plist-get scope :manifest))
   :context hutch-system-prompt
   :tools (hutch--tools-for-scope scope result-box)
   :reasoning hutch-reasoning))

(defun hutch--normalize-tool-finding (raw)
  "Normalize RAW alist finding from submit_review into a finding plist.
RAW is an alist with symbol keys (file, title, description, etc.)."
  (let* ((file  (or (alist-get 'file raw) "unknown"))
         (lgtm  (eq (alist-get 'lgtm raw) t))
         (patch (alist-get 'patch raw))
         (lines (or (alist-get 'lines raw) "?"))
         (title (hutch--str-truncate (or (alist-get 'title raw) "Issue") 80))
         (desc  (hutch--str-truncate (or (alist-get 'description raw) "") 300)))
    (cond
     (lgtm (hutch--make-finding 'lgtm file nil nil nil nil))
     ((and patch (stringp patch) (not (string-empty-p patch)))
      (hutch--make-finding 'suggestion file lines title desc patch))
     (t (hutch--make-finding 'comment file lines title desc nil)))))

(defun hutch--accumulate-tokens (token-box response round)
  "Add token counts from RESPONSE into TOKEN-BOX.
TOKEN-BOX car is (input-total . output-total)."
  (when (listp response)
    (let ((cur (hutch--result-box-get token-box))
          (in  (plist-get response :input-tokens))
          (out (plist-get response :output-tokens)))
      (when (or in out)
        (hutch--log "tokens" "round %d +R%d/+W%d" round (or in 0) (or out 0))
        (hutch--result-box-set
         token-box
         (cons (+ (or (car cur) 0) (or in 0))
               (+ (or (cdr cur) 0) (or out 0))))))))

(defun hutch--agent-loop (prompt round on-done on-error result-box token-box cancel-box max-rounds)
  "Generic async tool-use loop with PROMPT.
Calls ON-DONE when RESULT-BOX is set, ON-ERROR with (type msg) on failure.
Accumulates token usage in TOKEN-BOX across rounds.
Tracks ROUND recursively bounded by MAX-ROUNDS."
  (->> (llm-chat-async
        hutch-provider
        prompt
        (lambda (response)
          (hutch--accumulate-tokens token-box response round)
          (hutch--log "debug" "round %d result-box=%s tool-results=%s text=%s"
                round
                (if (hutch--result-box-get result-box) "SET" "nil")
                (if (and (listp response) (plist-get response :tool-results)) "yes" "no")
                (if (and (listp response) (plist-get response :text)) "yes" "no"))
          (cond
           ;; got a result back, exit
           ((hutch--result-box-get result-box)
            (hutch--result-box-set cancel-box nil)
            (funcall on-done))

           ;; not the final tool call (continue looping)
           ((and (listp response) (plist-get response :tool-results))
            (if (>= round max-rounds)
                (funcall on-error 'tool-loop-exceeded "exceeded max tool rounds")
              (hutch--agent-loop prompt
                                 (+ 1 round)
                                 on-done
                                 on-error
                                 result-box
                                 token-box
                                 cancel-box
                                 max-rounds)))

           ;; nudge and keep looping (response is text)
           (t
            (if (>= round max-rounds)
                (funcall on-error 'no-submit "model never called result tool")
              (llm-chat-prompt-append-response prompt response 'assistant)
              (llm-chat-prompt-append-response
               prompt
               "Do not respond with text. Use your tools to complete the task.")
              (hutch--agent-loop prompt
                                 (+ 1 round)
                                 on-done
                                 on-error
                                 result-box
                                 token-box
                                 cancel-box
                                 max-rounds)))))
        on-error
        t)
       (hutch--result-box-set cancel-box)))

(defun hutch--agent-cancel (cancel-box)
  "Cancel an LLM request if there's a handle in CANCEL-BOX."
  (when-let ((handle (hutch--result-box-get cancel-box)))
    (llm-cancel-request handle)))

(defun hutch--review-on-done (scope result-box token-box callback)
  "Handle successful review for SCOPE.
Read findings from RESULT-BOX, token counts from TOKEN-BOX, call CALLBACK."
  (let* ((result   (hutch--result-box-get result-box))
         (tokens   (hutch--result-box-get token-box))
         (findings (mapcar #'hutch--normalize-tool-finding result)))
    (hutch--log "llm"
                "submit_review received for %s: %d findings (R%dk/W%dk)"
                (plist-get scope :scope)
                (length findings)
                (/ (or (car tokens) 0) 1000)
                (/ (or (cdr tokens) 0) 1000))
    (funcall callback (hutch--make-result :ok scope findings nil tokens))))

(defun hutch--review-on-error (scope callback type msg)
  "Handle review error for SCOPE.  Call CALLBACK with error result from TYPE and MSG."
  (funcall callback (hutch--make-error-result scope type msg)))

(defun hutch--review-scope (scope callback cancel-box)
  "Review SCOPE asynchronously with tool use.  Call CALLBACK with a result plist.
Argument CANCEL-BOX to be passed through."
  (unless hutch-provider (error "hutch-provider is not set"))
  (let ((result-box    (hutch--make-result-box))
        (token-box     (hutch--make-result-box)))
    (hutch--agent-loop
     (hutch--make-prompt scope result-box)
     0
     (lambda () (hutch--review-on-done scope result-box token-box callback))
     (lambda (type msg) (hutch--review-on-error scope callback type msg))
     result-box
     token-box
     cancel-box
     20)))

(defun hutch--review (scopes callback)
  "Review SCOPES with caching.  Call CALLBACK with each result plist as it arrives.
Returns a list of cancel-callbacks to cancel reviews for SCOPES."
  (hutch--log "review" "found %d scopes" (length scopes))
  (mapcar (lambda (scope)
            (let ((hash             (plist-get scope :hash))
                  (desc             (plist-get scope :desc))
                  (scope-key        (plist-get scope :scope))
                  (cancel-scope-box (hutch--make-result-box)))
              (if-let ((cached (hutch--cache-lookup hash)))
                  (progn
                    (hutch--log "review" "cache hit for %s %s" scope-key desc)
                    (funcall callback cached)
                    (lambda () nil))
                (hutch--log "review" "dispatching %s %s" scope-key desc)
                (hutch--review-scope scope
                                     (hutch--write-through-cache-callback hash callback)
                                     cancel-scope-box)
                (lambda () (hutch--agent-cancel cancel-scope-box)))))
          scopes))

(provide 'magit-hutch-agent)

;;; magit-hutch-agent.el ends here
