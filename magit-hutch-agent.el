;;; magit-hutch-agent.el --- LLM review agent -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'gptel-request)
(require 'magit-hutch-utils)
(require 'magit-hutch-git)
(require 'magit-hutch-prompts)
(require 'magit-hutch-tools)
(require 'magit-hutch-cache)

;;; --- User-facing options ---

(defvar hutch-backend nil
  "gptel backend for hutch code reviews.
Defaults to `gptel-backend' if nil.  Set this to a gptel backend, e.g.:
  (setq hutch-backend
        (gptel-make-anthropic \"Claude\"
          :key (getenv \"ANTHROPIC_API_KEY\")
          :models \\='(claude-sonnet-4-6)))")

(defvar hutch-model nil
  "Model to use for hutch reviews.  Defaults to `gptel-model' if nil.")

;;; --- Findings ---

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

(defun hutch--sanitize-emsg (type msg)
  "Return a brief, single-line error string from TYPE symbol and MSG."
  (let* ((raw  (or msg (format "%s" type)))
         (line (car (split-string raw "\n")))
         (text (string-trim line)))
    (hutch--str-truncate (if (string-empty-p text) (format "%s" type) text) 120)))

(defun hutch--make-error-result (scope type msg)
  "Build an error result plist from SCOPE, error TYPE, and MSG."
  (hutch--log "llm" "error for %s: %s: %s" (plist-get scope :scope) type msg)
  (hutch--make-result :error scope nil (hutch--sanitize-emsg type msg)))

(defun hutch--normalize-tool-finding (raw)
  "Normalize RAW plist finding from submit_review into a finding plist."
  (let* ((file  (or (plist-get raw :file) "unknown"))
         (lgtm  (eq (plist-get raw :lgtm) t))
         (patch (plist-get raw :patch))
         (lines (or (plist-get raw :lines) "?"))
         (title (hutch--str-truncate (or (plist-get raw :title) "Issue") 80))
         (desc  (hutch--str-truncate (or (plist-get raw :description) "") 300)))
    (cond
     (lgtm (hutch--make-finding 'lgtm file nil nil nil nil))
     ((and patch (stringp patch) (not (string-empty-p patch)))
      (hutch--make-finding 'suggestion file lines title desc patch))
     (t (hutch--make-finding 'comment file lines title desc nil)))))

(defun hutch--accumulate-tokens (token-box info round)
  "Add token counts from INFO into TOKEN-BOX for ROUND."
  (when-let ((tokens (plist-get info :tokens)))
    (let* ((cur (hutch--result-box-get token-box))
           (in  (plist-get tokens :input))
           (out (plist-get tokens :output)))
      (when (or in out)
        (hutch--log "tokens" "round %d +R%d/+W%d" round (or in 0) (or out 0))
        (hutch--result-box-set
         token-box
         (cons (+ (or (car cur) 0) (or in 0))
               (+ (or (cdr cur) 0) (or out 0))))))))

(defun hutch--agent-request (response info on-done on-error result-box token-box round-box max-rounds)
  "gptel callback that drives one step of the tool-use loop.
RESPONSE and INFO are the gptel callback arguments.  ON-DONE is called
when RESULT-BOX is set (submit_review fired).  ON-ERROR is called with
\(TYPE MSG) on failure.  ROUND-BOX tracks the number of tool rounds;
errors if it exceeds MAX-ROUNDS.  Token counts accumulate into TOKEN-BOX."
  (let ((round (or (hutch--result-box-get round-box) 0)))
    (hutch--accumulate-tokens token-box info round)
    (hutch--log "debug" "round %d response=%S"
                round (cond ((stringp response) "text")
                            ((consp response) (car response))
                            (t response)))
    (cond
     ((eq response 'abort) nil)

     ((null response)
      (funcall on-error 'llm-error (plist-get info :status)))

     ;; gptel auto-executed tools; check result and track rounds
     ((and (consp response) (eq (car response) 'tool-result))
      (hutch--result-box-set round-box (1+ round))
      (cond
       ((hutch--result-box-get result-box)
        (funcall on-done))
       ((> (1+ round) max-rounds)
        (funcall on-error 'tool-loop-exceeded "exceeded max tool rounds"))))

     ;; text response — error only if submit_review was never called
     ((stringp response)
      (unless (hutch--result-box-get result-box)
        (funcall on-error 'no-submit "model responded with text instead of tools"))))))

(defun hutch--agent (prompt on-done on-error result-box token-box cancel-box round-box max-rounds)
  "Start a gptel request with PROMPT and a callback that drives the tool-use loop.
Calls ON-DONE when RESULT-BOX is set, ON-ERROR with (type msg) on failure.
Accumulates token usage in TOKEN-BOX.  Bounded by MAX-ROUNDS."
  (->>
   (gptel-request (plist-get prompt :user)
                  :system (plist-get prompt :system)
                  :callback (lambda (response info) (hutch--agent-request response
                                                                          info
                                                                          on-done
                                                                          on-error
                                                                          result-box
                                                                          token-box
                                                                          round-box
                                                                          max-rounds)))
   (hutch--result-box-set cancel-box)))

(defun hutch--agent-cancel (cancel-box)
  "Cancel an in-flight review if there is an FSM handle in CANCEL-BOX."
  (when-let ((fsm (hutch--result-box-get cancel-box)))
    (gptel--fsm-transition fsm 'ABRT)))

(defun hutch--review-on-done (scope result-box token-box callback)
  "Handle successful review for SCOPE.
Read findings from RESULT-BOX, token counts from TOKEN-BOX, call CALLBACK."
  (let* ((result   (hutch--result-box-get result-box))
         (tokens   (hutch--result-box-get token-box))
         (findings (mapcar #'hutch--normalize-tool-finding result)))
    (hutch--log "llm" "submit_review for %s: %d findings (R%dk/W%dk)"
                (plist-get scope :scope)
                (length findings)
                (/ (or (car tokens) 0) 1000)
                (/ (or (cdr tokens) 0) 1000))
    (funcall callback (hutch--make-result :ok scope findings nil tokens))))

(defun hutch--review-on-error (scope callback type msg)
  "Handle review error for SCOPE.  Call CALLBACK with error result from TYPE and MSG."
  (funcall callback (hutch--make-error-result scope type msg)))

(defun hutch--review-scope (scope callback cancel-box)
  "Review SCOPE asynchronously with tool use.  Call CALLBACK with a result plist."
  (unless (or hutch-backend gptel-backend)
    (error "hutch-backend is not set"))
  (let* ((result-box    (hutch--make-result-box))
         (token-box     (hutch--make-result-box))
         (round-box     (hutch--make-result-box))
         (tools         (hutch--make-tools scope result-box))
         (user-prompt   (format hutch-review-template (plist-get scope :manifest)))
         (prompt        (list :user user-prompt :system hutch-system-prompt))
         (gptel-backend (or hutch-backend gptel-backend))
         (gptel-model   (or hutch-model gptel-model))
         (gptel-tools   tools))
    (hutch--agent
     prompt
     (lambda ()
       (hutch--agent-cancel cancel-box)
       (hutch--review-on-done scope result-box token-box callback))
     (lambda (type msg)
       (hutch--agent-cancel cancel-box)
       (hutch--review-on-error scope callback type msg))
     result-box
     token-box
     cancel-box
     round-box
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
