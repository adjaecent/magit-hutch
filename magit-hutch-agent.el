;;; magit-hutch-agent.el --- LLM review agent -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'dash)
(require 'gptel-request)
(require 'magit-hutch-utils)
(require 'magit-hutch-git)
(require 'magit-hutch-prompts)
(require 'magit-hutch-tools)
(require 'magit-hutch-cache)
(require 'magit-hutch-findings)
(require 'magit-hutch-gates)

;;; --- User-facing options ---

(defgroup hutch nil
  "AI code review integrated with Magit."
  :group 'magit
  :prefix "hutch-")

(defvar hutch-backend nil
  "gptel backend for hutch code reviews.
Defaults to `gptel-backend' if nil.  Set this to a gptel backend, e.g.:
  (setq hutch-backend
        (gptel-make-anthropic \"Claude\"
          :key (getenv \"ANTHROPIC_API_KEY\")
          :models \\='(claude-sonnet-4-6)))")

(defcustom hutch-max-tool-rounds 20
  "Maximum number of tool-call rounds per review scope before giving up."
  :type 'integer
  :group 'hutch)

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


(defun hutch--accumulate-tokens (token-box info round)
  "Add token counts from INFO into TOKEN-BOX for ROUND."
  (let ((in  (plist-get info :input-tokens))
        (out (plist-get info :output-tokens)))
    (when (or in out)
      (hutch--log "tokens" "round %d +R%d/+W%d" round (or in 0) (or out 0))
      (let ((cur (hutch--result-box-get token-box)))
        (hutch--result-box-set
         token-box
         (cons (when (or in (car cur)) (+ (or (car cur) 0) (or in 0)))
               (when (or out (cdr cur)) (+ (or (cdr cur) 0) (or out 0)))))))))

(defun hutch--agent-request (response info on-done on-error result-box token-box round-box max-rounds)
  "gptel callback that drives one step of the tool-use loop.
RESPONSE and INFO are the gptel callback arguments.  ON-DONE is called
when RESULT-BOX is set (submit_review fired).  ON-ERROR is called with
\(TYPE MSG) on failure.  ROUND-BOX tracks the number of tool rounds;
errors if it exceeds MAX-ROUNDS.  Token counts accumulate into TOKEN-BOX."
  (let* ((round             (or (hutch--result-box-get round-box) 0))
         (is-abort          (eq response 'abort))
         (is-nil-resp       (null response))
         (is-str-resp       (stringp response))
         (evt-resp          (and (consp response) (car response)))
         (is-tool-resp      (eq evt-resp 'tool-result))
         (is-reasoning-resp (eq evt-resp 'reasoning)))
    (hutch--log "debug" "round %d response=%S"
                round (cond ((stringp response) "text")
                            (evt-resp evt-resp)
                            (t response)))
    (cond
     (is-abort nil) ;; was aborted in the FSM or cancelled externally
     (is-nil-resp (funcall on-error 'llm-error (plist-get info :status))) ;; no response

     ;; gptel auto-executed tools; check result and track rounds
     (is-tool-resp
      (hutch--accumulate-tokens token-box info round)
      (hutch--result-box-set round-box (1+ round))
      (cond
       ((hutch--result-box-get result-box)
        (condition-case err
            (funcall on-done)
          (error (funcall on-error 'on-done-error (error-message-string err)))))
       ((> (1+ round) max-rounds)
        (funcall on-error 'tool-loop-exceeded "exceeded max tool rounds"))))

     ;; reasoning block — normally intermediate, but if stop_reason is max_tokens
     ;; the FSM goes DONE with no further callback; surface that as an error
     (is-reasoning-resp
      (when (equal (plist-get info :status) "max_tokens")
        (funcall on-error 'max-tokens "stopped mid-thinking: max_tokens exhausted")))

     ;; text response — error only if no tools are pending in the same response.
     ;; Some providers (e.g. DeepSeek) emit prose alongside a tool_call in the
     ;; final response; we don't want to error on the text and miss the tool.
     ((and is-str-resp
           (not (hutch--result-box-get result-box))
           (not (plist-get info :tool-use)))
      (funcall on-error 'no-submit "model responded with text instead of calling submit_review"))

     ;; text alongside pending tools — no-op, wait for the tool result
     (is-str-resp nil)

     ;; unrecognized response type
     (t
      (funcall on-error 'unexpected-response
               (format "model stopped unexpectedly (response type: %s)"
                       (or evt-resp response)))))))

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
         (findings (->> result
                        (mapcar #'hutch--normalize-tool-finding)
                        (hutch--run-gates scope))))
    (hutch--log "llm" "submit_review for %s: %d findings after gates"
                (plist-get scope :scope)
                (length findings))
    (funcall callback (hutch--make-result :ok scope findings nil tokens))))

(defun hutch--review-on-error (scope callback type msg)
  "Handle review error for SCOPE.  Call CALLBACK with error result from TYPE and MSG."
  (funcall callback (hutch--make-error-result scope type msg)))

(defun hutch--review-scope (scope callback cancel-box)
  "Review SCOPE asynchronously with tool use.  Call CALLBACK with a result plist."
  (unless hutch-backend
    (error "hutch-backend is not set"))
  (let* ((result-box    (hutch--make-result-box))
         (token-box     (hutch--make-result-box))
         (round-box     (hutch--make-result-box))
         (tools         (hutch--make-tools scope result-box))
         (user-prompt   (format hutch-review-template (plist-get scope :manifest)))
         (prompt        (list :user user-prompt :system hutch-system-prompt))
         (gptel-backend hutch-backend)
         (gptel-model   (car (gptel-backend-models hutch-backend)))
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
     hutch-max-tool-rounds)))

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
