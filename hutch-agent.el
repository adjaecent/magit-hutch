;;; hutch-agent.el --- LLM review agent -*- lexical-binding: t; -*-

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

;; Generic agent loop and review-specific callbacks.  Drives the
;; multi-turn tool-calling conversation with the LLM and dispatches
;; findings into the review buffer.

;;; Code:

(require 'subr-x)
(require 'gptel-request)
(require 'hutch-utils)
(require 'hutch-git)
(require 'hutch-prompts)
(require 'hutch-tools)
(require 'hutch-cache)
(require 'hutch-findings)
(require 'hutch-gates)
(require 'hutch-instrument)

;;; --- User-facing options ---

(defgroup hutch nil
  "AI code review integrated with Magit."
  :group 'magit
  :prefix "hutch-")

(defvar hutch-backend nil
  "Gptel backend for hutch code reviews.
Defaults to `gptel-backend' if nil.  Set this to a gptel backend, e.g.:
  (setq hutch-backend
        (gptel-make-anthropic \"Claude\"
          :key (getenv \"ANTHROPIC_API_KEY\")
          :models \\='(claude-sonnet-4-6)))")

(defcustom hutch-max-tool-rounds 40
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

(defun hutch--agent-request (response info on-done on-error on-progress result-box token-box round-box max-rounds scope-id)
  "Gptel callback that drives one step of the tool-use loop.
RESPONSE and INFO are the gptel callback arguments.  ON-DONE is called
when RESULT-BOX is set (submit_review fired).  ON-ERROR is called with
\(TYPE MSG) on failure.  ON-PROGRESS, if non-nil, is called as
\(CURRENT MAX) after each tool round.  ROUND-BOX tracks the number of
tool rounds; errors if it exceeds MAX-ROUNDS.  Token counts accumulate
into TOKEN-BOX."
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
      (let ((completed (1+ round)))
        (hutch--result-box-set round-box completed)

        (hutch--trace-slice-end "llm-call" "llm" scope-id)
        (let ((tokens (hutch--result-box-get token-box)))
          (hutch--trace-counter "tokens"
                                scope-id
                                `(:input ,(or (car tokens) 0) :output ,(or (cdr tokens) 0))))

        (when on-progress (funcall on-progress completed max-rounds))
        (cond
         ((hutch--result-box-get result-box)
          (condition-case err
              (funcall on-done)
            (error (funcall on-error 'on-done-error (error-message-string err)))))
         ((> completed max-rounds)
          (funcall on-error 'tool-loop-exceeded "exceeded max tool rounds"))
         (t
          (hutch--trace-slice-beg "llm-call" "llm" scope-id `(:turn ,(1+ completed)))))))

     ;; reasoning block — normally intermediate, but if stop_reason is max_tokens
     ;; the FSM goes DONE with no further callback; surface that as an error
     (is-reasoning-resp
      (when (equal (plist-get info :status) "max_tokens")
        (funcall on-error 'max-tokens "stopped mid-thinking: max_tokens exhausted")))

     ;; an empty response is considered model giving up
     ((and is-str-resp
           (string-empty-p response)
           (not (hutch--result-box-get result-box))
           (not (plist-get info :tool-use)))
      (funcall on-error 'no-submit "model returned empty response without calling submit_review"))

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

(defun hutch--agent (prompt on-done on-error on-progress result-box token-box cancel-box round-box max-rounds scope-id)
  "Start a gptel request with PROMPT and a callback that drives the tool-use loop.
Calls ON-DONE when RESULT-BOX is set, ON-ERROR with (type msg) on failure.
ON-PROGRESS, if non-nil, is called as (CURRENT MAX) after each tool round.
Accumulates token usage in TOKEN-BOX.  Bounded by MAX-ROUNDS."
  (hutch--trace-slice-beg "llm-call" "llm" scope-id `(:turn 1))
  (thread-last
    (gptel-request (plist-get prompt :user)
      :system (plist-get prompt :system)
      :callback (lambda (response info)
                  (hutch--agent-request response
                                        info
                                        on-done
                                        on-error
                                        on-progress
                                        result-box
                                        token-box
                                        round-box
                                        max-rounds
                                        scope-id)))
    (hutch--result-box-set cancel-box)))

(defun hutch--agent-cancel (cancel-box)
  "Cancel an in-flight review if there is an FSM handle in CANCEL-BOX."
  (when-let* ((fsm (hutch--result-box-get cancel-box)))
    (gptel--fsm-transition fsm 'ABRT)))

(defun hutch--review-on-done (scope result-box token-box callback)
  "Handle successful review for SCOPE.
Read findings from RESULT-BOX, token counts from TOKEN-BOX, call CALLBACK."
  (let* ((result       (hutch--result-box-get result-box))
         (tokens       (hutch--result-box-get token-box))
         (shash        (plist-get scope :shash))
         (scope-name   (plist-get scope :scope))
         (findings     (thread-last result
                                    (mapcar #'hutch--normalize-tool-finding)
                                    (hutch--run-gates scope))))

    (hutch--log "llm"
                "submit_review for %s: %d findings after gates"
                scope-name
                (length findings))
    (hutch--trace-instant "sanitized-findings"
                          "agent"
                          shash
                          `(:findings ,(vconcat (mapcar #'hutch--finding-for-trace findings))))

    (funcall callback (hutch--make-result :ok scope findings nil tokens))))

(defun hutch--review-on-error (scope callback type msg)
  "Handle review error for SCOPE.
Call CALLBACK with an error result built from TYPE and MSG."
  (funcall callback (hutch--make-error-result scope type msg)))

(defun hutch--review-scope (scope on-done on-progress cancel-box)
  "Review SCOPE asynchronously with tool use.
Call ON-DONE with a result plist when finished.
ON-PROGRESS, if non-nil, is called as (CURRENT MAX) after each tool round.
CANCEL-BOX is a result-box; when set non-nil the in-flight request is
aborted before its callback fires."
  (unless hutch-backend
    (error "Hutch-backend is not set"))
  (let ((scope-id   (plist-get scope :shash))
        (scope-name (symbol-name (plist-get scope :scope)))
        (scope-desc (plist-get scope :desc)))
    (hutch--trace-meta-thread-name scope-id (format "scope:%s:%s" scope-name scope-desc))
    (hutch--trace-slice-beg "review-scope" "scope" scope-id `(:s ,scope-name :d ,scope-desc))

    (let* ((result-box    (hutch--make-result-box))
           (token-box     (hutch--make-result-box))
           (round-box     (hutch--make-result-box))
           (tools         (hutch--make-tools scope result-box))
           (user-prompt   (format hutch-review-template (plist-get scope :manifest)))
           (prompt        (list :user user-prompt
                                :system (hutch-system-prompt hutch-max-tool-rounds)))
           (gptel-backend hutch-backend)
           (gptel-model   (car (gptel-backend-models hutch-backend)))
           (gptel-tools   tools)
           (on-progress   (when on-progress
                            (lambda (current max) (funcall on-progress scope current max)))))
      (hutch--agent
       prompt
       (lambda ()
         (hutch--trace-slice-end "review-scope" "scope" scope-id)

         (hutch--agent-cancel cancel-box)
         (hutch--review-on-done scope result-box token-box on-done))
       (lambda (type msg)
         (hutch--trace-slice-end "review-scope" "scope" scope-id)

         (hutch--agent-cancel cancel-box)
         (hutch--review-on-error scope on-done type msg))
       on-progress
       result-box
       token-box
       cancel-box
       round-box
       hutch-max-tool-rounds
       scope-id))))

(defun hutch--review (scopes on-done on-progress)
  "Review SCOPES with caching.
Call ON-DONE with each result plist as it arrives (cache hits included).
Call ON-PROGRESS as (SCOPE CURRENT MAX) after each tool round (uncached only).
Returns a list of cancel-callbacks to cancel reviews for SCOPES."
  (hutch--log "review" "found %d scopes" (length scopes))
  (mapcar (lambda (scope)
            (let ((hash             (plist-get scope :hash))
                  (desc             (plist-get scope :desc))
                  (scope-key        (plist-get scope :scope))
                  (cancel-scope-box (hutch--make-result-box)))
              (if-let* ((cached (hutch--cache-lookup hash)))
                  (progn
                    (hutch--log "review" "cache hit for %s %s" scope-key desc)
                    (funcall on-done cached)
                    (lambda () nil))
                (hutch--log "review" "dispatching %s %s" scope-key desc)
                (hutch--review-scope scope
                                     (hutch--write-through-cache-callback hash on-done)
                                     on-progress
                                     cancel-scope-box)
                (lambda () (hutch--agent-cancel cancel-scope-box)))))
          scopes))

(provide 'hutch-agent)

;;; hutch-agent.el ends here
