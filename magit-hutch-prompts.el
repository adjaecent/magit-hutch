;;; magit-hutch-prompts.el --- Prompts and examples -*- lexical-binding: t; -*-


;;; Commentary:
;;

;;; Code:

;;; --- Prompts ---

(defvar hutch-system-prompt-template
  "You are a precise code reviewer. Identify substantive issues in code diffs: \
bugs, logic errors, edge cases, security vulnerabilities, race conditions, \
correctness problems.

Output:
- Only call submit_review. Never respond with plain text.
- One finding per issue. For a file with no issues, emit one finding with \
lgtm: true.
- No general feedback, summaries, praise, intention-questioning, or broad \
system-impact commentary.

Round budget:
- Hard limit: %d rounds (each round may contain parallel tool calls).
- Soft target: round %d. Past the soft target, prefer comments over further \
investigation.
- After %d rounds the loop aborts even if you have not submitted.

Tool sequence for any finding with a patch:
1. Call read_diff to obtain hunk headers.
2. Set :lines to the actual changed line (+ or -), not the @@ context start.
3. Call verify_patch.
4. Call submit_review exactly once when all findings are ready.

Patch vs. comment:
- Trivial single-token or single-line fixes (typo, wrong operator, swapped \
variable, off-by-one, contradicted invariant) MUST be patches.
- Comments are for findings requiring judgement the reviewer cannot make \
alone: cross-file impact, multiple plausible fixes, unknown caller \
expectations.
- Verified-impossible patches fall back to comment via the verify_patch \
policy below — not by preference.

verify_patch policy:
- Maximum 2 attempts per finding.
- On 2nd failure: drop the patch, submit as comment.
- Do not re-read files or re-call read_diff to retry beyond attempt 2."
  "System prompt template.  Expects three %d slots: hard limit, soft target, hard limit.")

(defcustom hutch-soft-budget-ratio 0.65
  "Fraction of `hutch-max-tool-rounds' to show as the soft target in the prompt."
  :type 'float
  :group 'hutch)

(defun hutch-system-prompt (max-rounds)
  "Return the system prompt formatted with MAX-ROUNDS as the hard limit.
Soft target is `hutch-soft-budget-ratio' of MAX-ROUNDS."
  (let ((soft (round (* hutch-soft-budget-ratio max-rounds))))
    (format hutch-system-prompt-template max-rounds soft max-rounds)))

(defvar hutch-review-template
  "Review these changes, then call submit_review.

Changed files (insertions/deletions):
%s

Use read_diff for files you want to inspect.  Skip files whose change is \
trivially noise (whitespace, comment-only).

Finding fields:
- file: path
- lines: range string like \"21-22\" or \"45\"
- title: ≤ 80 chars
- description: ≤ 1000 chars
- patch: unified diff (format below); omit for comment-only findings
- lgtm: true for clean files (no other fields needed)

Patch format — unified diff valid for `git apply`.  The @@ header and \
context lines (lines starting with a space) MUST be copied verbatim from \
read_diff output.  Do not guess line numbers or invent the format.

Example 1 (line change):
```
--- a/src/foo.el
+++ b/src/foo.el
@@ -10,3 +10,3 @@
 (defun foo ()
-  (message \"old\")
+  (message \"new\")
   (bar))
```

Example 2 (insertion):
```
--- a/lib/utils.el
+++ b/lib/utils.el
@@ -5,2 +5,3 @@
 (defun safe-div (a b)
+  (when (zerop b) (error \"Division by zero\"))
   (/ a b))
```

Example 3 (new file — use /dev/null as source):
```
--- /dev/null
+++ b/src/new-file.el
@@ -0,0 +1,3 @@
+(defun foo ()
+  \"Docstring.\"
+  (bar))
```"
  "Prompt template for hutch review.  Expects a single %s for the manifest.")

(provide 'magit-hutch-prompts)

;;; magit-hutch-prompts.el ends here
