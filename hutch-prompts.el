;;; hutch-prompts.el --- Prompts and examples -*- lexical-binding: t; -*-

;;; Code:

;;; --- Prompts ---

(defvar hutch-system-prompt
  "You are a precise code reviewer. Your job is to identify substantive issues \
in code diffs: bugs, logic errors, edge cases, security vulnerabilities, race \
conditions, and correctness problems.

Rules:
- Do NOT provide general feedback, summaries, explanations of changes, or praise.
- Focus solely on specific, objective issues based on the diff context.
- Do not make broad comments about potential system impacts or question intentions.
- If a file has no issues, respond with a single entry with lgtm: true for that file."
  "System prompt for hutch code review.")

(defvar hutch-review-template
  "Review this diff and respond with a JSON array of findings.

Each element must have these fields:
- \"file\": the file path
- \"lines\": a string like \"21-22\" or \"45\" for the line range in the new file
- \"title\": short issue title
- \"description\": explanation of the issue
- \"patch\": a complete unified diff patch that can be applied with `git apply`, or null if no fix
- \"lgtm\": boolean, true only if the file has no issues

The patch field MUST be a valid unified diff with --- a/ and +++ b/ headers and @@ hunk headers.
Use the correct line numbers from the original diff context.

If a file is clean, include one entry: {\"file\": \"path\", \"lgtm\": true}

Diff to review:

%s"
  "Prompt template for hutch review. Expects a single %s for the diff.")

(defconst hutch--review-example
  (cons "Review this diff:

--- a/utils.py
+++ b/utils.py
@@ -20,3 +20,3 @@
 def add(x, y):
-    retrn x + y
+    return x + y

--- a/config.py
+++ b/config.py
@@ -1,3 +1,3 @@
 DEBUG = True"
        "[{\"file\": \"utils.py\", \"lines\": \"21-22\", \"title\": \"Typo causes NameError\", \
\"description\": \"retrn is not a valid Python keyword.\", \
\"patch\": \"--- a/utils.py\\n+++ b/utils.py\\n@@ -20,3 +20,3 @@\\n def add(x, y):\\n-    retrn x + y\\n+    return x + y\", \
\"lgtm\": false}, {\"file\": \"config.py\", \"lgtm\": true}]")
  "Few-shot example for hutch review. Cons of (input . expected-output).")

(provide 'hutch-prompts)

;;; hutch-prompts.el ends here
