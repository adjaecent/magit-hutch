;;; magit-hutch-treesit.el --- Find enclosing definitions with tree-sitter -*- lexical-binding: t; -*-

;;; Commentary:

;; Language-agnostic tree-sitter utility for finding the enclosing
;; definition (function, method, class, etc.) for a given line in a file.

;;; Code:

(require 'treesit)

(defvar hutch--treesit-lang-alist
  '(("el" . elisp) ("py" . python) ("rb" . ruby) ("rs" . rust)
    ("js" . javascript) ("ts" . typescript) ("tsx" . tsx)
    ("go" . go) ("java" . java) ("c" . c) ("cpp" . cpp)
    ("clj" . clojure) ("ex" . elixir) ("exs" . elixir))
  "Map file extensions to tree-sitter language symbols.")

(defvar hutch--treesit-definition-types
  '(;; Common
    "function_definition" "function_declaration"
    "method_definition" "method_declaration"
    "class_definition" "class_declaration"
    "module_definition"
    ;; Ruby
    "method" "class" "module"
    ;; JS/TS
    "arrow_function" "generator_function" "generator_function_declaration"
    "interface_declaration" "enum_declaration" "type_alias_declaration"
    ;; Python
    "lambda"
    ;; Go
    "type_declaration" "type_spec"
    ;; Rust
    "function_item" "impl_item" "struct_item"
    "trait_item" "enum_item" "type_item"
    ;; C/C++
    "struct_specifier" "class_specifier" "namespace_definition"
    ;; Elisp
    "defun" "special_form"
    ;; Elixir
    "anonymous_function"
    ;; Clojure (all def forms are list_lit)
    "list_lit")
  "Tree-sitter node types that represent definitions.")

(defun hutch--treesit-lang-for-file (path)
  "Return the tree-sitter language symbol for PATH, or nil."
  (let ((ext (file-name-extension path)))
    (alist-get ext hutch--treesit-lang-alist nil nil #'equal)))

(defun hutch--treesit-format-node (def)
  "Format a definition node DEF as \"Lines N-M:\\ntext\"."
  (let* ((start-line (line-number-at-pos (treesit-node-start def)))
         (text (treesit-node-text def)))
    (format "Lines %d-%d:\n%s"
            start-line
            (+ start-line (length (split-string text "\n")) -1)
            text)))

(defun hutch--treesit-enclosing-parents (node parents found max-depth)
  "Walk up from NODE collecting parents definitions into PARENTS.
FOUND tracks how many definitions collected so far.  Stop at MAX-DEPTH.
Returns parents in outermost-first order.  `nreverse` to fetch it innermost-first."
  (cond
   ((or (null node) (>= found max-depth))
    parents)

   ((member (treesit-node-type node) hutch--treesit-definition-types)
    (hutch--treesit-enclosing-parents
     (treesit-node-parent node)
     (cons node parents)
     (+ found 1)
     max-depth))

   (t
    (hutch--treesit-enclosing-parents
     (treesit-node-parent node)
     parents
     found
     max-depth))))

(defun hutch--treesit-enclosing-definition (lang full-path line &optional depth)
  "Find enclosing definitions at LINE in FULL-PATH for LANG.
Returns up to DEPTH definitions (default 1).
Returns nil if none found."
  (when (treesit-language-available-p lang)
    (with-temp-buffer
      (insert-file-contents full-path)
      (let* ((parser (treesit-parser-create lang))
             (pos (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- line))
                    (point)))
             (node (treesit-node-at pos))
             (defs (nreverse
                    (hutch--treesit-enclosing-parents node nil 0 (min (or depth 1) 10)))))
        (when defs
          (mapconcat #'hutch--treesit-format-node defs "\n\n"))))))

(provide 'magit-hutch-treesit)

;;; magit-hutch-treesit.el ends here
