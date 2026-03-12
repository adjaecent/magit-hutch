;;; surrounding-context-test.el --- Test tree-sitter enclosing definitions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(let ((hutch-dir (file-name-directory
                  (directory-file-name
                   (file-name-directory load-file-name)))))
  (add-to-list 'load-path hutch-dir)
  (require 'treesit-context))

(defvar sct--fixtures-dir
  (expand-file-name "fixtures"
                    (file-name-directory load-file-name))
  "Path to test fixture files.")

(defun sct--fixture (name)
  "Return full path to fixture NAME."
  (expand-file-name name sct--fixtures-dir))

(defun sct--result (lang file line)
  "Run enclosing definition lookup for LANG on FILE at LINE."
  (treesit-context-enclosing-definition lang (sct--fixture file) line))

(defun sct--assert-contains (result expected-substring)
  "Assert RESULT is non-nil and contains EXPECTED-SUBSTRING."
  (should result)
  (should (string-match-p (regexp-quote expected-substring) result)))

(defun sct--assert-nil (result)
  "Assert RESULT is nil (no enclosing definition found)."
  (should (null result)))

;;; --- Python ---

(ert-deftest sct-python-standalone-function ()
  "Line inside a standalone function should find it."
  (sct--assert-contains (sct--result 'python "sample.py" 6)
                        "def helper"))

(ert-deftest sct-python-method ()
  "Line inside a class method should find the method."
  (sct--assert-contains (sct--result 'python "sample.py" 14)
                        "def find_user"))

(ert-deftest sct-python-class ()
  "Line on the class definition should find the class."
  (sct--assert-contains (sct--result 'python "sample.py" 9)
                        "class UserService"))

(ert-deftest sct-python-top-level ()
  "Line at module level (not in a function) should return nil."
  (sct--assert-nil (sct--result 'python "sample.py" 3)))

;;; --- JavaScript ---

(ert-deftest sct-js-function-declaration ()
  "Line inside a function declaration."
  (sct--assert-contains (sct--result 'javascript "sample.js" 4)
                        "function greet"))

(ert-deftest sct-js-method ()
  "Line inside a class method."
  (sct--assert-contains (sct--result 'javascript "sample.js" 14)
                        "addRoute"))

(ert-deftest sct-js-nested-inner ()
  "Line inside a nested function should find the inner one."
  (sct--assert-contains (sct--result 'javascript "sample.js" 24)
                        "function inner"))

(ert-deftest sct-js-top-level ()
  "Line at top level should return nil."
  (sct--assert-nil (sct--result 'javascript "sample.js" 1)))

;;; --- Go ---

(ert-deftest sct-go-method ()
  "Line inside a method receiver."
  (sct--assert-contains (sct--result 'go "sample.go" 12)
                        "func (s *Server) Start"))

(ert-deftest sct-go-function ()
  "Line inside a standalone function."
  (sct--assert-contains (sct--result 'go "sample.go" 22)
                        "func helper"))

(ert-deftest sct-go-top-level ()
  "Line on package declaration should return nil."
  (sct--assert-nil (sct--result 'go "sample.go" 1)))

;;; --- Ruby ---

(ert-deftest sct-ruby-method ()
  "Line inside a method."
  (sct--assert-contains (sct--result 'ruby "sample.rb" 8)
                        "def verify"))

(ert-deftest sct-ruby-class ()
  "Line on the class definition."
  (sct--assert-contains (sct--result 'ruby "sample.rb" 2)
                        "class Authenticator"))

(ert-deftest sct-ruby-standalone ()
  "Line inside a standalone function."
  (sct--assert-contains (sct--result 'ruby "sample.rb" 21)
                        "def standalone_func"))

;;; --- C ---

(ert-deftest sct-c-function ()
  "Line inside a function."
  (sct--assert-contains (sct--result 'c "sample.c" 10)
                        "int add"))

(ert-deftest sct-c-main ()
  "Line inside main."
  (sct--assert-contains (sct--result 'c "sample.c" 19)
                        "int main"))

(ert-deftest sct-c-top-level ()
  "Line on an include should return nil."
  (sct--assert-nil (sct--result 'c "sample.c" 1)))

;;; --- Rust ---

(ert-deftest sct-rust-impl-method ()
  "Line inside an impl method."
  (sct--assert-contains (sct--result 'rust "sample.rs" 9)
                        "fn new"))

(ert-deftest sct-rust-standalone-fn ()
  "Line inside a standalone function."
  (sct--assert-contains (sct--result 'rust "sample.rs" 22)
                        "fn process"))

(ert-deftest sct-rust-top-level ()
  "Line on struct definition should return nil."
  (sct--assert-nil (sct--result 'rust "sample.rs" 1)))

;;; surrounding-context-test.el ends here
