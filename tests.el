;; Tests for zig-mode.

(require 'ert)
(require 'zig-mode)
(require 'imenu)

;;===========================================================================;;
;; Font lock tests

(defun zig-test-font-lock (code expected)
  (let* ((fontified-code
          (with-temp-buffer
            (zig-mode)
            (insert code)
            (font-lock-fontify-buffer)
            (buffer-string)))
         (start 0)
         (actual '()))
    (while start
      (let* ((end (next-single-property-change start 'face fontified-code))
             (substring (substring-no-properties fontified-code start end))
             (face (get-text-property start 'face fontified-code)))
        (when face
          (setq actual (cons (list substring face) actual)))
        (setq start end)))
    (should (equal expected (reverse actual)))))

(ert-deftest test-font-lock-backslash-in-char-literal ()
  (zig-test-font-lock
   "const escaped = '\\'';"
   '(("const" font-lock-keyword-face)
     ("escaped" font-lock-variable-name-face)
     ("'\\''" font-lock-string-face))))

(ert-deftest test-font-lock-backslash-in-multiline-str-literal ()
  (zig-test-font-lock
   "
const string =
    \\\\ This newline is NOT escaped \\
;"
   '(("const" font-lock-keyword-face)
     ("string" font-lock-variable-name-face)
     ("\\\\ This newline is NOT escaped \\\n" zig-multiline-string-face))))

(ert-deftest test-font-lock-backslash-in-str-literal ()
  (zig-test-font-lock
   "\"This quote \\\" is escaped\""
   '(("\"This quote \\\" is escaped\"" font-lock-string-face))))

(ert-deftest test-font-lock-builtins ()
  (zig-test-font-lock
   "const std = @import(\"std\");"
   '(("const" font-lock-keyword-face)
     ("std" font-lock-variable-name-face)
     ("@import" font-lock-builtin-face)
     ("\"std\"" font-lock-string-face))))

(ert-deftest test-font-lock-comments ()
  (zig-test-font-lock
   "
// This is a normal comment\n
/// This is a doc comment\n
//// This is a normal comment again\n"
   '(("// This is a normal comment\n" font-lock-comment-face)
     ("/// This is a doc comment\n" font-lock-doc-face)
     ("//// This is a normal comment again\n" font-lock-comment-face))))

(ert-deftest test-font-lock-decl-const ()
  (zig-test-font-lock
   "const greeting = \"Hello, world!\";"
   '(("const" font-lock-keyword-face)
     ("greeting" font-lock-variable-name-face)
     ("\"Hello, world!\"" font-lock-string-face))))

(ert-deftest test-font-lock-decl-fn ()
  (zig-test-font-lock
   "fn plus1(value: u32) u32 { return value + 1; }"
   '(("fn" font-lock-keyword-face)
     ("plus1" font-lock-function-name-face)
     ("value" font-lock-variable-name-face)
     ("u32" font-lock-type-face)
     ("u32" font-lock-type-face)
     ("return" font-lock-keyword-face))))

(ert-deftest test-font-lock-decl-var ()
  (zig-test-font-lock
   "var finished = false;"
   '(("var" font-lock-keyword-face)
     ("finished" font-lock-variable-name-face)
     ("false" font-lock-constant-face))))

(ert-deftest test-font-lock-multiline-str-literal ()
  (zig-test-font-lock
   "
const python =
    \\\\def main():
    \\\\    print(\"Hello, world!\")
;"
   '(("const" font-lock-keyword-face)
     ("python" font-lock-variable-name-face)
     ("\\\\def main():\n" zig-multiline-string-face)
     ("\\\\    print(\"Hello, world!\")\n" zig-multiline-string-face))))

(ert-deftest test-font-lock-parameters-pointers-and-arrays ()
  (zig-test-font-lock
   "fn doSomething(thingPtr: *Thing, string: []const u8, maybeFoo: ?Foo) void {}"
   '(("fn" font-lock-keyword-face)
     ("doSomething" font-lock-function-name-face)
     ("thingPtr" font-lock-variable-name-face)
     ("Thing" font-lock-type-face)
     ("string" font-lock-variable-name-face)
     ("const" font-lock-keyword-face)
     ("u8" font-lock-type-face)
     ("maybeFoo" font-lock-variable-name-face)
     ("Foo" font-lock-type-face)
     ("void" font-lock-type-face)
     )))

;; Test all permutations of '?', '*', '[]', '* const', and '[] const' for 3 of those in a row
;; For example, ??[]Bar or [][]const *Bar
(ert-deftest test-font-lock-parameters-optionals-pointers-and-arrays ()
  (dotimes (i (* 5 5 5))
    (let* ((int-to-opt-ptr-array (lambda (x)
                                   (pcase x (0 "?") (1 "*") (2 "[]") (3 "*const ") (4 "[]const "))))
           (first-symbol-int (/ i (* 5 5)))
           (second-symbol-int (/ (% i (* 5 5)) 5))
           (third-symbol-int (% i 5))
           (first-symbol-const (>= first-symbol-int 3))
           (second-symbol-const (>= second-symbol-int 3))
           (third-symbol-const (>= third-symbol-int 3))
           (first-symbol (funcall int-to-opt-ptr-array first-symbol-int))
           (second-symbol (funcall int-to-opt-ptr-array second-symbol-int))
           (third-symbol (funcall int-to-opt-ptr-array third-symbol-int))
           (test-string (concat "fn foo(bar: " first-symbol second-symbol third-symbol "Bar) void {}"))
           (expected (append '(("fn" font-lock-keyword-face)
                               ("foo" font-lock-function-name-face)
                               ("bar" font-lock-variable-name-face))
                             (if first-symbol-const '(("const" font-lock-keyword-face)) nil)
                             (if second-symbol-const '(("const" font-lock-keyword-face)) nil)
                             (if third-symbol-const '(("const" font-lock-keyword-face)) nil)
                             '(("Bar" font-lock-type-face)
                               ("void" font-lock-type-face)))))
      (zig-test-font-lock test-string expected))))

;;===========================================================================;;
;; Indentation tests

(defun zig-test-indent-line (line-number original expected-line)
  (with-temp-buffer
    (zig-mode)
    (insert original)
    (goto-line line-number)
    (indent-for-tab-command)
    (let* ((current-line (thing-at-point 'line t))
           (stripped-line (replace-regexp-in-string "\n\\'" "" current-line)))
      (should (equal expected-line stripped-line)))))

(ert-deftest test-indent-from-current-block ()
  (zig-test-indent-line
   6
   "
{
  // Normally, zig-mode indents to 4, but suppose
  // someone indented this part to 2 for some reason.
  {
    // This line should get indented to 6, not 8.
  }
}"
   "      // This line should get indented to 6, not 8."))

(defun zig-test-indent-region (original expected)
  (with-temp-buffer
    (zig-mode)
    (insert original)
    (indent-region 1 (+ 1 (buffer-size)))
    (should (equal expected (buffer-string)))))

(ert-deftest test-indent-top-level ()
  (zig-test-indent-region
   "  const four = 4;"
   "const four = 4;"))

(ert-deftest test-indent-fn-def-body ()
  (zig-test-indent-region
   "
pub fn plus1(value: u32) u32 {
return value + 1;
}"
   "
pub fn plus1(value: u32) u32 {
    return value + 1;
}"))

(ert-deftest test-indent-fn-def-args ()
  (zig-test-indent-region
   "
pub fn add(value1: u32,
value2: u32) u32 {
return value1 + value2;
}"
   "
pub fn add(value1: u32,
           value2: u32) u32 {
    return value1 + value2;
}"))

(ert-deftest test-indent-fn-call-args ()
  (zig-test-indent-region
   "
blarg(foo,
foo + bar + baz +
quux,
quux);"
   "
blarg(foo,
      foo + bar + baz +
          quux,
      quux);"))

(ert-deftest test-indent-if-else ()
  (zig-test-indent-region
   "
fn sign(value: i32) i32 {
if (value > 0) return 1;
else if (value < 0) {
return -1;
} else {
return 0;
}
}"
   "
fn sign(value: i32) i32 {
    if (value > 0) return 1;
    else if (value < 0) {
        return -1;
    } else {
        return 0;
    }
}"))

(ert-deftest test-indent-struct ()
  (zig-test-indent-region
   "
const Point = struct {
x: f32,
y: f32,
};
const origin = Point {
.x = 0.0,
.y = 0.0,
};"
   "
const Point = struct {
    x: f32,
    y: f32,
};
const origin = Point {
    .x = 0.0,
    .y = 0.0,
};"))

(ert-deftest test-indent-multiline-str-literal ()
  (zig-test-indent-region
   "
const code =
\\\\const foo = []u32{
\\\\    12345,
\\\\};
;"
   "
const code =
    \\\\const foo = []u32{
    \\\\    12345,
    \\\\};
;"))

(ert-deftest test-indent-array-literal-1 ()
  (zig-test-indent-region
   "
const msgs = [][]u8{
\"hello\",
\"goodbye\",
};"
   "
const msgs = [][]u8{
    \"hello\",
    \"goodbye\",
};"))

(ert-deftest test-indent-array-literal-2 ()
  (zig-test-indent-region
   "
const msg = []u8{'h', 'e', 'l', 'l', 'o',
'w', 'o', 'r', 'l', 'd'};"
   "
const msg = []u8{'h', 'e', 'l', 'l', 'o',
                 'w', 'o', 'r', 'l', 'd'};"))

(ert-deftest test-indent-paren-block ()
  (zig-test-indent-region
   "
const foo = (
some_very_long + expression_that_is * set_off_in_parens
);"
   "
const foo = (
    some_very_long + expression_that_is * set_off_in_parens
);"))

(ert-deftest test-indent-double-paren-block ()
  (zig-test-indent-region
   "
const foo = ((
this_expression_is + set_off_in_double_parens * for_some_reason
));"
   "
const foo = ((
    this_expression_is + set_off_in_double_parens * for_some_reason
));"))

(ert-deftest test-indent-with-comment-after-open-brace ()
  (zig-test-indent-region
   "
if (false) { // This comment shouldn't mess anything up.
launchTheMissiles();
}"
   "
if (false) { // This comment shouldn't mess anything up.
    launchTheMissiles();
}"))

;;===========================================================================;;
;; Imenu tests

;; taken from rust-mode
(defun test-imenu (code expected-items)
  (with-temp-buffer
	(zig-mode)
	(insert code)
	(let ((actual-items
		   ;; Replace ("item" . #<marker at ? in ?.el) with "item"
		   (mapcar (lambda (class)
					 (cons (car class)
						   (mapcar #'car (cdr class))))
				   (imenu--generic-function zig-imenu-generic-expression))))
	  (should (equal expected-items actual-items)))))


(ert-deftest test-imenu-struct ()
  (test-imenu
   "
pub const Foo = struct {};
pub const Bar = extern struct {};
const FooBar = struct {};
"
   '(("Struct"
	  "Foo"
	  "Bar"
	  "FooBar"))))

(ert-deftest test-imenu-enum ()
  (test-imenu
   "
pub const Foo = enum {};
const FooBarError = enum {};
"
   '(("Enum"
	  "Foo"
	  "FooBarError"))))

(ert-deftest test-imenu-enum ()
  (test-imenu
   "
pub const Foo = enum {};
const FooBarError = enum {};
"
   '(("Enum"
	  "Foo"
	  "FooBarError"))))

(ert-deftest test-imenu-all ()
  (test-imenu
   "
const Foo = struct {
	pub fn init() void {}
};

const FooError = enum {};

pub fn main() void {
}
"
   '(("Fn" "init" "main")
	 ("Struct" "Foo")
	 ("Enum" "FooError"))))


;;===========================================================================;;
