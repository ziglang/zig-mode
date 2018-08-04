;; Tests for zig-mode.

(require 'ert)
(require 'zig-mode)

;;===========================================================================;;
;; Indentation tests

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

;;===========================================================================;;
