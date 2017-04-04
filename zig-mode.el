;;; zig-mode.el --- A major mode for the Zig programming language -*- lexical-binding: t -*-

;; Version: 0.0.1
;; Author: Andrea Orru <andreaorru1991@gmail.com>, Andrew Kelley <superjoe30@gmail.com>
;; Keywords: zig, languages
;; Package-Requires: ((emacs "24.0"))
;; URL: https://github.com/AndreaOrru/zig-mode

;;; Commentary:
;;

;;; Code:
(defvar zig-mode-builtins
  '("@addWithOverflow"
    "@alignOf"
    "@alloca"
    "@cDefine"
    "@cImport"
    "@cInclude"
    "@cUndef"
    "@canImplicitCast"
    "@clz"
    "@cmpxchg"
    "@compileError"
    "@compileLog"
    "@compileVar"
    "@ctz"
    "@divExact"
    "@embedFile"
    "@errorName"
    "@fence"
    "@generatedCode"
    "@import"
    "@intType"
    "@isFloat"
    "@isInteger"
    "@maxValue"
    "@memberCount"
    "@minValue"
    "@mulWithOverflow"
    "@setDebugSafety"
    "@setGlobalAlign"
    "@setGlobalLinkage"
    "@setGlobalSection"
    "@shlWithOverflow"
    "@sizeOf"
    "@subWithOverflow"
    "@truncate"
    "@typeName"
    "@typeOf"
    "@unreachable"))

(defvar zig-mode-keywords
  '("asm"
    "break"
    "coldcc"
    "comptime"
    "const"
    "continue"
    "defer"
    "else"
    "enum"
    "export"
    "extern"
    "fn"
    "for"
    "goto"
    "if"
    "inline"
    "nakedcc"
    "noalias"
    "packed"
    "pub"
    "return"
    "struct"
    "switch"
    "try"
    "union"
    "unreachable"
    "use"
    "var"
    "volatile"
    "while"))

(defvar zig-mode-constants
  '("null"
    "this"
    "undefined"))

(defvar zig-mode-types
  '("Unreachable"
    "bool"
    "c_int"
    "c_long"
    "c_long_double"
    "c_longlong"
    "c_short"
    "c_uint"
    "c_ulong"
    "c_ulonglong"
    "c_ushort"
    "error"
    "f32"
    "f64"
    "i16"
    "i32"
    "i64"
    "i8"
    "isize"
    "type"
    "u16"
    "u32"
    "u64"
    "u8"
    "usize"
    "void"
    ))

(defvar zig-mode-font-lock-keywords
  `((,(regexp-opt zig-mode-keywords)  . font-lock-keyword-face)
    (,(regexp-opt zig-mode-builtins)  . font-lock-builtin-face)
    (,(regexp-opt zig-mode-constants) . font-lock-constant-face)
    (,(regexp-opt zig-mode-types)     . font-lock-type-face)))

;;;###autoload
(define-derived-mode zig-mode c-mode "Zig"
  "A major mode for the Zig programming language."
  :group 'zig-mode
  (setq-local font-lock-defaults '(zig-mode-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))

(provide 'zig-mode)
;;; zig-mode.el ends here
