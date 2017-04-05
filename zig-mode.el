;;; zig-mode.el --- A major mode for the Zig programming language -*- lexical-binding: t -*-

;; Version: 0.0.1
;; Author: Andrea Orru <andreaorru1991@gmail.com>, Andrew Kelley <superjoe30@gmail.com>
;; Keywords: zig, languages
;; Package-Requires: ((emacs "24.0"))
;; URL: https://github.com/AndreaOrru/zig-mode

;;; Commentary:
;;

;;; Code:
(defvar zig--builtins
  '("@addWithOverflow"
    "@mulWithOverflow"
    "@shlWithOverflow"
    "@subWithOverflow"
    "@divExact"

    "@cDefine" "@cImport" "@cInclude" "@cUndef"
    "@setGlobalAlign" "@setGlobalLinkage" "@setGlobalSection"
    "@setDebugSafety"

    "@alignOf" "@sizeOf"
    "@typeName" "@typeOf"
    "@intType" "@isFloat" "@isInteger"
    "@maxValue" "@minValue"
    "@memberCount"
    "@canImplicitCast"

    "@alloca"
    "@memcpy" "@memset"

    "@compileError" "@compileLog" "@compileVar"
    "@errorName"

    "@import" "@embedFile"
    "@clz"
    "@cmpxchg"
    "@ctz"
    "@fence"
    "@generatedCode"
    "@truncate"
    "@unreachable"))

(defvar zig--keywords
  '("asm"
    "noalias"
    "unreachable"
    "use"

    "coldcc" "nakedcc"
    "export" "extern" "inline" "pub"
    "fn"

    "enum" "struct" "union"
    "packed"
    "comptime"
    "const" "var"
    "volatile"

    "try"
    "defer"

    "if" "else"
    "for" "while"
    "goto" "break" "continue"
    "switch"
    "return"))

(defvar zig--constants
  '("null"
    "this"
    "true" "false"
    "undefined"))

(defvar zig--types
  '("Unreachable"
    "error"
    "type"

    "bool"
    "c_int" "c_long" "c_long_double" "c_longlong" "c_short"
    "c_uint" "c_ulong" "c_ulonglong" "c_ushort"

    "i8" "i16" "i32" "i64" "isize"
    "u8" "u16" "u32" "u64" "usize"
    "f32" "f64"

    "noreturn"
    "void"))

(defvar zig--font-lock-keywords
  `((,(regexp-opt zig--keywords  'symbols) . font-lock-keyword-face)
    (,(regexp-opt zig--builtins          ) . font-lock-builtin-face)
    (,(regexp-opt zig--constants 'symbols) . font-lock-constant-face)
    (,(regexp-opt zig--types     'symbols) . font-lock-type-face)))


(defvar zig-mode-init-hook nil
  "This hook is called when 'zig-mode' is initialized.")

;; Indentation is 4 spaces by default:
(add-hook 'zig-mode-init-hook '(lambda() (setq-local c-basic-offset 4)))

;;;###autoload
(define-derived-mode zig-mode c-mode "Zig"
  "A major mode for the Zig programming language."
  (run-hooks 'zig-mode-init-hook)
  (setq-local font-lock-defaults '(zig--font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))

(provide 'zig-mode)
;;; zig-mode.el ends here
