;;; zig-mode.el --- A major mode for the Zig programming language -*- lexical-binding: t -*-

;; Version: 0.0.7
;; Author: Andrea Orru <andreaorru1991@gmail.com>, Andrew Kelley <superjoe30@gmail.com>
;; Keywords: zig, languages
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/zig-lang/zig-mode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(defun zig-re-word (inner)
  "Construct a regular expression for the word INNER."
  (concat "\\<" inner "\\>"))

(defun zig-re-grab (inner)
  "Construct a group regular expression for INNER."
  (concat "\\(" inner "\\)"))

(defconst zig-re-identifier "[[:word:]_][[:word:]_[:digit:]]*")
(defconst zig-re-type-annotation
  (concat (zig-re-grab zig-re-identifier)
          "[[:space:]]*:[[:space:]]*"
          (zig-re-grab zig-re-identifier)))

(defun zig-re-definition (dtype)
  "Construct a regular expression for definitions of type DTYPE."
  (concat (zig-re-word dtype) "[[:space:]]+" (zig-re-grab zig-re-identifier)))

(defconst zig-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?= ?! ?< ?>))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 12" table)
    (modify-syntax-entry ?\n ">"    table)

    table))

(defconst zig-keywords
  '(
    ;; Storage
    "const" "var" "extern" "packed" "export" "pub" "noalias" "inline"
    "comptime" "nakedcc" "stdcallcc" "volatile" "align" "section"

    ;; Structure
    "struct" "enum" "union"

    ;; Statement
    "break" "return" "continue" "asm" "defer" "errdefer" "unreachable"
    "try" "catch" "async" "await" "suspend" "resume" "cancel"

    ;; Conditional
    "if" "else" "switch" "and" "or" "orelse"

    ;; Repeat
    "while" "for"

    ;; Other keywords
    "fn" "use" "test"))

(defconst zig-types
  '(
    ;; Integer types
    "i2" "u2" "i3" "u3" "i4" "u4" "i5" "u5" "i6" "u6" "i7" "u7" "i8" "u8"
    "i16" "u16" "i29" "u29" "i32" "u32" "i64" "u64" "i128" "u128"
    "isize" "usize"

    ;; Floating types
    "f16" "f32" "f64" "f128"

    ;; C types
    "c_short" "c_ushort" "c_int" "c_uint" "c_long" "c_ulong"
    "c_longlong" "c_ulonglong" "c_longdouble" "c_void"

    ;; Comptime types
    "comptime_int" "comptime_float"

    ;; Other types
    "bool" "void" "noreturn" "type" "error" "promise"))

(defconst zig-constants
  '(
    ;; Boolean
    "true" "false"

    ;; Other constants
    "null" "undefined" "this"))

(defgroup zig-mode nil
  "Support for Zig code."
  :link '(url-link "https://ziglang.org/")
  :group 'languages)

(defcustom zig-indent-offset 4
  "Indent Zig code by this number of spaces."
  :type 'integer
  :group 'zig-mode
  :safe #'integerp)

(defface zig-multiline-string-face
  '((t :inherit font-lock-string-face))
  "Face for multiline string literals."
  :group 'zig-mode)

(defvar zig-font-lock-keywords
  (append
   `(
     ;; Builtins (prefixed with @)
     (,(concat "@" zig-re-identifier) . font-lock-builtin-face)

     ;; Keywords, constants and types
     (,(regexp-opt zig-keywords  'symbols) . font-lock-keyword-face)
     (,(regexp-opt zig-constants 'symbols) . font-lock-constant-face)
     (,(regexp-opt zig-types     'symbols) . font-lock-type-face)

     ;; Type annotations (both variable and type)
     (,zig-re-type-annotation 1 font-lock-variable-name-face)
     (,zig-re-type-annotation 2 font-lock-type-face)
     )

   ;; Definitions
   (mapcar #'(lambda (x)
               (list (zig-re-definition (car x))
                     1 (cdr x)))
           '(("const" . font-lock-variable-name-face)
             ("var"   . font-lock-variable-name-face)
             ("fn"    . font-lock-function-name-face)))))

(defun zig-paren-nesting-level () (nth 0 (syntax-ppss)))
(defun zig-prev-open-paren-pos () (car (last (nth 9 (syntax-ppss)))))
(defun zig-currently-in-str () (nth 3 (syntax-ppss)))
(defun zig-start-of-current-str-or-comment () (nth 8 (syntax-ppss)))

(defun zig-skip-backwards-past-whitespace-and-comments ()
  (while (or
          ;; If inside a comment, jump to start of comment.
          (let ((start (zig-start-of-current-str-or-comment)))
            (and start
                 (not (zig-currently-in-str))
                 (goto-char start)))
          ;; Skip backwards past whitespace and comment end delimiters.
          (/= 0 (skip-syntax-backward " >")))))

(defun zig-mode-indent-line ()
  (interactive)
  (let ((indent-col
         (save-excursion
           (back-to-indentation)
           (let ((paren-level
                  (let ((level (zig-paren-nesting-level)))
                    (if (looking-at "[]})]") (1- level) level))))
             (+ (if (<= paren-level 0)
                    0
                  (or (save-excursion
                        (goto-char (1+ (zig-prev-open-paren-pos)))
                        (and (not (looking-at "\n"))
                             (current-column)))
                      (* zig-indent-offset paren-level)))
                (if (and
                     (not (looking-at ";"))
                     (save-excursion
                       (zig-skip-backwards-past-whitespace-and-comments)
                       (when (> (point) 1)
                         (backward-char)
                         (not (looking-at "[,;([{}]")))))
                     zig-indent-offset 0))))))
    (if (<= (current-column) (current-indentation))
        (indent-line-to indent-col)
      (save-excursion (indent-line-to indent-col)))))

(defun zig-syntax-propertize-newline-if-in-multiline-str (end)
  (when (and (zig-currently-in-str)
             (save-excursion
               (goto-char (zig-start-of-current-str-or-comment))
               (looking-at "\\\\\\\\"))
             (re-search-forward "\n" end t))
    (put-text-property (match-beginning 0) (match-end 0)
                       'syntax-table (string-to-syntax "|"))
    (goto-char (match-end 0))))

(defun zig-syntax-propertize (start end)
  (goto-char start)
  (zig-syntax-propertize-newline-if-in-multiline-str end)
  (funcall
   (syntax-propertize-rules
    ;; Multiline strings
    ("\\(\\\\\\)\\\\"
     (1 (prog1 "|"
	  (goto-char (match-end 0))
	  (zig-syntax-propertize-newline-if-in-multiline-str end)))))
   (point) end))

(defun zig-mode-syntactic-face-function (state)
  (if (nth 3 state)
      (save-excursion
        (goto-char (nth 8 state))
        (if (looking-at "\\\\\\\\")
            'zig-multiline-string-face
          'font-lock-string-face))
    (save-excursion
      (goto-char (nth 8 state))
      (if (looking-at "///[^/]")
          'font-lock-doc-face
        'font-lock-comment-face))))

;;;###autoload
(define-derived-mode zig-mode prog-mode "Zig"
  "A major mode for the Zig programming language."
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local indent-line-function 'zig-mode-indent-line)
  (setq-local indent-tabs-mode nil)  ; Zig forbids tab characters.
  (setq-local syntax-propertize-function 'zig-syntax-propertize)
  (setq font-lock-defaults '(zig-font-lock-keywords
                             nil nil nil nil
                             (font-lock-syntactic-face-function
                              . zig-mode-syntactic-face-function))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))

(provide 'zig-mode)
;;; zig-mode.el ends here
