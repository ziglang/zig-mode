;;; zig-ts-mode.el --- A tree-sitter enabled major mode for the Zig programming language -*- lexical-binding: t -*-

;; Version: 0.0.8
;; Author: Nan Zhong <me@nanzho.ng>
;; Keywords: zig, languages, tree-sitter
;; Package-Requires: ((emacs "29.1") (reformatter "0.6"))
;; Homepage: https://github.com/zig-lang/zig-mode

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

;; A tree-sitter enabled major mode for the Zig programming languages.

;; See documentation on https://github.com/zig-lang/zig-mode

;;; Code:

(require 'zig-base-mode)
(eval-when-compile (require 'rx))

(defvar zig-ts-font-lock-rules
  `(:language zig
    :override t
    :feature comment
    (;; doc
     [(container_doc_comment)
      (doc_comment)] @font-lock-comment-face

      ;; line
     (line_comment) @font-lock-comment-face)

    :language zig
    :override t
    :feature type
    (;; TitleCase
     ([variable_type_function: (IDENTIFIER)
       field_access: (IDENTIFIER)
       parameter: (IDENTIFIER)] @type
      ;; "^[A-Z]([a-z]+[A-Za-z0-9]*)+$"
      (:match ,(rx string-start
                   (char "A-Z")
                   (1+ (1+ (char "a-z"))
                       (0+ (char "A-Za-z0-9")))
                   string-end)
              @type)) @font-lock-type-face)

    :language zig
    :override t
    :feature constant
    (field_constant: (IDENTIFIER) @font-lock-constant-face
     ;; ALL_CAPS
     ([variable_type_function: (IDENTIFIER)
       field_access: (IDENTIFIER)] @constant
      ;; "^[A-Z][A-Z_0-9]+$"
      (:match ,(rx string-start
                   (char "A-Z")
                   (1+ (char "A-Z_0-9"))
                   string-end)
              @constant)) @font-lock-constant-face

     [,@zig-constants] @font-lock-constant-face)

    :language zig
    :override t
    :feature builtin
    ((BuildinTypeExpr) @font-lock-builtin-face
     (BUILTINIDENTIFIER) @font-lock-builtin-face

     ;; _
     ((IDENTIFIER) @builtin
      (:equal @builtin "_")) @font-lock-builtin-face

      ;; C Pointers [*c]T
      (PtrTypeStart "c") @font-lock-builtin-face)

    :language zig
    :override t
    :feature variable
    ([variable: (IDENTIFIER)
      variable_type_function: (IDENTIFIER)] @font-lock-variable-name-face

     parameter: (IDENTIFIER) @font-lock-variable-name-face

     [field_member: (IDENTIFIER)
      field_access: (IDENTIFIER)] @font-lock-property-name-face)

    :language zig
    :override t
    :feature function
    ([function_call: (IDENTIFIER)
      function: (IDENTIFIER)] @font-lock-function-name-face

     ;; camelCase
     ([variable_type_function: (IDENTIFIER)
      field_access: (IDENTIFIER)
      parameter: (IDENTIFIER)] @function
      ;; "^[a-z]+([A-Z][a-z0-9]*)+$"
      (:match ,(rx string-start
                   (1+ (char "a-z"))
                   (1+ (char "A-Z")
                       (0+ (char "a-z0-9")))
                   string-end)
              @function))
     @font-lock-function-name-face)

    :language zig
    :override t
    :feature keyword
    (((IDENTIFIER) @builtin
      (:equal @builtin "_")) @font-lock-builtin-face

     ((BUILTINIDENTIFIER) @keyword
      (:equal @keyword "@import")) @font-lock-keyword-face

     ((BUILTINIDENTIFIER) @keyword
      (:equal @keyword "@cImport")) @font-lock-keyword-face

     exception: "!" @font-lock-keyword-face
     (ERROR) @font-lock-keyword-face

     [,@zig-keywords] @font-lock-keyword-face)

    :language zig
    :override t
    :feature numeric
    ([(INTEGER)
      (FLOAT)] @font-lock-number-face)

    :language zig
    :override t
    :feature string
    ([(LINESTRING)
      (STRINGLITERALSINGLE)] @font-lock-string-face

     (CHAR_LITERAL) @font-lock-string-face

     (EscapeSequence) @font-lock-escape-face

     (FormatSequence) @font-lock-string-face)

    :language zig
    :override t
    :feature label
    ((BreakLabel (IDENTIFIER)) @font-lock-type-face
     (BlockLabel (IDENTIFIER)) @font-lock-type-face)

    :language zig
    :feature operator
    ([(CompareOp)
      (BitwiseOp)
      (BitShiftOp)
      (AdditionOp)
      (AssignOp)
      (MultiplyOp)
      (PrefixOp)
      "*"
      "**"
      "->"
      ".?"
      ".*"
      "?"] @font-lock-operator-face)

    :language zig
    :feature punctuation
    (["["
      "]"
      "("
      ")"
      "{"
      "}"
      (Payload "|")
      (PtrPayload "|")
      (PtrIndexPayload "|")] @font-lock-bracket-face

     [";"
      "."
      ","
      ":"] @font-lock-delimiter-face

     [".."
      "..."] @font-lock-punctuation-face)))

(defvar zig-ts-indent-rules
  `((zig
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((parent-is "block") parent-bol zig-indent-offset)
     ((parent-is "AsmExpr") parent-bol zig-indent-offset)
     ((parent-is "AssignExpr") parent-bol zig-indent-offset)
     ((parent-is "Block") parent-bol zig-indent-offset)
     ((parent-is "BlockExpr") parent-bol zig-indent-offset)
     ((parent-is "ContainerDecl") parent-bol zig-indent-offset)
     ((parent-is "ErrorUnionExpr") parent-bol zig-indent-offset)
     ((parent-is "InitList") parent-bol zig-indent-offset)
     ((parent-is "SwitchExpr") parent-bol zig-indent-offset)
     ((parent-is "TestDecl") parent-bol zig-indent-offset))))

(defun zig-ts-setup ()
  "Setup treesit for zig-ts-mode."
  (interactive)

  ;; Font-lock
  (setq-local treesit-font-lock-settings
               (apply #'treesit-font-lock-rules
                    zig-ts-font-lock-rules))
  (setq-local treesit-font-lock-feature-list
              '((comment)
                (builtin keyword type)
                (constant string numeric)
                (variable function label operator punctuation)))

  ;; Indentation
  (setq-local treesit-simple-indent-rules zig-ts-indent-rules)

  ;; TODO Navigation
  ;; (setq-local treesit-defun-type-regexp ...)
  ;; (setq-local treesit-defun-name-function ...)

  ;; TODO Imenu
  ;; (setq-local treesit-simple-imenu-settings

  (treesit-major-mode-setup))

(defvar zig-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b") 'zig-compile)
    (define-key map (kbd "C-c C-f") 'zig-format-buffer)
    (define-key map (kbd "C-c C-r") 'zig-run)
    (define-key map (kbd "C-c C-t") 'zig-test-buffer)
    map)
  "Keymap for Zig major mode.")

;;;###autoload
(define-derived-mode zig-ts-mode zig-base-mode "Zig"
  "A treesitter enabled major mode for the Zig programming language.

\\{zig-ts-mode-map}"
  :group 'zig-ts-mode
  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'zig)
    (treesit-parser-create 'zig)
    (zig-ts-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(zig\\|zon\\)\\'" . zig-ts-mode))

(provide 'zig-ts-mode)

;;; zig-ts-mode.el ends here
