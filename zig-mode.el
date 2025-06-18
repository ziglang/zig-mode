;;; zig-mode.el --- A major mode for the Zig programming language -*- lexical-binding: t -*-

;; Author: Andrea Orru <andreaorru1991@gmail.com>
;;         Andrew Kelley <superjoe30@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/zig-lang/zig-mode
;; Version: 0.0.8
;; Package-Requires: ((emacs "26.1") (reformatter "0.6"))
;; Keywords: zig, languages

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

;; A major mode for the Zig programming languages.

;; See documentation on https://github.com/zig-lang/zig-mode

;;; Code:

(require 'reformatter)

(defgroup zig-mode nil
  "Support for Zig code."
  :link '(url-link "https://ziglang.org/")
  :group 'languages)

(defcustom zig-indent-offset 4
  "Indent Zig code by this number of spaces."
  :type 'integer
  :safe #'integerp)

(defcustom zig-format-on-save t
  "Format buffers before saving using zig fmt."
  :type 'boolean
  :safe #'booleanp)

(defcustom zig-ast-check-on-format nil
  "Look for simple compile errors on format."
  :type 'boolean
  :safe #'booleanp)

(defcustom zig-zig-bin "zig"
  "Path to zig executable."
  :type 'file
  :safe #'stringp)

(defcustom zig-run-optimization-mode "Debug"
  "Optimization mode to run code with."
  :type 'string
  :safe #'stringp)

(defcustom zig-test-optimization-mode "Debug"
  "Optimization mode to run tests with."
  :type 'string
  :safe #'stringp)

;; zig CLI commands

(defun zig--run-cmd (cmd &optional source &rest args)
  "Use compile command to execute a zig CMD with ARGS if given.
If given a SOURCE, execute the CMD on it."
  (let ((cmd-args (if source (cons source args) args)))
    (save-some-buffers)
    (compilation-start (mapconcat 'shell-quote-argument
                                  `(,zig-zig-bin ,cmd ,@cmd-args) " "))))

;;;###autoload
(defun zig-compile ()
  "Compile using `zig build`."
  (interactive)
  (zig--run-cmd "build"))

;;;###autoload
(defun zig-build-exe ()
  "Create executable from source or object file."
  (interactive)
  (zig--run-cmd "build-exe" (file-local-name (buffer-file-name))))

;;;###autoload
(defun zig-build-lib ()
  "Create library from source or assembly."
  (interactive)
  (zig--run-cmd "build-lib" (file-local-name (buffer-file-name))))

;;;###autoload
(defun zig-build-obj ()
  "Create object from source or assembly."
  (interactive)
  (zig--run-cmd "build-obj" (file-local-name (buffer-file-name))))

;;;###autoload
(defun zig-test-buffer ()
  "Test buffer using `zig test`."
  (interactive)
  (zig--run-cmd "test" (file-local-name (buffer-file-name)) "-O" zig-test-optimization-mode))

;;;###autoload
(defun zig-run ()
  "Create an executable from the current buffer and run it immediately."
  (interactive)
  (zig--run-cmd "run" (file-local-name (buffer-file-name)) "-O" zig-run-optimization-mode))

;; zig fmt
(reformatter-define zig-format
  :program zig-zig-bin
  :args (append '("fmt" "--stdin")
                (when (string-match-p "\\.zon\\'" buffer-file-name) '("--zon"))
                (when zig-ast-check-on-format '("--ast-check")))
  :group 'zig-mode
  :lighter " ZigFmt")

;;;###autoload (autoload 'zig-format-buffer "zig-mode" nil t)
;;;###autoload (autoload 'zig-format-region "zig-mode" nil t)
;;;###autoload (autoload 'zig-format-on-save-mode "zig-mode" nil t)

(defun zig-re-word (inner)
  "Construct a regular expression for the word INNER."
  (concat "\\<" inner "\\>"))

(defun zig-re-grab (inner)
  "Construct a group regular expression for INNER."
  (concat "\\(" inner "\\)"))

(defconst zig-re-optional "\\(?:[[:space:]]*\\?[[:space:]]*\\)")
(defconst zig-re-pointer "\\(?:[[:space:]]*\\*\\(?:const[[:space:]]*\\)?[[:space:]]*\\)")
(defconst zig-re-array "\\(?:[[:space:]]*\\[[^]]*\\]\\(?:const[[:space:]]*\\)?[[:space:]]*\\)")

(defconst zig-re-optionals-pointers-arrays
  (concat "\\(?:" zig-re-optional "\\|" zig-re-pointer "\\|" zig-re-array "\\)*"))

(defconst zig-re-identifier "[[:word:]_][[:word:]_[:digit:]]*")
(defconst zig-re-type "[[:word:]_.][[:word:]_.[:digit:]]*")
(defconst zig-re-type-annotation
  (concat (zig-re-grab zig-re-identifier)
          "[[:space:]]*:[[:space:]]*"
          zig-re-optionals-pointers-arrays
          (zig-re-grab zig-re-type)))

(defconst zig-re-block-label-open " \\([[:word:]_]+:\\)[[:space:]]*{")
(defconst zig-re-block-label-break "break[[:space:]]*\\(:[[:word:]_]+\\)")

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

(defconst zig-electric-indent-chars
  '(?\; ?\, ?\) ?\] ?\}))

(defface zig-multiline-string-face
  '((t :inherit font-lock-string-face))
  "Face for multiline string literals.")

(defvar zig-font-lock-keywords
  (append
   `(;; Builtins (prefixed with @)
     (,(concat "@" zig-re-identifier) . font-lock-builtin-face)

     ;; Keywords, constants and types
     (,(rx symbol-start
           (|
            ;; Storage
            "const" "var" "extern" "packed" "export" "pub" "noalias" "inline"
            "noinline" "comptime" "callconv" "volatile" "allowzero"
            "align" "linksection" "threadlocal" "addrspace"

            ;; Structure
            "struct" "enum" "union" "error" "opaque"

            ;; Statement
            "break" "return" "continue" "asm" "defer" "errdefer" "unreachable"
            "try" "catch" "async" "nosuspend" "await" "suspend" "resume"

            ;; Conditional
            "if" "else" "switch" "and" "or" "orelse"

            ;; Repeat
            "while" "for"

            ;; Other keywords
            "fn" "usingnamespace" "test")
           symbol-end)
      . font-lock-keyword-face)

     (,(rx symbol-start
           (|
            ;; Boolean
            "true" "false"

            ;; Other constants
            "null" "undefined")
           symbol-end)
      . font-lock-constant-face)

     (,(rx symbol-start
           (|
            ;; Integer types
            (: (any ?i ?u) (| ?0 (: (any (?1 . ?9)) (* digit))))
            "isize" "usize"

            ;; Floating types
            "f16" "f32" "f64" "f80" "f128"

            ;; C types
            "c_char" "c_short" "c_ushort" "c_int" "c_uint" "c_long" "c_ulong"
            "c_longlong" "c_ulonglong" "c_longdouble"

            ;; Comptime types
            "comptime_int" "comptime_float"

            ;; Other types
            "bool" "void" "noreturn" "type" "anyerror" "anyframe" "anytype"
            "anyopaque")
           symbol-end)
      . font-lock-type-face)

     ;; Block labels
     (,zig-re-block-label-open 1 font-lock-constant-face)
     (,zig-re-block-label-break 1 font-lock-constant-face)

     ;; Type annotations (both variable and type)
     (,zig-re-type-annotation 1 font-lock-variable-name-face)
     (,zig-re-type-annotation 2 font-lock-type-face))

   ;; Definitions
   (mapcar (lambda (x)
             (list (zig-re-definition (car x))
                   1 (cdr x)))
           '(("const" . font-lock-variable-name-face)
             ("var"   . font-lock-variable-name-face)
             ("fn"    . font-lock-function-name-face)))))

(defun zig-paren-nesting-level () (nth 0 (syntax-ppss)))
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

(defun zig-in-str-or-cmnt () (nth 8 (syntax-ppss)))
(defconst zig-top-item-beg-re
  (concat "^ *"
          (regexp-opt
           '("pub" "extern" "export" ""))
          "[[:space:]]*"
          (regexp-opt
           '("fn" "test"))
          "[[:space:]]+")
  "Start of a Zig item.")

(defun zig-beginning-of-defun (&optional arg)
  "Move backward to the beginning of the current defun.

With ARG, move backward multiple defuns.  Negative ARG means
move forward.

This is written mainly to be used as `beginning-of-defun-function' for Zig."
  (interactive "p")
  (let* ((arg (or arg 1))
         (magnitude (abs arg))
         (sign (if (< arg 0) -1 1)))
    ;; If moving forward, don't find the defun we might currently be
    ;; on.
    (when (< sign 0)
      (end-of-line))
    (catch 'done
      (dotimes (_ magnitude)
        ;; Search until we find a match that is not in a string or comment.
        (while (if (re-search-backward (concat "^[[:space:]]*\\(" zig-top-item-beg-re "\\)")
                                       nil 'move sign)
                   (zig-in-str-or-cmnt)
                 ;; Did not find it.
                 (throw 'done nil)))))
    t))

(defun zig-end-of-defun ()
  "Move forward to the next end of defun.

With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

Assume that this is called after `beginning-of-defun'.  So point is
at the beginning of the defun body.

This is written mainly to be used as `end-of-defun-function' for Zig."
  (interactive)

  ;; Jump over the function parameters and paren-wrapped return, if they exist.
  (while (re-search-forward "(" (line-end-position) t)
    (progn
      (backward-char)
      (forward-sexp)))

  ;; Find the opening brace
  (if (re-search-forward "[{]" nil t)
      (progn
        (goto-char (match-beginning 0))
        ;; Go to the closing brace
        (condition-case nil
            (forward-sexp)
          (scan-error
           (goto-char (point-max))))
        (end-of-line))
    ;; There is no opening brace, so consider the whole buffer to be one "defun"
    (goto-char (point-max))))

(defun zig-mode-indent-line ()
  (interactive)
  ;; First, calculate the column that this line should be indented to.
  (let ((indent-col
         (save-excursion
           (back-to-indentation)
           (let* (;; paren-level: How many sets of parens (or other delimiters)
                  ;;   we're within, except that if this line closes the
                  ;;   innermost set(s) (e.g. the line is just "}"), then we
                  ;;   don't count those set(s).
                  (paren-level
                   (save-excursion
                     (while (looking-at "[]})]") (forward-char))
                     (zig-paren-nesting-level)))
                  ;; prev-block-indent-col: If we're within delimiters, this is
                  ;; the column to which the start of that block is indented
                  ;; (if we're not, this is just zero).
                  (prev-block-indent-col
                   (if (<= paren-level 0) 0
                     (save-excursion
                       (while (>= (zig-paren-nesting-level) paren-level)
                         (backward-up-list)
                         (back-to-indentation))
                       (current-column))))
                  ;; base-indent-col: The column to which a complete expression
                  ;;   on this line should be indented.
                  (base-indent-col
                   (if (<= paren-level 0)
                       prev-block-indent-col
                     (or (save-excursion
                           (backward-up-list)
                           (forward-char)
                           (and (not (looking-at " *\\(//[^\n]*\\)?\n"))
                                (current-column)))
                         (+ prev-block-indent-col zig-indent-offset))))
                  ;; is-expr-continuation: True if this line continues an
                  ;; expression from the previous line, false otherwise.
                  (is-expr-continuation
                   (and
                    (not (looking-at "[]});]\\|else"))
                    (save-excursion
                      (zig-skip-backwards-past-whitespace-and-comments)
                      (when (> (point) 1)
                        (backward-char)
                        (or (zig-currently-in-str)
                            (not (looking-at "[,;([{}]"))))))))
             ;; Now we can calculate indent-col:
             (if is-expr-continuation
                 (+ base-indent-col zig-indent-offset)
               base-indent-col)))))
    ;; If point is within the indentation whitespace, move it to the end of the
    ;; new indentation whitespace (which is what the indent-line-to function
    ;; always does).  Otherwise, we don't want point to move, so we use a
    ;; save-excursion.
    (if (<= (current-column) (current-indentation))
        (indent-line-to indent-col)
      (save-excursion (indent-line-to indent-col)))))

(defun zig-syntax-propertize-multiline-string (end)
  (let* ((eol (save-excursion (search-forward "\n" end t)))
         (stop (or eol end)))
    (while (search-forward "\\" stop t)
      (put-text-property (match-beginning 0) (match-end 0) 'syntax-table (string-to-syntax ".")))
    (when eol (put-text-property (- eol 2) (1- eol) 'syntax-table (string-to-syntax "|")))
    (goto-char stop)))

(defun zig-syntax-propertize (start end)
  (goto-char start)
  (when (eq t (zig-currently-in-str))
    (zig-syntax-propertize-multiline-string end))
  (while (search-forward "\\\\" end t)
    (when (null (save-excursion (backward-char 2) (zig-currently-in-str)))
      (backward-char)
      (put-text-property (match-beginning 0) (point) 'syntax-table (string-to-syntax "|"))
      (zig-syntax-propertize-multiline-string end))))

(defun zig-mode-syntactic-face-function (state)
  (save-excursion
    (goto-char (nth 8 state))
    (if (nth 3 state)
        (if (looking-at "\\\\\\\\")
            'zig-multiline-string-face
          'font-lock-string-face)
      (if (looking-at "//[/|!][^/]")
          'font-lock-doc-face
        'font-lock-comment-face))))

;;; Imenu support
(defun zig-re-structure-def-imenu (stype)
  "Construct a regular expression for strucutres definitions of type STYPE."
  (concat (zig-re-word "const") "[[:space:]]+"
          (zig-re-grab zig-re-identifier)
          ".*"
          (zig-re-word stype)))

(defvar zig-imenu-generic-expression
  (append (mapcar (lambda (x)
                    (list (capitalize x) (zig-re-structure-def-imenu x) 1))
                  '("enum" "struct" "union"))
          `(("Fn" ,(zig-re-definition "fn") 1))))

(defvar zig-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b") #'zig-compile)
    (define-key map (kbd "C-c C-f") #'zig-format-buffer)
    (define-key map (kbd "C-c C-r") #'zig-run)
    (define-key map (kbd "C-c C-t") #'zig-test-buffer)
    map)
  "Keymap for Zig major mode.")

;;;###autoload
(define-derived-mode zig-mode prog-mode "Zig"
  "A major mode for the Zig programming language."
  (setq-local adaptive-fill-regexp
	      (concat "//[!/]* *"
		  (if (default-value 'adaptive-fill-regexp)
		      (concat "\\("
			      (default-value 'adaptive-fill-regexp)
			      "\\)")
		    "")))
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//[!/]* *")
  (setq-local comment-end "")
  (setq-local electric-indent-chars
              (append zig-electric-indent-chars
                      (and (boundp 'electric-indent-chars)
                           electric-indent-chars)))
  (setq-local beginning-of-defun-function 'zig-beginning-of-defun)
  (setq-local end-of-defun-function 'zig-end-of-defun)
  (setq-local indent-line-function 'zig-mode-indent-line)
  (setq-local indent-tabs-mode nil)  ; Zig forbids tab characters.
  (setq-local syntax-propertize-function 'zig-syntax-propertize)
  (setq-local imenu-generic-expression zig-imenu-generic-expression)
  (setq-local compile-command "zig build")
  (setq buffer-file-coding-system 'utf-8-unix) ; zig source is always utf-8
  (setq font-lock-defaults '(zig-font-lock-keywords
                             nil nil nil nil
                             (font-lock-syntactic-face-function . zig-mode-syntactic-face-function)))

  (when zig-format-on-save
    (zig-format-on-save-mode 1)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(zig\\|zon\\)\\'" . zig-mode))

(provide 'zig-mode)
;;; zig-mode.el ends here
