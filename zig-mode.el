;;; zig-mode.el --- A major mode for the Zig programming language -*- lexical-binding: t -*-

;; Version: 0.0.8
;; Author: Andrea Orru <andreaorru1991@gmail.com>, Andrew Kelley <superjoe30@gmail.com>
;; Keywords: zig, languages
;; Package-Requires: ((emacs "24.3"))
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

;; A major mode for the Zig programming languages.

;; See documentation on https://github.com/zig-lang/zig-mode

;;; Code:

(defgroup zig-mode nil
  "Support for Zig code."
  :link '(url-link "https://ziglang.org/")
  :group 'languages)

(defcustom zig-indent-offset 4
  "Indent Zig code by this number of spaces."
  :type 'integer
  :group 'zig-mode
  :safe #'integerp)

(defcustom zig-format-on-save t
  "Format buffers before saving using zig fmt."
  :type 'boolean
  :safe #'booleanp
  :group 'zig-mode)

(defcustom zig-zig-bin "zig"
  "Path to zig executable."
  :type 'string
  :safe #'stringp
  :group 'zig-mode)

;; zig CLI commands

(defun zig--run-cmd (cmd &optional source &rest args)
  "Use compile command to execute a zig CMD with ARGS if given.
If given a SOURCE, execute the CMD on it."
  (let ((cmd-args
         (if source
             (mapconcat 'identity (cons source args) " ")
           args)))
    (compile (concat zig-zig-bin " " cmd " " cmd-args))))

;;;###autoload
(defun zig-toggle-format-on-save ()
  "Switch format before save on current buffer."
  (interactive)
  (if zig-format-on-save
      (setq-local zig-format-on-save nil)
    (setq-local zig-format-on-save t)))

;;;###autoload
(defun zig-compile ()
  "Compile using `zig build`."
  (interactive)
  (zig--run-cmd "build"))

;;;###autoload
(defun zig-build-exe ()
  "Create executable from source or object file."
  (interactive)
  (zig--run-cmd "build-exe" (buffer-file-name)))

;;;###autoload
(defun zig-build-lib ()
  "Create library from source or assembly."
  (interactive)
  (zig--run-cmd "build-lib" (buffer-file-name)))

;;;###autoload
(defun zig-build-obj ()
  "Create object from source or assembly."
  (interactive)
  (zig--run-cmd "build-obj" (buffer-file-name)))

;;;###autoload
(defun zig-test-buffer ()
  "Test buffer using `zig test`."
  (interactive)
  (zig--run-cmd "test" (buffer-file-name) "--release-fast"))

;;;###autoload
(defun zig-run ()
  "Create an executable from the current buffer and run it immediately."
  (interactive)
  (zig--run-cmd "run" (buffer-file-name)))

;;;###autoload
(defun zig-format-buffer ()
  "Format the current buffer using the zig fmt."
  (interactive)
  (let ((fmt-buffer-name "*zig-fmt*"))
	;; If we have an old *zig-fmt* buffer, we want to kill
	;; it and start a new one to show the new errors
	(when (get-buffer fmt-buffer-name)
	  (kill-buffer fmt-buffer-name))
	(let ((fmt-buffer (get-buffer-create fmt-buffer-name)))
	  (set-process-sentinel
	   (start-process "zig-fmt"
					  fmt-buffer
					  zig-zig-bin
					  "fmt"
					  (buffer-file-name))
	   (lambda (process _e)
		 (if (> (process-exit-status process) 0)
		     (progn
			 (switch-to-buffer-other-window fmt-buffer)
			 (compilation-mode))
		   (revert-buffer :ignore-auto :noconfirm)))))))

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
    "noinline" "comptime" "callconv" "volatile" "allowzero"
    "align" "linksection" "threadlocal"

    ;; Structure
    "struct" "enum" "union" "error"

    ;; Statement
    "break" "return" "continue" "asm" "defer" "errdefer" "unreachable"
    "try" "catch" "async" "nosuspend" "await" "suspend" "resume"

    ;; Conditional
    "if" "else" "switch" "and" "or" "orelse"

    ;; Repeat
    "while" "for"

    ;; Other keywords
    "fn" "usingnamespace" "test"))

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
    "bool" "void" "noreturn" "type" "error" "anyerror" "anyframe" "anytype"))

(defconst zig-constants
  '(
    ;; Boolean
    "true" "false"

    ;; Other constants
    "null" "undefined" "this"))

(defconst zig-electric-indent-chars
  '(?\; ?, ?\) ?\] ?}))


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
                  ;; is-expr-continutation: True if this line continues an
                  ;; expression from the previous line, false otherwise.
                  (is-expr-continutation
                   (and
                    (not (looking-at "[]});]"))
                    (save-excursion
                      (zig-skip-backwards-past-whitespace-and-comments)
                      (when (> (point) 1)
                        (backward-char)
                        (not (looking-at "[,;([{}]")))))))
             ;; Now we can calculate indent-col:
             (if is-expr-continutation
                 (+ base-indent-col zig-indent-offset)
               base-indent-col)))))
    ;; If point is within the indentation whitespace, move it to the end of the
    ;; new indentation whitespace (which is what the indent-line-to function
    ;; always does).  Otherwise, we don't want point to move, so we use a
    ;; save-excursion.
    (if (<= (current-column) (current-indentation))
        (indent-line-to indent-col)
      (save-excursion (indent-line-to indent-col)))))

(defun zig-syntax-propertize-to-newline-if-in-multiline-str (end)
  ;; First, we need to check if we're in a multiline string literal; if we're
  ;; not, do nothing.
  (when (zig-currently-in-str)
    (let ((start (zig-start-of-current-str-or-comment)))
      (when (save-excursion
              (goto-char start)
              (looking-at "\\\\\\\\"))
        ;; At this point, we've determined that we're within a multiline string
        ;; literal.  Let `stop' be the position of the closing newline, or
        ;; `end', whichever comes first.
        (let ((stop (if (save-excursion
                          (goto-char start)
                          (re-search-forward "\n" end t))
                        (prog1 (match-end 0)
                          ;; We found the closing newline, so mark it as the
                          ;; end of this string literal.
                          (put-text-property (match-beginning 0)
                                             (match-end 0)
                                             'syntax-table
                                             (string-to-syntax "|")))
                      end)))
          ;; Zig multiline string literals don't support escapes, so mark all
          ;; backslashes (up to `stop') as punctation instead of escapes.
          (save-excursion
            (goto-char (+ 2 start))
            (while (re-search-forward "\\\\" stop t)
              (put-text-property (match-beginning 0) (match-end 0)
                                 'syntax-table (string-to-syntax "."))
              (goto-char (match-end 0))))
          ;; Move to the end of the string (or `end'), so that
          ;; zig-syntax-propertize can pick up from there.
          (goto-char stop))))))

(defun zig-syntax-propertize (start end)
  (goto-char start)
  (zig-syntax-propertize-to-newline-if-in-multiline-str end)
  (funcall
   (syntax-propertize-rules
    ;; Multiline strings
    ;; Do not match backslashes that are preceded by single or
    ;; double-quotes.
    ("[^\\'\"]c?\\(\\\\\\)\\\\"
     (1 (prog1 "|"
          (goto-char (match-end 0))
          (zig-syntax-propertize-to-newline-if-in-multiline-str end)))))
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

;;; Guarantee filesystem unix line endings
(defun zig-file-coding-system ()
  (with-current-buffer (current-buffer)
    (when (and buffer-file-name
               (string-match "\\.d?zig\\'" buffer-file-name))
      (setq buffer-file-coding-system 'utf-8-unix))))

(add-hook 'zig-mode-hook 'zig-file-coding-system)

(defvar zig-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-c C-b") 'zig-compile)
	(define-key map (kbd "C-c C-f") 'zig-format-buffer)
	(define-key map (kbd "C-c C-r") 'zig-run)
	(define-key map (kbd "C-c C-t") 'zig-test-buffer)
	map)
  "Keymap for Zig major mode.")

;;;###autoload
(define-derived-mode zig-mode prog-mode "Zig"
  "A major mode for the Zig programming language.

\\{zig-mode-map}"
  :group 'zig-mode
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local electric-indent-chars
              (append zig-electric-indent-chars
                      (and (boundp 'electric-indent-chars)
                           electric-indent-chars)))
  (setq-local indent-line-function 'zig-mode-indent-line)
  (setq-local indent-tabs-mode nil)  ; Zig forbids tab characters.
  (setq-local syntax-propertize-function 'zig-syntax-propertize)
  (setq-local imenu-generic-expression zig-imenu-generic-expression)
  (setq font-lock-defaults '(zig-font-lock-keywords
                             nil nil nil nil
                             (font-lock-syntactic-face-function . zig-mode-syntactic-face-function)))

  (add-hook 'before-save-hook 'zig-before-save-hook nil t))

(defun zig-before-save-hook ()
  (when zig-format-on-save
	(zig-format-buffer)))

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(provide 'zig-mode)
;;; zig-mode.el ends here
