;;; zig-mode.el --- A major mode for the Zig programming language -*- lexical-binding: t -*-

;; Version: 0.0.8
;; Author: Andrea Orru <andreaorru1991@gmail.com>, Andrew Kelley <superjoe30@gmail.com>
;; Keywords: zig, languages
;; Package-Requires: ((emacs "24.3") (reformatter "0.6"))
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

;; A concrete implementation of a major mode for the Zig programming languages dervied from zig-base-mode.

;; See documentation on https://github.com/zig-lang/zig-mode

;;; Code:

(require 'zig-base-mode)

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
            (goto-char (1+ start))
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

(defvar zig-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b") 'zig-compile)
    (define-key map (kbd "C-c C-f") 'zig-format-buffer)
    (define-key map (kbd "C-c C-r") 'zig-run)
    (define-key map (kbd "C-c C-t") 'zig-test-buffer)
    map)
  "Keymap for Zig major mode.")

(add-hook 'zig-mode-hook 'zig-file-coding-system)

;;;###autoload
(define-derived-mode zig-mode zig-base-mode "Zig"
  "A major mode for the Zig programming language.

\\{zig-mode-map}"
  :group 'zig-mode
  (setq font-lock-defaults '(zig-font-lock-keywords
                             nil nil nil nil
                             (font-lock-syntactic-face-function . zig-mode-syntactic-face-function))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(zig\\|zon\\)\\'" . zig-mode))

(provide 'zig-mode)
;;; zig-mode.el ends here
