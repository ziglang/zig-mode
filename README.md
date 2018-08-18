# zig-mode

Syntax highlighting and automatic indentation for the
[Zig programming language](http://ziglang.org) in Emacs.  Requires Emacs 24 or
later.

## Installation

[![MELPA](https://melpa.org/packages/zig-mode-badge.svg)](https://melpa.org/#/zig-mode)

Simply install the `zig-mode` package via
[MELPA](https://melpa.org/#/getting-started).

Alternatively, you can `git clone` the `zig-mode` repository somewhere
(e.g. under your `~/.emacs.d/`), then add the following to your `.emacs` file:

```elisp
(unless (version< emacs-version "24")
  (add-to-list 'load-path "~/path/to/your/zig-mode/")
  (autoload 'zig-mode "zig-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))
```
