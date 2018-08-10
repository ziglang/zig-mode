# zig-mode
Syntax highlighting for the [Zig programming language](http://ziglang.org) in Emacs.

## Installation
Simply install the `zig-mode` package via [MELPA](https://melpa.org/#/getting-started).

Alternatively, you can `git clone` the `zig-mode` repository somewhere
(e.g. under your `~/.emacs.d/`), then add the following to your `.emacs` file:

```elisp
(add-to-list 'load-path "~/path/to/your/zig-mode/")
(autoload 'zig-mode "zig-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))
```
