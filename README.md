# zig-mode

Syntax highlighting and automatic indentation for the [Zig programming
language](http://ziglang.org) in Emacs.  Requires Emacs 24.3 or later.

## Installation

[![NonGNU ELPA](https://elpa.nongnu.org/nongnu/zig-mode.svg)](https://elpa.nongnu.org/nongnu/zig-mode.html)
[![MELPA](https://melpa.org/packages/zig-mode-badge.svg)](https://melpa.org/#/zig-mode)

Simply install the `zig-mode` package via [NonGNU ELPA](https://elpa.nongnu.org/) or
[MELPA](https://melpa.org/#/getting-started).

### Manual install

Alternatively, you can `git clone` the `zig-mode` repository somewhere
(e.g. under your `~/.emacs.d/`). `zig-mode` depends on
[reformatter](https://github.com/purcell/emacs-reformatter) for
formatting buffers with `zig fmt`.

`reformatter` can be installed from MELPA or installed manually in a
similar fashion to `zig-mode`. For this method, clone the
`reformatter` git repository and add the path to the repository to
your `load-path`.

Then add the following to your `.emacs` file:

```elisp
(unless (version< emacs-version "24")
  (add-to-list 'load-path "~/path/to/your/zig-mode/")
  (autoload 'zig-mode "zig-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))
```

## Testing

[![CI](https://github.com/ziglang/zig-mode/actions/workflows/main.yml/badge.svg)](https://github.com/ziglang/zig-mode/actions/workflows/main.yml)

To run all unit tests with `emacs`, run:

```bash
./run_tests.sh
```

Note that Emacs 26.1 or later is required.  If you need to specify which Emacs
binary to use, you can do that by setting the `EMACS` environment variable,
e.g.:

```bash
EMACS=/usr/bin/emacs24 ./run_tests.sh
```

## Optional Configuration

`zig-mode` used to enable coloration of the compilation buffer using
ANSI color codes, but this affected *all* compilation buffers, not just
zig compilation output.
If you want to restore this behavior, you can add the following snippet
to your `init.el` or `.emacs` file:

```elisp
(if (>= emacs-major-version 28)
    (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (progn
    (defun colorize-compilation-buffer ()
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point))))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))
```

## License

`zig-mode` is distributed under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 3, or (at your
option) any later version.

See the [LICENSE](LICENSE) file for details.
