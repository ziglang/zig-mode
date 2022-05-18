# zig-mode

Syntax highlighting and automatic indentation for the
[Zig programming language](http://ziglang.org) in Emacs.  Requires Emacs 24.3 or
later.

## Installation

[![NonGNU ELPA](https://elpa.nongnu.org/nongnu/zig-mode.svg)](https://elpa.nongnu.org/nongnu/zig-mode.html)
[![MELPA](https://melpa.org/packages/zig-mode-badge.svg)](https://melpa.org/#/zig-mode)

Simply install the `zig-mode` package via [NonGNU ELPA](https://elpa.nongnu.org/) or
[MELPA](https://melpa.org/#/getting-started).

Alternatively, you can `git clone` the `zig-mode` repository somewhere
(e.g. under your `~/.emacs.d/`), then add the following to your `.emacs` file:

```elisp
(unless (version< emacs-version "24")
  (add-to-list 'load-path "~/path/to/your/zig-mode/")
  (autoload 'zig-mode "zig-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))
```

To enable parsing of Zig `std.Progress` output (which is shown during `zig build`) in compilation or shell
buffers, add the following to your emacs config:
```
(zig-enable-progress-parsing)
```
This overrides function `ansi-color-apply-on-region` with a patched version.

## Testing

[![Build status](https://ci.appveyor.com/api/projects/status/u78j130vv4l6v21t?svg=true)](https://ci.appveyor.com/project/mdsteele/zig-mode)

To run all unit tests with `emacs`, run:

```bash
./run_tests.sh
```

Note that Emacs 24.3 or later is required.  If you need to specify which Emacs
binary to use, you can do that by setting the `EMACS` environment variable,
e.g.:

```bash
EMACS=/usr/bin/emacs24 ./run_tests.sh
```

## License

`zig-mode` is distributed under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 3, or (at your
option) any later version.

See the [LICENSE](LICENSE) file for details.
