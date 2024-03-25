#!/bin/sh

if [ -z "${EMACS}" ]; then
    EMACS="emacs"
else
    echo "Running with EMACS=${EMACS}"
fi

# From https://github.com/jcollard/elm-mode/blob/master/Makefile
NEEDED_PACKAGES="reformatter"
INIT_PACKAGES="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
        (package-refresh-contents)) \
      (package-install pkg))) \
  )"

"${EMACS}" --eval "${INIT_PACKAGES}" --batch -l zig-mode.el -l test/zig-tests.el -f ert-run-tests-batch-and-exit
