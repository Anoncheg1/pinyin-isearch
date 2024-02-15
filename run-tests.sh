#!/bin/sh -e

EMACS="${EMACS:=emacs}"

if [ -n "${EMACS_LINT_IGNORE+x}" ]; then
    ERROR_ON_WARN=nil
else
    ERROR_ON_WARN=t
fi

# NEEDED_PACKAGES="cl-lib let-alist compat"
echo '# ----- ert -------'
"$EMACS" --quick -batch -l ert -l pinyin-isearch-loaders.el -l \
         pinyin-isearch-pinyin.el -l pinyin-isearch-pinyin-tests.el -f \
         ert-run-tests-batch-and-exit

"$EMACS" --quick -batch -l ert -l pinyin-isearch-loaders.el -l \
         pinyin-isearch-chars.el -l pinyin-isearch-chars-tests.el -f \
         ert-run-tests-batch-and-exit
echo '# ----- batch-byte-compile -------'
"$EMACS" --quick -batch \
         -l pinyin-isearch-loaders.el \
         -l pinyin-isearch-pinyin.el \
         -l pinyin-isearch-chars.el \
         -l pinyin-isearch.el \
         --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
         -f batch-byte-compile \
         pinyin-isearch.el pinyin-isearch-loaders.el pinyin-isearch-chars.el pinyin-isearch-pinyin.el

echo '# ----- package-lint ---------'
"$EMACS" --quick -batch \
         --eval "(setq package-lint-main-file \"pinyin-isearch.el\")" \
         --eval "(let ((default-directory  \"~/.emacs.d/elpa/\")) (normal-top-level-add-subdirs-to-load-path))" \
         --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
         -l package-lint \
         -f package-lint-batch-and-exit \
         pinyin-isearch.el pinyin-isearch-loaders.el pinyin-isearch-chars.el pinyin-isearch-pinyin.el
