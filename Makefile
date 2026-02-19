EMACS ?= emacs
EASK ?= eask

# --- package-lint installation: https://github.com/purcell/package-lint/blob/master/Makefile

PACKAGE_LINT_MARKER := .package-lint-installed

$(PACKAGE_LINT_MARKER):
	$(EMACS) -Q --batch \
		--eval "(progn \
                  (require 'package) \
                  (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) \
                  (package-initialize) \
                  (unless (package-installed-p 'package-lint) \
                    (progn \
                      (package-refresh-contents) \
                      (package-install 'package-lint))) \
                  (write-region \"\" nil \"$(PACKAGE_LINT_MARKER)\"))"
	@touch $(PACKAGE_LINT_MARKER)

# package-lint: $(PACKAGE_LINT_MARKER)
# 	$(EMACS) -Q --batch \
# 		--eval "(require 'package-lint)" \
# 		-f package-lint-batch-and-exit your-package.el
# ---

# .PHONY: clean checkdoc lint package install compile test

ci: test checkdoc lint
# clean package install compile

# package:
# 	$(EASK) package

# install:
# 	@echo "Installing..."
# 	$(EASK) install

# compile:
# 	@echo "Compiling..."
# 	$(EASK) compile

test:
	@echo "Testing..."
	$(EASK) test ert ./*tests.el

checkdoc: $(PACKAGE_LINT_MARKER)
	@echo "Run checkdoc..."
	$(EASK) lint checkdoc

# https://emacs-eask.github.io/Getting-Started/Commands-and-options/#-eask-lint-package

lint: $(PACKAGE_LINT_MARKER)
	@echo "Run package-lint..."
	$(EASK) lint package

# clean:
# 	$(EASK) clean all
# rm -f $(PACKAGE_LINT_MARKER)
