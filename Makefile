# Makefile
# REQUIRE Eask: # in [[file:./.github/workflows/test.yml::53::version: 'snapshot']]
# - name: Install Eask
#         uses: emacs-eask/setup-eask@master
#         with:
#           version: 'snapshot'

EMACS ?= emacs
EASK ?= eask

# --- package-lint installation: https://github.com/purcell/package-lint/blob/master/Makefile

PACKAGE_LINT_MARKER := .package-lint-installed

# Checks if .package-lint-installed exists. If it doesn't, it fires up
#  Emacs in batch mode (-Q --batch), temporarily adds the MELPA
#  repository, installs package-lint, and then creates (touch) that
#  hidden marker file so it doesn't have to reinstall it next time.

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
	@echo "\nMakefine 1) Testing...\n"
	$(EASK) test ert ./*tests.el

checkdoc: $(PACKAGE_LINT_MARKER)
	@echo "\nMakefine 2) Run checkdoc...\n"
	$(EASK) lint checkdoc  2>&1

# https://emacs-eask.github.io/Getting-Started/Commands-and-options/#-eask-lint-package

# When you run eask lint package, Eask wraps around Emacs. It spins up
#  its own internal Emacs batch process, loads the package-lint tool that
#  was downloaded in the previous step, and uses it to analyze your
#  package files for errors or formatting issues.

lint: $(PACKAGE_LINT_MARKER)
	@echo "\nMakefine 3) Run package-lint...\n"
	$(EASK) lint package  2>&1

# clean:
# 	$(EASK) clean all
# rm -f $(PACKAGE_LINT_MARKER)
