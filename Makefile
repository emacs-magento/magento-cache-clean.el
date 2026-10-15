EMACS ?= emacs
BATCH = $(EMACS) -Q --batch -L .
DEPS_DIR = .deps
PACKAGES = --eval "(progn (require 'package) \
  (setq package-user-dir (expand-file-name \"$(DEPS_DIR)\")) \
  (package-initialize))"

.PHONY: all compile checkdoc package-lint lint test deps clean

all: compile lint test

compile:
	$(BATCH) -L test --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile magento-cache-clean.el test/magento-cache-clean-test.el

checkdoc:
	$(BATCH) --eval "(setq checkdoc-create-error-function \
	  (lambda (text start _end &optional _unfixable) \
	    (error \"%s:%d: %s\" (buffer-file-name) (line-number-at-pos start) text)))" \
	  --eval '(checkdoc-file "magento-cache-clean.el")'

package-lint: $(DEPS_DIR)
	$(BATCH) $(PACKAGES) -l package-lint \
	  -f package-lint-batch-and-exit magento-cache-clean.el

lint: checkdoc package-lint

test:
	$(BATCH) -L test -l magento-cache-clean-test -f ert-run-tests-batch-and-exit

deps: $(DEPS_DIR)

$(DEPS_DIR):
	$(BATCH) $(PACKAGES) --eval "(progn \
	  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
	  (package-refresh-contents) \
	  (package-install 'package-lint))"

clean:
	rm -rf *.elc test/*.elc $(DEPS_DIR)
