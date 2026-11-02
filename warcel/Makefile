EMACS ?= emacs

.PHONY: all compile autoloads test checkdoc clean

all: compile autoloads test

compile:
	$(EMACS) -Q --batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile warcel.el

autoloads:
	$(EMACS) -Q --batch --eval '(progn (require (quote loaddefs-gen)) (loaddefs-generate default-directory (expand-file-name "warcel-autoloads.el") (list (expand-file-name "test/warcel-tests.el"))))'

test:
	$(EMACS) -Q --batch -L . -l test/warcel-tests.el -f ert-run-tests-batch-and-exit

checkdoc:
	$(EMACS) -Q --batch --eval '(checkdoc-file "warcel.el")'

clean:
	rm -f *.elc test/*.elc
