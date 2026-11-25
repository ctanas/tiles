EMACS ?= emacs

.PHONY: all compile test checkdoc clean

all: compile test

compile:
	$(EMACS) -Q --batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile tiles.el

test:
	$(EMACS) -Q --batch -L . -l test/tiles-warc-tests.el -f ert-run-tests-batch-and-exit

checkdoc:
	$(EMACS) -Q --batch --eval '(checkdoc-file "tiles.el")'

clean:
	rm -f *.elc test/*.elc
