.PHONY: all
all:
	dune build @all

.PHONY: test
test:
	dune runtest

.PHONY: clean
clean:
	dune clean

.PHONY: fmt
fmt:
	dune build @fmt --auto-promote

.PHONY: install
install:
	dune install

.PHONY: uninstall
uninstall:
	dune uninstall

.PHONY: reinstall
reinstall: uninstall install

.PHONY: doc
doc:
	dune build @doc

.PHONY: publish-doc
publish-doc: doc
	rm -rf .gh-pages
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp -r _build/default/_doc/* .gh-pages/
	git -C .gh-pages add .
	git -C .gh-pages commit -m "Update Pages"
	git -C .gh-pages push origin gh-pages -f

.PHONY: publish
publish: gh-pages
	opam-publish submit "./imap.$(VERSION)"
