all:
	dune build --dev @install # @examples/examples

test:
	dune runtest

clean:
	dune clean

install:
	dune install

uninstall:
	dune uninstall

reinstall: uninstall install

doc:
	dune build @doc

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

publish: gh-pages
	opam-publish submit "./imap.$(VERSION)"

.PHONY: all test clean install uninstall doc
