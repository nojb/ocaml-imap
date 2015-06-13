OCAMLBUILD = ocamlbuild -use-ocamlfind -classic-display

lib:
	$(OCAMLBUILD) lib/imap.cma
	-$(OCAMLBUILD) lib/imap.cmxa
	-$(OCAMLBUILD) lib/imap.cmxs

imap_shell:
	$(OCAMLBUILD) test/imap_shell.byte

wait_mail:
	$(OCAMLBUILD) test/wait_mail.byte

all: lib imap_shell wait_mail

clean:
	$(OCAMLBUILD) -clean

doc:
	$(OCAMLBUILD) -docflags -colorize-code,-css-style,style.css doc/api.docdir/index.html
	cp doc/style.css api.docdir/

install: lib
	opam-installer --prefix=`opam config var prefix` imap.install

uninstall:
	opam-installer --prefix=`opam config var prefix` -u imap.install

reinstall: uninstall install

gh-pages: doc
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp api.docdir/* .gh-pages/
	git -C .gh-pages add .
	git -C .gh-pages commit -m "Update Pages"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages

prepare: lib doc
ifdef VERSION
	git diff --quiet && git diff --cached --quiet # make sure there are no uncommited changes
	git tag -f "v$(VERSION)"
	git push origin master
	git push --force origin "v$(VERSION)"
	opam-publish prepare "imap.$(VERSION)" \
		"https://github.com/nojb/ocaml-imap/archive/v$(VERSION).tar.gz"
else
	$(error VERSION is undefined)
endif

publish: gh-pages
	opam-publish submit "./imap.$(VERSION)"

.PHONY: lib clean doc imap_shell wait_mail install uninstall
