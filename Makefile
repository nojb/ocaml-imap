OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

all: imap unix lwt test

imap: src/imap/imap.cma

unix: src/unix/imapUnix.cma

lwt: src/lwt/imapLwt.cma

test: tests/test_unix.byte

%.cma:
	$(OCAMLBUILD) $@

%.cmxa:
	$(OCAMLBUILD) $@

%.native:
	$(OCAMLBUILD) $@

%.byte:
	$(OCAMLBUILD) $@

clean:
	$(OCAMLBUILD) -clean

.PHONY: clean all
