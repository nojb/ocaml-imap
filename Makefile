OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

all: core unix lwt

core: src/core/imap.cma

unix: src/unix/imapUnix.cma

lwt: src/lwt/imapLwt.cma

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
