OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

all: imap/imap.cma imap/imap.cmxa

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
