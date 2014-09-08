OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

all: src/core/imap.cma

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
