OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

all: imap lwt

imap: imap/imap.cma

lwt: lwt/imapSession.cma

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
