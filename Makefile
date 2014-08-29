OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

lib:
	$(OCAMLBUILD) imap/imapClient.native

clean:
	$(OCAMLBUILD) -clean

.PHONY: clean lib
