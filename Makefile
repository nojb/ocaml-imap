OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

lib:
	$(OCAMLBUILD) imap/imapClient.byte

clean:
	$(OCAMLBUILD) -clean

.PHONY: clean lib
