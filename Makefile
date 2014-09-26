OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

all:
	ocaml pkg/build.ml native=true native-dynlink=true lwt=true ssl=true

clean:
	$(OCAMLBUILD) -clean
	rm -f imap.install

.PHONY: clean all
