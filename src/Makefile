all: occ

occ:
	ocamlbuild -use-ocamlfind -use-menhir -lexflag -ml -yaccflags -v,--explain occ.native
	cp occ.native ../bin/occ

clean:
	ocamlbuild -clean
