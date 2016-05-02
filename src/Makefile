all: occ

MENHIR=-v --explain --unused-tokens

USE_MENHIR_TRACE=false

ifeq ($(USE_MENHIR_TRACE),true)
	MENHIR+=--trace
endif 

occ:
	ocamlbuild -use-ocamlfind -use-menhir -lexflag -ml -menhir "menhir $(MENHIR)" -cflags -g occ.native
	cp occ.native ../bin/occ

clean:
	ocamlbuild -clean
