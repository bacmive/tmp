# Ref: https://hub.fastgit.org/ocaml/ocamlbuild/blob/master/manual/manual.adoc
# install z3 with ocaml-api(make; make install; cd ~/.opam/4.06.1/lib/; mv Z3 z3)
# $vim _tags	"true: package(z3), thread"
# ocamlbuild -use-ocamlfind '${OBJ}' 
.PHONY: all clean
OBJ= fifo.native

all: memory counter

memory:
	ocamlbuild 'memory.native'

counter:
	ocamlbuild 'counter.native'

fifo:
	ocamlbuild -use-ocamlfind '${OBJ}'

clean:
	ocamlbuild -clean
