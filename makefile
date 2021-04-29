# Ref: https://hub.fastgit.org/ocaml/ocamlbuild/blob/master/manual/manual.adoc
# install z3 with ocaml-api(make; make install; cd ~/.opam/4.06.1/lib/; mv Z3 z3)
# $vim _tags	"true: package(z3), thread"
# ocamlbuild -use-ocamlfind '${OBJ}' 
.PHONY: all clean memory fifo counter
Z3_PATH = /home/cheech/Project/z3/build/api/ml

all: memory fifo counter 

memory:
	ocamlbuild -use-ocamlfind 'memory.native'

counter:
	ocamlbuild -use-ocamlfind 'counter.native'

fifo:
	ocamlbuild -use-ocamlfind -use-ocamlfind 'fifo.native'

clean:
	ocamlbuild -clean
