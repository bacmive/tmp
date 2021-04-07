# Ref: https://hub.fastgit.org/ocaml/ocamlbuild/blob/master/manual/manual.adoc
# install z3 with ocaml-api(make; make install; cd ~/.opam/4.06.1/lib/; mv Z3 z3)
# _tags (true: package(z3), thread)
# ocamlbuild -use-ocamlfind '${OBJ}' 
.PHONY: all clean
OBJ= fifo.native
all:
	ocamlbuild -use-ocamlfind '${OBJ}'

counter:
	mv _tags jb
	ocamlbuild 'counter.native'
	mv jb _tags

clean:
	ocamlbuild -clean
