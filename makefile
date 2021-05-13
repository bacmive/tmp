# Ref: https://hub.fastgit.org/ocaml/ocamlbuild/blob/master/manual/manual.adoc
# install z3 with ocaml-api(make; make install; cd ~/.opam/4.06.1/lib/; mv Z3 z3)
# _tags (true: package(z3), thread)
# ocamlbuild -use-ocamlfind '${OBJ}' 
.PHONY: clean

rbfifo:
	ocamlbuild -use-ocamlfind 'fifo.native'

rsfifo:
	ocamlbuild -use-ocamlfind 'rsfifo.native'

counter:
	ocamlbuild -use-ocamlfind 'counter.native'

memory:
	ocamlbuild -use-ocamlfind 'memory.native'

clean:
	ocamlbuild -clean
