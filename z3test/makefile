INCL = -I ~/.opam/4.06.1/lib/z3 

all:
	ocamlopt ${INCL} -o z3test nums.cmxa z3ml.cmxa z3Test.ml

clean:
	rm *.cmi *.cmx *.cmo *.o z3test