# all:
# 	# ocamlbuild gste.byte
# 	ocamlbuild my_rbfifo.byte

# clean:
# 	ocamlbuild -clean

SRC_FILE = my_rbfifo.ml
OBJ = my_rbfifo

Z3_PATH = /home/cheech/Project/z3/build/api/ml
all: ${Z3_PATH}/z3ml.cma ${SRC_FILE}
	ocamlfind ocamlc -custom -o ${OBJ} -package zarith -I ${Z3_PATH}/ -cclib "-L. -lpthread -lstdc++ -lz3" -linkpkg z3ml.cma nums.cma ${SRC_FILE}

clean:
	rm *.cmi *.cmx *.cmo *.o ${OBJ} *.byte
