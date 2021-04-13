open B_types
open Trajectory
open Tools

let last = 3
let data_size = 2

(********************************* gste assertion graph skeleton ******************************)
let vertexI = Vertex 0
let vertexL = vertexI::(List.map (fun i -> Vertex i) (upt 1 (last+1)))

let edgeL = 
	let e1 = Edge (Vertex 0, Vertex 1) in
	let e2_list = List.map (fun i -> Edge (Vertex i, Vertex (i+1))) (upt 1 last) in
	let e3 = Edge (Vertex (last+1), Vertex 1) in
	(e1::e2_list)@[e3]

(********************************* gste assertion formulas ******************************)
let reset = IVar (Ident ("rst", Bool))
let dout = IVar (Ident ("dout", Int data_size))
let low : expression = Const (BoolC false) 
let high : expression = Const (BoolC true)

let rstFormula = Eqn(reset, high)
let noRstFormula =  Eqn(reset, low)

let antOfCounter e = 
	let f = nodeToInt (source e) in
	(
		if(f == 0) then rstFormula
		else noRstFormula
	)

let consOfCounter e = 
	let f = nodeToInt (source e) in
	(
		if f == 0 then Chaos
		else Eqn (dout, Const (IntC ((f-1), data_size)))
	)

let counterGsteSpec = Graph (vertexI , vertexL,  edgeL, antOfCounter, consOfCounter)


(********************************* gste tag invariant ******************************)
let last_var = IVar (Ident ("last", Int 2))

let tag (Vertex n) = 
	if n = 0 then Chaos
	else Eqn (last_var, Const( IntC ((n-1), data_size)))
	


(********************************* ocaml term-level AG to ocaml boolean-level AG ******************************)
let antOfCounter_bool e = 
	termForm2bitForm (antOfCounter e)

let consOfCounter_bool e =
	termForm2bitForm (consOfCounter e)

let tag_bool n =
	termForm2bitForm (tag n)


(************************************** transform ocaml AG to forte AG(defined in trajectory.ml) *****************************************)	
let pprint () = 
	List.iter (
				fun e -> (
					let f = nodeToInt (source e) in
					let t = nodeToInt (sink e) in
					Printf.printf "Edge (%d, %d)'s boolean antecedent is: %s\n"  f t (bitForm2str (antOfCounter_bool e));
					Printf.printf "Edge (%d, %d)'s boolean consequent is: %s\n\n"  f t (bitForm2str (consOfCounter_bool e))
				)
			) edgeL;
			print_endline "";
	List.iter (
				fun e -> (
					let f = nodeToInt (source e) in
					let t = nodeToInt (sink e) in
					Printf.printf "Edge (%d, %d)'s boolean forte antecedent is: %s\n"  f t (trajForm2str (bitForm2trajForm (antOfCounter_bool e)));
					Printf.printf "Edge (%d, %d)'s boolean forte consequent is: %s\n\n"  f t (trajForm2str (bitForm2trajForm (consOfCounter_bool e)))
				)
			) edgeL;
			print_endline "";
	List.iter (
				fun (Vertex i) -> Printf.printf "Node %d's boolean tag invariant is: %s\n" i (bitForm2str (tag_bool(Vertex i)))
			) vertexL;
			print_endline "";
	List.iter (
				fun (Vertex i) -> Printf.printf "Node %d's boolean forte tag invariant is: %s\n" i (trajForm2str (bitForm2trajForm (tag_bool(Vertex i))))
			) vertexL

(** transform gsteSpec to forte file *)

let binNodes = []

let () = 
	toFL counterGsteSpec "counter" binNodes