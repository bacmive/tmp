open B_types
open Tools
open Trajectory

let last = 3
let data_size = 2

(********************************* gste assertion graph skeleton ******************************)
let vectexI = Vertex 0
let vectexL = vectexI::(List.map (fun i -> Vertex i) (upt 1 (last+1)))

let edgeL = 
	let e1 = Edge (Vertex 0, Vertex 1) in
	let e2_list = List.map (fun i -> Edge (Vertex i, Vertex (i+1))) (upt 1 last) in
	let e3 = Edge (Vertex (last+1), Vertex 1) in
	(e1::e2_list)@[e3]

(********************************* gste assertion formulas ******************************)
let reset = IVar (Ident ("reset", Bool))
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

let counterGsteSpec = Graph (vectexI , edgeL, antOfCounter, consOfCounter)

let antOfCounter_bool e = 
	termForm2bitForm (antOfCounter e)

let consOfCounter_bool e =
	termForm2bitForm (consOfCounter e)
	
let () =
	(*
	List.iter (
				fun e -> (
					let f = nodeToInt (source e) in
					let t = nodeToInt (sink e) in
					Printf.printf "Edge (%d, %d)'s boolean antecedent is: %s\n"  f t (print_form (antOfCounter_bool e));
					Printf.printf "Edge (%d, %d)'s boolean consequent is: %s\n\n"  f t (print_form (consOfCounter_bool e))
				)
			) edgeL
	
	*)
	List.iter (
				fun e -> (
					let f = nodeToInt (source e) in
					let t = nodeToInt (sink e) in
					Printf.printf "Edge (%d, %d)'s boolean antecedent is: %s\n"  f t (trajForm2str (bitForm2trajForm (antOfCounter_bool e)));
					Printf.printf "Edge (%d, %d)'s boolean consequent is: %s\n\n"  f t (trajForm2str (bitForm2trajForm (consOfCounter_bool e)))
				)
			) edgeL





(***************** tools to translate a integer to boolean vectors ********************)
(**
	e.g. 
	translate 3 into 2-bit boolean vector:
	[true; true]

let decToBin_helper = function 
  | 1 -> true
  | 0 -> false
  | _ -> raise (Failure "jackass")
    
let decToBin x =
  let rec d2b y lst = 
    match y with 
    |0 -> lst
    |_ -> d2b (y/2) ((decToBin_helper (y mod 2))::lst)
  in
  d2b x [] 

let intToBinVec value size =
  let res = Array.make size false in
  let bin = List.rev (decToBin value) in
  let length = List.length bin in 
  let rec make_list_from_n n =
    match n with
    | 0 -> []
    | m -> ((make_list_from_n (m-1))@[(m-1)])
  in
  let indexes = make_list_from_n length in 
  List.iter (fun (v, i) -> Array.set res (size-1-i) v) (List.combine bin indexes);
  Array.to_list res
(*******************************************************)
*)