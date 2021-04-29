open Types
open Z3Utils
open ToForte
open Trajectory
open Tools
(* ring buffer *)

(** constant definition *)
let depth = 4
let last = depth - 1
let data_size = 2
let index_size = 2

(** main assertion graph *)
let vertexI = Vertex 0
let vertexL = vertexI :: ((Vertex 1):: (List.map (fun i -> Vertex i) (upt 3 (2*last+4))))
(** odd-vertex selfloop edge, odd-vertex bidirection edge, odd-vertex to even-vertex edge, even-vertex backward edge , Vertex 4 to Vertex 1 edge*)
let edgeL = [Edge (vertexI,(Vertex 1))] @ 
            (List.map (fun i -> Edge (Vertex (2*i+1), Vertex (2*i+1)))  (upt 0 depth))@
            (List.map (fun i -> Edge (Vertex (2*i-1), Vertex (2*i+1)))  (upt 1 depth))@
            (List.map (fun i -> Edge (Vertex (2*i+1), Vertex (2*i-1)))  (dwt depth 1))@
            (List.map (fun i -> Edge (Vertex (2*i-1), Vertex (2*i+2)))  (upt 1 depth))@
            (List.map (fun i -> Edge (Vertex (2*i+2), Vertex (2*i)))    (dwt depth 2))@
			[Edge ((Vertex 4), (Vertex 1))]


(** actions of assertion graph *)
let rst  	 : expression = IVar (Ident ("rst", Bool)) 
let push 	 : expression = IVar (Ident ("push", Bool))
let pop  	 : expression = IVar (Ident ("pop", Bool))
let empty    : expression = IVar (Ident ("empty", Bool))
let full 	 : expression = IVar (Ident ("full", Bool))
let dataIn   : expression = IVar (Ident ("dataIn", Int data_size))
let dataOut  : expression = IVar (Ident ("dataOut", Int data_size))
let low 	 : expression = Const (BoolC false) 
let high 	 : expression = Const (BoolC true)
let readDataIn  :  expression = Const (IntC (1, data_size))
let symbolDataIn : expression = Const (SymbIntC ("din", data_size))
let symbolDataOut :  expression = Const (SymbIntC ("dout", data_size))

let rstFormula = Eqn (rst, high)
let nrstFormula = Eqn (rst, low)
let pushFormula =  AndForm (nrstFormula, AndForm (Eqn (push, high), Eqn (pop, low)))
let popFormula  = AndForm (nrstFormula, AndForm (Eqn (push, low), Eqn (pop, high)))
let noPopPushFormula = AndForm (nrstFormula, AndForm(Eqn (push,low), Eqn (pop, low)))
let fullFormula = Eqn (full, high)
let noFullFormula = Eqn (full, low)
let emptyFormula = Eqn (empty, high)
let noEmptyFormula = Eqn (empty, low)

let pushData data = AndForm (pushFormula, Eqn (dataIn, data))
let popData data = Eqn (dataOut, data)
    
let antOfRbFIFO aEdge = 
  let f = nodeToInt (source aEdge) in
  let t = nodeToInt (sink aEdge)   in
  (
    if( f == 0) then rstFormula
    else (
		if (f mod 2 == 1 ) then (
			if (f = t) then noPopPushFormula
			else if ((f + 2) == t) then pushFormula
			else if ( f == (t+2)) then popFormula
			else pushData symbolDataIn
		)else popFormula
	)
  )

let consOfRbFIFO aEdge = 
	let f = nodeToInt (source aEdge) in
	let t = nodeToInt (sink aEdge)   in
	(
		if ((f mod 2 == 1)&&(t mod 2 == 1)) then (
			if (f == 1) then AndForm (emptyFormula, noFullFormula)
			else if (f == (2*depth+1)) then AndForm (noEmptyFormula, fullFormula)
			else AndForm (noEmptyFormula, noFullFormula)
		)else (
			if ((f == 4) && (t == 1)) then popData symbolDataIn
			else if (f == (2*depth+2))  then AndForm (noEmptyFormula, fullFormula)
			else if (f == 1) then AndForm (emptyFormula, noFullFormula)
			else if (f == 0) then Chaos
			else if (t == 4) then Chaos
			else AndForm (noEmptyFormula, noFullFormula)
		)  
	)
  
let rbfifoGsteSpec = Graph (vertexI, vertexL, edgeL, antOfRbFIFO, consOfRbFIFO)


(*************************************** tag invariant *************************************************)
let mem = Ident ("mem", Array (depth, (Int data_size)))
let head = IVar (Ident ("head", Int index_size))
let tail = IVar (Ident ("tail", Int index_size))

(*
smt
let read mem index = ()
let d = ()
Eqn (head, tail) :
	AndForm (Eqn (head<1>0(Bool) , tail<1>0(Bool)  ), AndForm (Eqn (head<0>0(Bool) , tail<0>0(Bool)  ), Chaos ))
	TAndList [ TAndList [ Is0 head<1>0; Is0 tail<1>0]; TAndList [ Is1 head<1>0; Is1 tail<1>0]; TAndList [ Is0 head<0>0; Is0 tail<0>0]; TAndList [ Is1 head<0>0; Is1 tail<0>0]]
Eqn (head, tail+2) => AndForm (Eqn (head , 2), Eqn (tail ,0))
let plus expr num = ()
Eqn (tail head) = ((...) OR (...) OR (...))
*)

(** tag invariants in CNF (conjunctive normal form) *)
let tag (Vertex n) = 
	if n==0 then TAGINV ([], [Chaos])
	else if n==1 then TAGINV ([head; tail], 
							[Eqn (tail, head); emptyFormula; noFullFormula]
						)
	else if (n mod 2) == 1 then (
		let i = n/2 in
		(	
			if (i<=last) then TAGINV (
								[head;tail],
								[Eqn (tail, plus head (number i index_size)); noEmptyFormula; noFullFormula]
			)
			else TAGINV (
				[head;tail],
				[Eqn (tail, head); noEmptyFormula; fullFormula]
			)
		)
	)
	else (
		let i = (n-4)/2 in
		(
			if(i==last) then TAGINV (
								[head;tail],
								[Eqn (readArray mem (plus head (number last index_size)), symbolDataIn); Eqn (tail, head); noEmptyFormula; fullFormula]
			) 
			else if(i==0) then TAGINV(
								[head;tail],
								[Eqn (readArray mem head, symbolDataIn); Eqn (tail, plus head (number 1 index_size)); noEmptyFormula; noFullFormula]
							)
			else TAGINV (
						[head;tail],
					    [Eqn (readArray mem (plus head (number i index_size)), symbolDataIn); Eqn (tail, plus head (number (i+1) index_size)); noEmptyFormula; noFullFormula]
					)
		)
	)

(******************************************************************************************)

let print_single_traj_f () =
    let bit_f = termForm2bitForm (pushData readDataIn) in 
	let traj_f = bitForm2trajForm bit_f in
	print_endline (trajForm2str traj_f)


let binNodes = [head;tail]
(*
let test1 () =
	(*print_single_traj_f ();*)
	(*toFL rbfifoGsteSpec "rbfifo" binNodes*)
	let ctx = make_z3_context () in
	let nodes = [Vertex 0; Vertex 1; Vertex 3; Vertex 5; Vertex 7; Vertex 9] in
	List.iter (fun anode -> (
		let TAGINV (vars, forms) = tag anode in
		let z3ExprOfForm = and_all_exprs ctx (List.map (fun f -> form2z3Expr ctx f) forms) in
		let args = List.map (fun f -> expr2z3Expr ctx f) vars in
		get_all_models ctx z3ExprOfForm args
		(*print_endline (z3exprToString z3ExprOfForm)*)
	)) nodes

let test2() =
	let ctx = make_z3_context () in
	let nodes = [Vertex 1;Vertex 3; Vertex 5; Vertex 7; Vertex 9] in
	List.iter (fun anode -> (
		let TAGINV (vars, forms) = tag anode in
		getAllModels ctx forms vars
	)) nodes
*)
let test3() =
	let ctx = make_z3_context () in
	let nodes = [Vertex 4] in
	List.iter (fun anode -> (
		let TAGINV (vars, forms) = tag anode in
		let concreteList = getAllModels ctx forms vars in
		let concreteBitForms = List.map (fun sublist -> (
			List.map (fun form -> tag_inv_to_bit_form form vars sublist) forms 
		)) concreteList in
		List.iter (fun concreteForms -> (
			List.iter (fun f -> Printf.printf "%s  " (bitForm2str f)) concreteForms;
			print_endline ""
		))concreteBitForms
		(*
		List.iter (fun sublist -> 
			(List.iter (fun elem -> Printf.printf "%d" elem) sublist; print_endline "" )
		) concreteList
		*)
	)) nodes

let () =
	toSTEfl "rbfifo" rbfifoGsteSpec tag
	
	