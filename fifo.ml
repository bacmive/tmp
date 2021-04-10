open ToForte
open Trajectory
open Tools
(* ring buffer *)

(** constant definition *)
let depth = 16
let last = depth - 1
let data_length = 4
let index_length = 4

(** main assertion graph *)
let vertexI = Vertex 0
let vertexL = vertexI :: ((Vertex 1):: (List.map (fun i -> Vertex i) (upt 3 (2*last+4))))
let edgeL = [Edge (vertexI,(Vertex 1))] @ 
            (List.map (fun i -> Edge (Vertex (2*i+1), Vertex (2*i+1)))  (upt 0 depth))@
            (List.map (fun i -> Edge (Vertex (2*i+2), Vertex (2*i+2)))  (upt 1 depth))@
            (List.map (fun i -> Edge (Vertex (2*i+1), Vertex (2*i-1)))  (dwt depth 1))@
            (List.map (fun i -> Edge (Vertex (2*i-1), Vertex (2*i+2)))  (upt 1 depth))@
            (List.map (fun i -> Edge (Vertex (2*i+2), Vertex (2*i)))    (dwt depth 1))


(** actions of assertion graph *)
let rst  	 : expression = IVar (Ident ("rst", Bool)) 
let push 	 : expression = IVar (Ident ("push", Bool))
let pop  	 : expression = IVar (Ident ("pop", Bool))
let empty    : expression = IVar (Ident ("empty", Bool))
let full 	 : expression = IVar (Ident ("full", Bool))
let dataIn   : expression = IVar (Ident ("dataIn", Int data_length))
let dataOut  : expression = IVar (Ident ("dataOut", Int data_length))
let low 	 : expression = Const (BoolC false) 
let high 	 : expression = Const (BoolC true)
let symbolDataIn  :  expression = Const (SymbIntC ("din", data_length))
let symbolDataOut :  expression = Const (SymbIntC ("dout", data_length))

let rstFormula = Eqn (rst, high)
let nrstFormula = Eqn (rst, low)
let pushFormula =  AndForm (nrstFormula, AndForm (Eqn (push, high), Eqn (pop, low)))
let popFormula  = AndForm (nrstFormula, AndForm (Eqn (push, low), Eqn (pop, high)))
let noPopPushFormula = AndForm (nrstFormula, AndForm(Eqn (push,low), Eqn (pop, low)))
let fullFormula = Eqn (full, high)
let noFullFormula = Eqn (full, low)
let emptyFormula = Eqn (empty, high)
let noEmptyFormula = Eqn (empty, low)

let pushData data = ()
let popData data = ()
    
let antOfRbFIFO aEdge = 
  let f = nodeToInt (source e) in
  let t = nodeToInt (sink e)   in
  (
    if( f == 0) then rstFormula
    else (
		if (f mod 2 == 1 ) then (
			if (f = t) then noPopPushFormula
			else if ((f + 2) == t) then pushFormula
			else if ( f == (t+2)) then popFormula
			else pushData vOfDataIn
		)else popFormula
	)
  )

let consOfRbFIFO aEdge = 
	let f = nodeToInt (source e) in
	let t = nodeToInt (sink e)   in
	(
		if ((f mod 2 == 1)&&(t mod 2 == 1)) then (
			if (f == 1) then AndForm (emptyFormula, noFullFormula)
			else if (f == (2*depth+1)) then AndForm (noEmptyFormula, fullFormula)
			else AndForm (noEmptyFormula, noFullFormula)
		)else (
			if (t == 2)  then popData vOfDataIn
			else Chaos
		)  
	)
  
let rbfifoGsteSpec = Graph (vertexI, vertexL, edgeL, antOfRbFIFO, consOfRbFIFO)

let () =
  List.iter (fun (Vertex i) -> print_int i) vertexL