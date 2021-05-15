open Types
open Tofortegste
open Trajectory
open Tools

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

let mem 	 : var= Ident ("mem", Array (depth, (Int data_size)))
let rst  	 : expression = IVar (Ident ("rst", Bool)) 
let push 	 : expression = IVar (Ident ("push", Bool))
let pop  	 : expression = IVar (Ident ("pop", Bool))
let empty    : expression = IVar (Ident ("empty", Bool))
let full 	 : expression = IVar (Ident ("full", Bool))
let dataIn   : expression = IVar (Ident ("dataIn", Int data_size))
let dataOut   : expression = IVar (Ident ("dataOut", Int data_size))
let low 	 : expression = Const (BoolC false) 
let high 	 : expression = Const (BoolC true)
let symbolDataIn : expression = Const (SymbIntC ("din", data_size))
let head 	 : expression = IVar (Ident ("head", Int index_size))
let tail 	 : expression = IVar (Ident ("tail", Int index_size))

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
			else AndForm (noEmptyFormula, noFullFormula)
		)  
	)

let rsfifoGsteSpec = Graph (vertexI, vertexL, edgeL, antOfRbFIFO, consOfRbFIFO)

let head = IVar (Ident ("head", Int index_size))
let tail = IVar (Ident ("tail", Int index_size))
let binNodes = [head; tail]

let () =
	toFL "rsfifo" rsfifoGsteSpec binNodes