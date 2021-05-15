open Types
open Tofortegste
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

let () =
	toFL counterGsteSpec "counter" [] 