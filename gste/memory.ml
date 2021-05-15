open Types
open Tofortegste
open Trajectory
open Tools
(* memory *)

let data_size = 8
let addr_size = 10
let depth = 1024

(** main assertion graph *)
let vertexI = Vertex 0
let vertexL = vertexI :: [Vertex 1; Vertex 2]
let edgeL = [Edge (vertexI, (Vertex 1)); Edge ((Vertex 1), (Vertex 1)); Edge ((Vertex 1), (Vertex 2))]

(** actions of assertion graph *)
let wr 		: expression = IVar (Ident ("wr", Bool))
let low 	: expression = Const (BoolC false) 
let high 	: expression = Const (BoolC true)
let addrIn 	: expression = IVar (Ident ("addr", Int addr_size))
let dataIn  : expression = IVar (Ident ("din", Int data_size))
let dataOut : expression = IVar (Ident ("dout", Int data_size))
let symbolAddrIn : expression = Const (SymbIntC ("addrIn", addr_size))
let symbolDataIn : expression = Const (SymbIntC ("dataIn", data_size))

let write = Eqn (wr, high)
let read =  Eqn (wr, low)

let writeDataAt d a = AndForm (write, AndForm(Eqn (dataIn, d), Eqn (addrIn, a)))
let readAt a = AndForm (read, Eqn (addrIn, a))
let lastReadDataIs d = Eqn (dataOut, d)

let antOfMemory aEdge = 
	let f = nodeToInt (source aEdge) in
	let t = nodeToInt (sink aEdge) in
	(
		if ((f ==0) && (t ==1)) then writeDataAt symbolDataIn symbolAddrIn
		else if ((f==1) && (t==1)) then read
		else readAt symbolAddrIn
	)

let consOfMemory aEdge =
	let f = nodeToInt (source aEdge) in
	let t = nodeToInt (sink aEdge) in
	(
		if ((f ==0) && (t ==1)) then Chaos
		else if ((f==1) && (t==1)) then Chaos
		else lastReadDataIs symbolDataIn
	)

let memoryGsteSpec = Graph (vertexI, vertexL, edgeL, antOfMemory, consOfMemory)
	
let () =
	toFL memoryGsteSpec "memory" []
			