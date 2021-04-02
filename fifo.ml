open Types
open Z3Utils

let index_size = 2
let data_size = 2

(** Array Manipulation with Expression and Statement*)
let readArray (v : var) (bound : int) (e : expression) : expression =
	caseExpression (List.map (fun index -> 
							(Eqn (e, (Const (IntC (index, index_size)))), IVar (Para (v, Const (IntC (index, index_size)))))
						) (down bound) 
				)

let writeArray (v : var) (bound : int) (addressE : expression) (ce : expression) : assign list =
	List.map 
        (fun i -> Assign((Para (v, Const(IntC(i, index_size)))), 
						IteForm (Eqn (addressE, Const (IntC (i, index_size))), ce, (IVar (Para (v, Const (IntC (i, index_size))))))
					)
	)(down bound)
	
let mem : var = Ident ("mem", Array (index_size, data_size))

let rst : expression = IVar (Ident ("reset", Bool)) 
let push : expression = IVar (Ident ("push", Bool))
let pop : expression = IVar (Ident ("pop", Bool))
let dataIn : expression = IVar (Ident ("dataIn", Int data_size)) (*Const (IntC int * int) *)
let low : expression = Const (BoolC false) 
let high : expression = Const (BoolC true)
let empty : expression = IVar (Ident ("empty", Bool))
let full : expression = IVar (Ident ("full", Bool))
let tail : expression = IVar (Ident ("tail", Int index_size))
let head : expression = IVar (Ident ("head", Int index_size))

let fullFormula : formula = Eqn (full, high)
let rstFormula : formula = Eqn (rst, high)
let emptyFormula : formula = Eqn (empty, high)
let pushFormula : formula = AndForm (AndForm (Eqn (rst, low), Eqn (push, high)), Eqn (pop, low)) 
let popFormula : formula = AndForm (AndForm (Eqn (rst, low), Eqn (push, low)), Eqn (pop, high))
let noPopPushFormula : formula = AndForm (AndForm (Eqn (rst, low), Eqn (push, low)), Eqn (pop, low))
let noFullFormula :formula = Neg fullFormula
let noEmptyFormula : formula = Neg emptyFormula

let dataOut : int -> expression = function 
	| depth -> readArray mem depth head

let pushDataFormula : int -> formula = function 
	| d -> AndForm (pushFormula, Eqn (dataIn, Const (IntC (d, data_size))))

let popDataFormula (d : int)  (depth : int) : formula = 
	Eqn ((dataOut depth), Const (IntC (d, data_size))) 		

let last = 3
let vectexI = Vertex 0
let vectexL = [vectexI; (Vertex 1)]@(List.map (fun i -> Vertex i) (upt 3 (2*last+4)))

let edgeL = 
	let e1 = Edge (Vertex 0, Vertex 1) in
	let e2 = Edge (Vertex 1, Vertex 3) in 
	let e3 = Edge (Vertex 1, Vertex 4) in
	let e4_list = List.map (fun i -> Edge (Vertex (2*i+1), Vertex (2*i+1) )) (upt 0 (last+1)) in (*self loop*)
	let e5_list = List.map (fun i -> Edge (Vertex (2*i+2), Vertex (2*i+2) )) (upt 1 (last+1)) in (*self loop*)
	let e6_list = List.map (fun i -> Edge (Vertex (2*i+1), Vertex (2*i+3) )) (upt 1 (last)) in  
	let e7_list = List.map (fun i -> Edge (Vertex (2*i+1), Vertex (2*i+4) )) (upt 0 (last)) in
	let e8_list = List.map (fun i -> Edge (Vertex (2*i+3), Vertex (2*i+1) )) (upt 0 (last)) in
	let e9_list = List.map (fun i -> Edge (Vertex (2*i+4), Vertex (2*i+2) )) (upt 1 (last)) in
	let e10 = Edge (Vertex 4, Vertex 1) in
	[e1; e2; e3; e10]@e4_list@e5_list@e6_list@e7_list@e8_list@e9_list

let antOfRbFifo (d : int) (e : edge) : formula =
	let f = nodeToInt (source e) in
	let t = nodeToInt (sink e) in 
	(	
		if (f = 0) then rstFormula 
		else if (f = t) then noPopPushFormula 
		else if ((f mod 2)=1) then 
		(	
			if ((f + 2)=t) then pushFormula 
			else if (f=(t+2)) then popFormula
			else pushDataFormula d
		)
		else popFormula
	)

let consOfRbFifo (d : int) (e :edge) : formula =
	let f = nodeToInt (source e) in
	let t = nodeToInt (sink e) in
	(
		if ( (f mod 2) = 1 && (t mod 2) = 1 ) then 
		(
			if f = 1 then AndForm (emptyFormula, noFullFormula)
			else if f = (2*last +3) then AndForm (noEmptyFormula, fullFormula)
			else AndForm (noEmptyFormula, noFullFormula)
		)
		else if ( f = 4 && t = 1 ) then popDataFormula last d
		else if ( f = (2*last+4) ) then AndForm (noEmptyFormula, fullFormula)
		else if ( f = 1 ) then AndForm (emptyFormula, noFullFormula)
		else if ( f <> 0) then AndForm (noEmptyFormula, noFullFormula)
		else Chaos
	)
	
let rbFifoGsteSpec ( d : int ) : gsteSpec = 
	Graph (vectexI, edgeL, antOfRbFifo d, consOfRbFifo d)
	


(*********************************** rbFIFO GSTE tag function *******************************************)
let rec applyPlusN (e : expression) (n : int) : expression = 
	if n = 0 then e 
	else Uif ("+", [applyPlusN e (n-1) ; Const (IntC (1, index_size))])

let tagFunOfRbFifo (d : int) (n : node) : formula list = 
	let dataI = Const (IntC (d, data_size)) in 
	let lastV = Const (IntC (last, index_size)) in 
	let x = nodeToInt n in
	(
		if (x=0) then []
		else 
		(
			if ( (x mod 2)=1 ) then 
			(
				if ( x=1 ) then [Eqn (tail, head); Eqn (empty, high); Eqn (full, low); Uip ("le", [head; lastV])] 
				else 
				(
					if ( x=(2*last+3) ) then [Eqn (tail, head); Eqn (empty, low); Eqn (full, high); Uip ("le", [head; lastV])]
					else [Eqn (tail, applyPlusN head (x/2)); Eqn (empty, low); Eqn (full, low)]
				)
			)
			else
			(
				if( x=(2*last +4) ) then [Eqn (tail, head); Eqn (empty, low); Eqn (full, high); Eqn (readArray mem last (applyPlusN head last), dataI); Uip ("le", [head; lastV])]
				else [Eqn (tail, applyPlusN head (x/2-1)); Eqn (empty, low); Eqn (full, low); Eqn (readArray mem last (applyPlusN head (x/2-2)), dataI); Uip ("le", [head; lastV])]
			)
		)
	)


(** GSTE assertion graph NODE to Z3-API formula(tag function) *)
let tag (ctx : Z3.context) (d : int) (n : node)  = 
	match tagFunOfRbFifo d n with
	[] -> Boolean.mk_true ctx
	|t -> Boolean.mk_and ctx (List.map (fun f -> form2z3expr ctx f) t)


(** main function  *)
let () =
	let ctx = Z3.mk_context [("model", "true"); ("proof", "false")] in
	let nodes = List.map (fun x -> (Vertex x)) ([0; 1]@(upt 3 (2*3+4))) in 
	let asserts = List.map (fun i -> tag ctx 1 i) nodes in 
	(** get vars from an assertion 
	let get_vars (expr_list : Expr.expr list) = 
		List.fold_right ExprSet.union (List.map 
										(fun x -> List.fold_right ExprSet.add (Expr.get_args x) ExprSet.empty)
										expr_list
									) ExprSet.empty
		|> ExprSet.elements
	in *)
	Printf.printf "number of asserts: %d\n" (List.length asserts);
	List.iter (fun x -> if Boolean.is_true x  then ()
						else (
							let args_of_x = [expr2z3Expr ctx tail; expr2z3Expr ctx head] in 
							get_all_models ctx x args_of_x
						)
			) asserts
			


	

