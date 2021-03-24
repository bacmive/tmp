(********************************* Types ********************************************)


(** Constant Type*)
type scalar = 
	| DataC of int 
	| IndexC of int (* int * int *)
	| BoolC of bool
	(*
		| TopVal 
		| BottomVal
	*)

(** Variable Type*)
type var = (* symbol *)
	| BoolV of string 
	| DataV  of string
	| IndexV of string
	| ArrayV of string
	| ParaV of var * scalar  
	(* | Field of var * string *)

(* expression and formula *)
type expression = 
    | IVar of var 
    | Const of scalar (* Boolean.mk_true / mk_false *)
    | IteForm of formula * expression * expression
    | Uif of string * expression list
    | Top
    | Unknown
and formula = 
    | Eqn of expression * expression 
    | Uip of string * expression list 
    | AndForm of formula * formula 
    | Neg of formula 
    | OrForm of formula * formula 
    | ImplyForm of formula * formula 
    | Chaos

(* case expression *)
type formulaExpPair = formula * expression

let rec caseExpression : formulaExpPair list -> expression = function
  | [] -> Unknown
  | (f, e)::t -> IteForm (f, e, (caseExpression t))


(* assignment *)
type assign = Assign of var * expression

(* statement *)
type statement =
  | Parallel of assign list
  | If of formula * statement * statement

(* case statement *)
type formulaStaPair = formula * statement

let skip = Parallel []

let rec caseStatement : formulaStaPair list -> statement = function
  | [] -> skip
  | (f, gS)::t -> If (f, gS, (caseStatement t))

(** auxiliary tools: down and upt*)
let rec down : int -> int list = function 
	| 0 -> [0]
	| n -> (down (n-1))@[n]

let rec upt (f : int) (t : int) : int list =
	if f > t then []
	else f :: upt (f+1) t
	
(** Array Manipulation with Expression and Statement*)
let readArray (v : var) (bound : int) (e : expression) : expression =
	caseExpression (List.map (fun index -> (Eqn (e, (Const (IndexC index))), IVar (ParaV (v, IndexC index)))) (down bound) )

let writeArray (v : var) (bound : int) (addressE : expression) (ce : expression) : assign list =
	List.map 
        (fun i -> Assign((ParaV(v, IndexC i)), IteForm (Eqn (addressE, Const (IndexC i)), ce, (IVar (ParaV (v, IndexC i)))))) 
        (down bound)

(*********************************** GSTE assertion graph *******************************************)
type node = Vertex of int
type edge = Edge of node * node

type edgeToFormula = edge -> formula
(* gste Graph *)
type gsteSpec = Graph of node * edge list * edgeToFormula * edgeToFormula

(** the source node of an edge and the sink node of an edge*)
let source : edge -> node = function
    | Edge (n, _) -> n

let sink : edge -> node = function
    | Edge (_, n) -> n

(** retrive the int of a node *)
let nodeToInt : node -> int = function
	| Vertex n -> n


(*********************************** rbFIFO GSTE assertion graph *******************************************)
let mem : var = ArrayV "mem"

let rst : expression = IVar (BoolV "reset")
let push : expression = IVar (BoolV "push")
let pop : expression = IVar (BoolV "pop")
let dataIn : expression = IVar (DataV "dataIn")
let low : expression = Const (BoolC false)
let high : expression = Const (BoolC true)
let empty : expression = IVar (BoolV "empty")
let full : expression = IVar (BoolV "full")
let tail : expression = IVar (IndexV "tail")
let head : expression = IVar (IndexV "head")

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
	| d -> AndForm (pushFormula, Eqn (dataIn, Const (DataC d)))

let popDataFormula (d : int)  (depth : int) : formula = 
	Eqn ((dataOut depth), Const (DataC d)) 

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
	else Uif ("+", [applyPlusN e (n-1) ; Const (IndexC 1)])

let tagFunOfRbFifo (d : int) (n : node) : formula list = 
	let dataI = Const (DataC d) in 
	let lastV = Const (IndexC last) in 
	let x = nodeToInt n in
	(
		if (x=0) then []
		else 
		(
			if ( (x mod 2)=1 ) then 
			(
				if ( x=1 ) then [ Eqn (tail, head); Eqn (empty, high); Eqn (full, low); Uip ("le", [head; lastV]) ] 
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


(*********************************** rbFIFO GSTE SMT solve *******************************************)
(** expression and formula To SMT's Expr *)

open Z3
open Z3.Solver
open Z3.Boolean
open Z3.Expr
open Z3.Z3Array
open Z3.Arithmetic
open Printf

exception InvalidExpression
exception UnfoundFunction
exception UnMatchedExpr
exception UnMatchedArrayV
exception UnMatchedIndexC
exception UnMatchedUIF

let rec expr2z3Expr (ctx:Z3.context) (e : expression)  = 
	match e with 
	IVar v -> (
				match v with 
				BoolV str -> Boolean.mk_const_s ctx "str"
				| DataV str -> Expr.mk_const_s ctx str (BitVector.mk_sort ctx 1)
				| IndexV str -> Expr.mk_const_s ctx str (BitVector.mk_sort ctx 2)
				| ArrayV str -> Z3Array.mk_const_s ctx str (BitVector.mk_sort ctx 2) (BitVector.mk_sort ctx 1)
				| ParaV (varr, sclr) -> (
											match varr with 
											ArrayV str -> (
															match sclr with
															IndexC index -> Z3Array.mk_select ctx 
																		(Z3Array.mk_const_s ctx str (BitVector.mk_sort ctx 2) (BitVector.mk_sort ctx 1))
																		(Expr.mk_numeral_int ctx index (BitVector.mk_sort ctx 2))
															|_ -> raise UnMatchedIndexC
														)
											|_ -> raise UnMatchedArrayV					
										)
			)
	| Const s -> ( 
				match s with 
				DataC d ->  Expr.mk_numeral_int ctx d (BitVector.mk_sort ctx 1)
				| IndexC i -> Expr.mk_numeral_int ctx i (BitVector.mk_sort ctx 2)
				| BoolC b -> if b then Boolean.mk_true ctx else Boolean.mk_false ctx
			)
	| Uif (str, expr)-> (
							match expr with 
							h1::h2::[] -> BitVector.mk_add ctx (expr2z3Expr ctx h1) (expr2z3Expr ctx h2)
							| _ -> raise UnMatchedUIF
						)	
	| IteForm (f, e1, e2) -> Boolean.mk_ite ctx (form2z3expr ctx f) ( expr2z3Expr ctx e1) (expr2z3Expr ctx e2)
	| Unknown -> 
	| _ -> raise UnMatchedExpr
and form2z3expr (ctx:Z3.context) (f : formula)  =
	match f with 
	Eqn (e1, e2) -> Boolean.mk_eq ctx (expr2z3Expr ctx e1) (expr2z3Expr ctx e2)
	| AndForm (f1, f2) -> Boolean.mk_and ctx [(form2z3expr ctx f1); (form2z3expr ctx f2)]
	| Neg f -> Boolean.mk_not ctx (form2z3expr ctx f)
	| OrForm (f1, f2) -> Boolean.mk_or ctx [(form2z3expr ctx f1); (form2z3expr ctx f2)]
	| ImplyForm (f1, f2) -> Boolean.mk_implies ctx (form2z3expr ctx f1) (form2z3expr ctx f2)
	| Chaos -> Boolean.mk_true ctx
	| Uip (str, expr) -> (
							match expr with
							e1::e2::[] -> BitVector.mk_ule ctx (expr2z3Expr ctx e1) (expr2z3Expr ctx e2)
							| _ -> raise InvalidExpression
						)

(** GSTE assertion graph NODE to formula(tag function) *)
let tag (ctx : Z3.context) (d : int) (n : node)  = 
	match tagFunOfRbFifo d n with
	[] -> Boolean.mk_true ctx
	|t -> Boolean.mk_and ctx (List.map (fun f -> form2z3expr ctx f) t)
	

(** GSTE assertion graph and tag function concretization *)
(** concretize/solve one edge -> check out against graph *)

let assertions (ctx : Z3.context) = 
	let zero : Expr.expr = Arithmetic.Integer.mk_numeral_i ctx 0 in
	let three : Expr.expr = Arithmetic.Integer.mk_numeral_i ctx 3 in
	let x : Expr.expr = Arithmetic.Integer.mk_const_s ctx "x" in
	let y : Expr.expr = Arithmetic.Integer.mk_const_s ctx "y" in
	[Arithmetic.mk_ge ctx x zero; Arithmetic.mk_le ctx x three; Arithmetic.mk_ge ctx x y; Arithmetic.mk_le ctx x y]

let exprOfAssertions (ctx : Z3.context) = 
	[ Arithmetic.Integer.mk_const_s ctx "x"; Arithmetic.Integer.mk_const_s ctx "y"]

(** get_all_models *)
let models () =
	let ctx = Z3.mk_context [("model", "true"); ("proof", "false")] in
	let slvr = Solver.mk_solver ctx None in
	let get_all_models (c : Z3.context) (s : Solver.solver) = 
		Solver.add s (assertions c); 
		Printf.printf "%s\n" (Solver.string_of_status (Solver.check s [])); (* check before get models *)
		match Solver.get_model s with
		| Some m -> [m;]
		| None ->  []
	in 	
	match get_all_models ctx slvr with
	| t::[] -> (
			let res = List.map (fun e -> Model.eval t e true) (exprOfAssertions ctx) in
			let rec print_list = function
			[] -> ()
			| e::l -> (
					match e with 
					|Some ee -> Printf.printf "%s\n" (Expr.to_string ee); print_list l
					|None -> Printf.printf "wrong\n"
				)
			in 
			print_list res 
		)
	| _ -> raise InvalidExpression
	
let models2 () =
	let ctx = Z3.mk_context [("model", "true"); ("proof", "false")] in
	let slvr = Solver.mk_solver ctx None in
	let get_all_models (c : Z3.context) (s : Solver.solver) (extra_constraints : Expr.expr list) = 
		ignore (Solver.check s extra_constraints); 
		match Solver.get_model s with
		Some model -> (
					let res = List.map (fun e -> Model.eval model e true) (exprOfAssertions c) in
					let rec print_list = function
					[] -> ()
					| h::t -> (
							match h with 
							|Some hh -> Printf.printf "%s\n" (Expr.to_string hh); print_list t
							|None -> Printf.printf "wrong\n"
						)
					in 
					print_list res;
					() (* TODO *)
					
				)
		| None -> ()
	in
	Solver.add slvr (assertions ctx); 
	get_all_models ctx slvr []


let models3 () =
	let ctx = Z3.mk_context [("model", "true"); ("proof", "false")] in
	let slvr = Solver.mk_solver ctx None in
	let rec get_all_models (c : Z3.context) (s : Solver.solver) (extra_constraints : Expr.expr list) = 
		Solver.add s extra_constraints;
		ignore (Solver.check s []); 
		match Solver.get_model s with
		Some model -> (
			let new_constraints = List.map (fun e -> ( 
												match Model.eval model e true with
												| Some ee -> (
														let pre_model = Boolean.mk_and ctx [(Arithmetic.mk_le ctx e ee); (Arithmetic.mk_ge ctx e ee)] in
														(* Printf.printf "(%s, %s)\n" (Expr.to_string e) (Expr.to_string ee); *)
														Boolean.mk_not ctx pre_model 
													)
												| None -> raise InvalidExpression
											)
										) (exprOfAssertions ctx) 
			in
			let one_model = List.map (fun e -> ( 
												match Model.eval model e true with
												| Some ee -> ((Expr.to_string e), (Expr.to_string ee))
												| None -> raise InvalidExpression
											) 
									) (exprOfAssertions ctx)
			in 
			one_model::(get_all_models c s new_constraints)
		)
		| None -> []
	in
	Solver.add slvr (assertions ctx);
	get_all_models ctx slvr [] 	


let solves () =
	let ctx = Z3.mk_context [("model", "true"); ("proof", "false")] in
	let slvr = Solver.mk_solver ctx None in
	Solver.add slvr (assertions ctx); 
	Printf.printf "%s\n" (Solver.string_of_status (Solver.check slvr [])); (*  check before get models *)
	match Solver.get_model slvr with
	| Some m -> Printf.printf "%s\n" (Model.to_string m)
	| None -> Printf.printf "no model\n"

(*
let () = 	
	let rec prt vls = 
		match vls with
		[] -> ()
		| (Vertex i) :: t -> print_endline (string_of_int i);  prt t 
	in
	prt vectexL;

let () =
	let res = models3 () in
	let print_list lt = List.iter (fun (x, y) -> Printf.printf "(%s, %s) " x y ) lt in
	List.iter (fun lt -> print_list lt; print_endline "" ) res 
*)
let () =
	let nodes = List.map (fun x -> (Vertex x)) ([0; 1]@(upt 3 (2*3+4))) in
	let ctx = Z3.mk_context [("model", "true"); ("proof", "false")] in
	List.iter (fun x -> Printf.printf "%s\n" (Expr.to_string (tag ctx 1 x))) nodes
	(*
	let ctx = Z3.mk_context [("model", "true"); ("proof", "false")] in
	let slvr = Solver.mk_solver ctx None in
	let rec get_all_models (c : Z3.context) (s : Solver.solver) (extra_constraints : Expr.expr list) = 
		Solver.add s extra_constraints;
		ignore (Solver.check s []); 
		match Solver.get_model s with
		Some model -> (
			let new_constraints = List.map (fun e -> ( 
												match Model.eval model e true with
												| Some ee -> (
														let pre_model = Boolean.mk_and ctx [(Arithmetic.mk_le ctx e ee); (Arithmetic.mk_ge ctx e ee)] in
														(* Printf.printf "(%s, %s)\n" (Expr.to_string e) (Expr.to_string ee); *)
														Boolean.mk_not ctx pre_model 
													)
												| None -> raise InvalidExpression
											)
										) (exprOfAssertions ctx) 
			in
			let one_model = List.map (fun e -> ( 
												match Model.eval model e true with
												| Some ee -> ((Expr.to_string e), (Expr.to_string ee))
												| None -> raise InvalidExpression
											) 
									) (exprOfAssertions ctx)
			in 
			one_model::(get_all_models c s new_constraints)
		)
		| None -> []
	in
	Solver.add slvr (assertions ctx);
	get_all_models ctx slvr [] 	
	*)


	