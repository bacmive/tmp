open Types
open Tools
module Solver = Z3.Solver
module Boolean = Z3.Boolean
module Expr =  Z3.Expr
module Z3Array =  Z3.Z3Array
module Arithmetic = Z3.Arithmetic
module Integer = Arithmetic.Integer
module BitVector = Z3.BitVector
module Model = Z3.Model
open Printf

exception InvalidExpression
exception UnfoundFunction
exception UnMatchedExpr
exception UnMatchedIVar
exception UnMatchedPara
exception UnMatchedIndex
exception UnMatchedUIF

let make_z3_context () =
	Z3.mk_context  [("model", "true"); ("proof", "false")]

let and_all_exprs ctx exprs =
	Boolean.mk_and ctx exprs

let z3exprToString expr =
	Expr.to_string expr

(** term-level expression and formula To SMT's Expr *)
let rec expr2z3Expr (ctx:Z3.context) (e : expression)  = 
	match e with 	
	| IVar v -> (
				match v with 
				| Ident (str, _sort) -> (
					match _sort with 
					| Int size -> Expr.mk_const_s ctx str (BitVector.mk_sort ctx size)
					| Bool -> Boolean.mk_const_s ctx str
					| Array (index_size, elem_sort) -> (
						match elem_sort with 
						| Int elem_size -> Z3Array.mk_const_s ctx str (BitVector.mk_sort ctx index_size) (BitVector.mk_sort ctx elem_size)
						| Bool -> Z3Array.mk_const_s ctx str (BitVector.mk_sort ctx index_size) (Boolean.mk_sort ctx )
						| _ -> raise (Invalid_argument "In expr2z3Expr IVar v: not support nested array")
					)
				)
				| Para (varr, expr) -> (
					match varr, expr with
					|(Ident (str, Array(length, Int data_size)), Const (IntC (index_value, index_size))) -> (
						Z3Array.mk_select ctx 
							(Z3Array.mk_const_s ctx str (BitVector.mk_sort ctx index_size) (BitVector.mk_sort ctx data_size))
							(Expr.mk_numeral_int ctx index_value (BitVector.mk_sort ctx index_size))
					)
					|(Ident (str, Array (length, Bool)), Const (IntC (index_value, index_size))) -> (
						Z3Array.mk_select ctx
							(Z3Array.mk_const_s ctx str (BitVector.mk_sort ctx index_size) (Boolean.mk_sort ctx))
							(Expr.mk_numeral_int ctx index_value (BitVector.mk_sort ctx index_size))
					)
					|_ -> raise UnMatchedPara
				)
				(*| _ -> raise UnMatchedIVar*)
			)
	| Const s -> ( 
				match s with 
				| IntC (value, size) ->  Expr.mk_numeral_int ctx value (BitVector.mk_sort ctx size)
				| BoolC b -> if b then Boolean.mk_true ctx else Boolean.mk_false ctx
				(*
				| SymbIntC (str, size) -> 
				| SymbBoolC str ->
				*)
				|_-> raise (Invalid_argument "In expr2z3Expr: do not support symbolic constant")
			)
	| Uif (str, exprs)-> (
							match exprs with 
							h1::h2::[] -> BitVector.mk_add ctx (expr2z3Expr ctx h1) (expr2z3Expr ctx h2)
							| _ -> raise UnMatchedUIF
						)	
	| IteForm (f, e1, e2) -> Boolean.mk_ite ctx (form2z3Expr ctx f) ( expr2z3Expr ctx e1) (expr2z3Expr ctx e2)
	(*| _ -> raise UnMatchedExpr*)
and form2z3Expr (ctx:Z3.context) (f : formula)  =
	match f with 
	Eqn (e1, e2) -> Boolean.mk_eq ctx (expr2z3Expr ctx e1) (expr2z3Expr ctx e2)
	| AndForm (f1, f2) -> Boolean.mk_and ctx [(form2z3Expr ctx f1); (form2z3Expr ctx f2)]
	| Neg f -> Boolean.mk_not ctx (form2z3Expr ctx f)
	| OrForm (f1, f2) -> Boolean.mk_or ctx [(form2z3Expr ctx f1); (form2z3Expr ctx f2)]
	| ImplyForm (f1, f2) -> Boolean.mk_implies ctx (form2z3Expr ctx f1) (form2z3Expr ctx f2)
	| Chaos -> Boolean.mk_true ctx
	(*
	| Uip (str, expr) -> (
							match expr with
							e1::e2::[] -> BitVector.mk_ule ctx (expr2z3Expr ctx e1) (expr2z3Expr ctx e2)
							| _ -> raise InvalidExpression
						)
	*)
	(*|_ -> raise (Invalid_argument "In form2z3Expr: do not support uninterpreted predicate")*)
	
(** the SET of Z3 Expr *)
module ExprSet = Set.Make(
	struct
	let compare = Pervasives.compare
	type t = Expr.expr
	end
)

(**
	@param 1 A BitVector
	@return the int equal to that bitvector
	@example bv2int #b11 => 3
*)
let bv2int bv=
	let str_bv = Expr.to_string bv in
	let intarr = List.map 
				(fun i -> ((Char.code (String.get str_bv i)) - (Char.code '0'))) 
				(upt 2 ((String.length str_bv)-1)) 
	in
	List.fold_left (fun a b -> (2*a + b)) 0 intarr
	

(** 
	@param 1 Z3 context
	@param 2 Expr to be solved
	@param 3 Argument to be solved
	@return a list of list , each sublist representing a concretization of args
 *)
let get_all_models (ctx: Z3.context) (expr: Expr.expr) (args_of_expr:Expr.expr list) =
	let slvr = Solver.mk_solver ctx None in
	let rec get_helper (extra_constraints : Expr.expr list) = 
		Solver.add slvr extra_constraints;
		ignore (Solver.check slvr []); 
		match Solver.get_model slvr with
		Some model -> (
			let new_constraints = List.map (fun e -> 
												match Model.eval model e true with
												| Some ee -> (
														let pre_model = Boolean.mk_and ctx [(BitVector.mk_ule ctx e ee); (BitVector.mk_uge ctx e ee)] in
														Boolean.mk_not ctx pre_model 
													)	
												| None -> raise InvalidExpression
											) (List.filter (fun arg -> Expr.is_const arg) args_of_expr) 
			in
			let one_model = List.map (fun a -> ( 
												match Model.eval model a true with
												| Some av -> bv2int av
												| None -> raise InvalidExpression
											) 
									) args_of_expr
			in 
			one_model::(get_helper new_constraints)
		)
		| None -> []
	in 
	Solver.add slvr [expr];
	get_helper []
	(*
	let res = get_helper [] in
	List.iter (fun sublist -> 
			(List.iter (fun elem -> Printf.printf "%d" elem) sublist; print_endline "" )
		) res
	*)
	(*
	Printf.printf "For %s:\n" (Expr.to_string expr); 
	List.iter (fun aL -> ( List.iter (fun bT -> (let (var, value) = bT in Printf.printf "%s = %s  " var value) ) aL ;
				Printf.printf "\n" )
	) res
	*)


(** 
	Simply a wrapper for <i>get_all_models</i>
	@param 1 z3 context
	@param 2 a CNF of one assertion graph node (representd by a list of formula)
	@param 3 the argument to be concritized in CNF
	@return a list of list int for concretization
*)
let getAllModels ctx (tagForms : formula list) (args : expression list) =
	let expr_to_solve = and_all_exprs ctx (List.map (fun f -> form2z3Expr ctx f) tagForms ) in
	let args_to_solve = List.map ( fun e -> expr2z3Expr ctx e ) args in
	let concretization = get_all_models ctx expr_to_solve args_to_solve in
	concretization
	(*
	let models = () in (*map list : based on expr_to_solve *) 
	if (contain_args f) then ()
	else ()
	()
	*)