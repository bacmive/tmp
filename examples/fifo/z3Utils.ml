open Types
module Solver = Z3.Solver
module Boolean = Z3.Boolean
module Expr =  Z3.Expr
module Z3Array =  Z3.Z3Array
module Arithmetic = Z3.Arithmetic
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

(** trajectory expression and formula To SMT's Expr *)
let rec expr2z3Expr (ctx:Z3.context) (e : expression)  = 
	match e with 	
	| IVar v -> (
				match v with 
				| Ident (str, _sort) -> (
											match _sort with 
											| Int size -> Expr.mk_const_s ctx str (BitVector.mk_sort ctx size)
											| Bool -> Boolean.mk_const_s ctx str
											| Array (i_size, d_size) -> Z3Array.mk_const_s ctx str (BitVector.mk_sort ctx i_size) (BitVector.mk_sort ctx d_size)
										)
				| Para (varr, expr) -> (
											match varr, expr with
											(Ident (str, Array(i_size, d_size)), Const (IntC (index, size))) -> (
																		if i_size = size then 
																			Z3Array.mk_select ctx 
																			(Z3Array.mk_const_s ctx str (BitVector.mk_sort ctx i_size) (BitVector.mk_sort ctx d_size))
																			(Expr.mk_numeral_int ctx index (BitVector.mk_sort ctx size))
																		else
																			raise UnMatchedIndex
																	)
											|_ -> raise UnMatchedPara
										)
				| _ -> raise UnMatchedIVar
			)
	| Const s -> ( 
				match s with 
				| IntC (value, size) ->  Expr.mk_numeral_int ctx value (BitVector.mk_sort ctx size)
				| BoolC b -> if b then Boolean.mk_true ctx else Boolean.mk_false ctx
			)
	| Uif (str, expr)-> (
							match expr with 
							h1::h2::[] -> BitVector.mk_add ctx (expr2z3Expr ctx h1) (expr2z3Expr ctx h2)
							| _ -> raise UnMatchedUIF
						)	
	| IteForm (f, e1, e2) -> Boolean.mk_ite ctx (form2z3expr ctx f) ( expr2z3Expr ctx e1) (expr2z3Expr ctx e2)
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

(** the SET of Z3 Expr *)
module ExprSet = Set.Make(
	struct
	let compare = Pervasives.compare
	type t = Expr.expr
	end
)

let get_all_models ctx expr args_of_expr =
	let slvr = Solver.mk_solver ctx None in
	let rec get (c : Z3.context) (s : Solver.solver) (args : Expr.expr list) (extra_constraints : Expr.expr list) = 
		Solver.add s extra_constraints;
		ignore (Solver.check s []); 
		match Solver.get_model s with
		Some model -> (
			let new_constraints = List.map (fun e -> 
												match Model.eval model e true with
												| Some ee -> (
														let pre_model = Boolean.mk_and c [(BitVector.mk_ule c e ee); (BitVector.mk_uge c e ee)] in
														Boolean.mk_not c pre_model 
													)	
												| None -> raise InvalidExpression
											) (List.filter (fun a -> Expr.is_const a) args) 
			in
			let one_model = List.map (fun a -> ( 
												match Model.eval model a true with
												| Some aa -> ((Expr.to_string a), (Expr.to_string aa))
												| None -> raise InvalidExpression
											) 
									) args
			in 
			one_model::(get c s args new_constraints )
		)
		| None -> []
	in 
	Solver.add slvr [expr];
	let res = get ctx slvr args_of_expr [] in
	Printf.printf "For %s:\n" (Expr.to_string expr); 
	List.iter (fun aL -> ( List.iter (fun bT -> (let (var, value) = bT in Printf.printf "%s = %s  " var value) ) aL ;
				Printf.printf "\n" )
	) res;