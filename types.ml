open Tools
(**term-level constant, variable, expression and formula definition*)
type scalar = 
  | IntC of int * int (* value * size *)
  | BoolC of bool
  | SymbIntC of string * int
  | SymbBoolC of string
      
type sort = 
  | Int of int (* size *)
  | Bool
  | Array of int * sort (*array length * data sort*)
             
type var =
  | Ident of string * sort
  | Para of var * expression
and expression =
  | IVar of var
  | Const of scalar
  | Uif of string * expression list
  | IteForm of formula * expression * expression
and formula = 
  | Eqn of expression * expression
  | AndForm of formula * formula
  | Neg of formula
  | OrForm of formula * formula
  | ImplyForm of formula * formula
  | Chaos

(** a piece of shortcuts*)
let number data_value data_size= 
	Const (IntC (data_value, data_size))

let plus expr1 expr2 =
	Uif ("+", [expr1; expr2])

(* case expression *)
type formulaExpPair = formula * expression

let rec caseExpression : formulaExpPair list -> expression = function
  | (f, e)::[] -> IteForm (f, e, e)
  | (f, e)::t -> IteForm (f, e, (caseExpression t))
  | _ -> raise (Failure "case expression format error")
  
let readArray (v : var) (e : expression) : expression =
	match v,e with 
	| (Ident (name, Array (length, Bool)), Const (IntC(value, index_size))) -> 
		caseExpression (List.map (fun i -> (Eqn (e, (Const (IntC (i,index_size)))), IVar (Para (v, Const (IntC (i, index_size)))))) (upt 0 (length-1)))
	| (Ident (name, Array (length, Bool)), IVar (Ident (var_name, Int index_size))) ->
		caseExpression (List.map (fun i -> (Eqn (e, (Const (IntC (i, index_size)))), IVar (Para (v, Const (IntC (i, index_size)))))) (upt 0 (length-1)))
	| (Ident (name, Array (length, Int data_size)), IVar (Ident (var_name, Int index_size))) ->
		caseExpression (List.map (fun i -> (Eqn (e, (Const (IntC (i, index_size)))), IVar (Para (v, Const (IntC (i, index_size)))))) (upt 0 (length-1)))
	| (Ident (name, Array (length, Int data_size)), Uif (str, exprs)) ->(
		match exprs with
		| e1::e2::[] -> (
			match e1,e2 with
			|(IVar (Ident (e1_name, Int index_size)), Const (IntC (e2_value, e2_size))) -> (
				caseExpression (List.map (fun i -> (Eqn (e, (Const (IntC (i, index_size)))), IVar (Para (v, Const (IntC (i, index_size)))))) (upt 0 (length-1)))
			)
			|_ -> raise (Invalid_argument "In readArray: not supported uninterpreted function parameter")
		)
		|_->raise (Invalid_argument "In readArray: not matched argument")
	)	
	| _ -> raise (Invalid_argument "In readArray: not matched argument")


(*********************************** GSTE assertion graph *******************************************)
type node = Vertex of int
type edge = Edge of node * node

type edgeToFormula = edge -> formula
(* gste Graph *)
type gsteSpec = Graph of node * node list * edge list * edgeToFormula * edgeToFormula

(** the source node of an edge and the sink node of an edge*)
let source : edge -> node = function
    | Edge (n, _) -> n

let sink : edge -> node = function
    | Edge (_, n) -> n

(** retrive the int of a node *)
let nodeToInt : node -> int = function
  | Vertex n -> n

(** tag invariant type *)
type tag_invs = TAGINV of expression list * formula list
(**tag function type*)
type vertexToTagInv = node -> tag_invs


(**************************************container of formula and expression **************************)
(** the SET of expression *)
module TExpressionSet = Set.Make(
	struct
	let compare = Pervasives.compare
	type t = expression
	end
)
(** the SET of formula *)
module TFormulaSet = Set.Make(
	struct
	let compare = Pervasives.compare
	type t = formula
	end
)

(**************************************other tools *****************************************)
let rec contain_symbolic_const form = 
		match form with 
		| Eqn (e1, e2) ->(
			match e1,e2 with
			| (Const (SymbBoolC str), _) -> true
			| (Const (SymbIntC (str, size)), _) -> true
			| (_, Const (SymbBoolC str)) -> true
			| (_, Const (SymbIntC (str, size))) -> true
			| _-> false
		)
		| AndForm(f1, f2) -> (contain_symbolic_const f1) && (contain_symbolic_const f2)
		| Neg f -> contain_symbolic_const f
		| OrForm(f1, f2) -> (contain_symbolic_const f1) && (contain_symbolic_const f2)
		| ImplyForm (f1, f2) -> (contain_symbolic_const f1) && (contain_symbolic_const f2)
		| Chaos -> false
let not_contain_symbolic_const form =
	not (contain_symbolic_const form)


let rec expr_in_expr expr iexpr =
	match expr with 
	| Uif (str, exprs) -> (List.fold_right (||) (List.map (fun e -> (expr_in_expr e iexpr)) exprs) false) 
	| IteForm (f, e1, e2) -> (expr_in_form f iexpr)||(expr_in_expr e1 iexpr)||(expr_in_expr e2 iexpr)
	| IVar v -> expr=iexpr
	| Const s -> expr=iexpr
and expr_in_form form iexpr =
	match form with
	| Eqn (e1, e2) -> (expr_in_expr e1 iexpr)||(expr_in_expr e2 iexpr)
	| AndForm (f1, f2) -> (expr_in_form f1 iexpr) || (expr_in_form f2 iexpr)
	| Neg f -> (expr_in_form f iexpr)
	| OrForm (f1, f2) -> (expr_in_form f1 iexpr) || (expr_in_form f2 iexpr)
	| ImplyForm (f1, f2) -> (expr_in_form f1 iexpr)||(expr_in_form f2 iexpr)
	| Chaos -> false

let expr_not_in_form form iexpr =
	not (expr_in_form form iexpr)

let expr_in_args expr args = 
	List.fold_right (||) (List.map (fun e -> (expr = e) ) args) false

let expr_not_in_args expr args =
	not (expr_in_args expr args)

let form_contain_args form args = 
	List.fold_right (||) (List.map (fun e -> (expr_in_form form e)) args) false

let form_not_contain_args form args =
	not (form_contain_args form args)

let rec form_solve_directly form =
	match form with
	| Eqn (e1, e2) ->(
		match e1, e2 with
		| (IVar (Ident (str, Int size)), Const (IntC (data, data_size))) -> true
		| (Const (IntC (data, data_size)), IVar (Ident (str, Int size))) -> true
		| (IVar (Ident (str, Bool)), Const (BoolC b)) -> true
		| (Const (BoolC b), IVar (Ident (str, Bool))) -> true
		| _ -> false
	)
	| AndForm(f1, f2) -> (form_solve_directly f1) && (form_solve_directly f2)
	| Neg f -> form_solve_directly f
	| OrForm(f1, f2) -> (form_solve_directly f1) && (form_solve_directly f2)
	| ImplyForm (f1, f2) -> (form_solve_directly f1) && (form_solve_directly f2)
	| Chaos -> true

let form_not_solve_directly form = 
	not (form_solve_directly form)