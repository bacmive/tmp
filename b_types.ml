open Tools
open Trajectory

type scalar = 
  | IntC of int * int
  | BoolC of bool
  | SymbIntC of string * int
  | SymbBoolC of string
      
type sort = 
  | Int of int
  | Bool
  | Array of int * sort
             
type var =
  | Ident of string * sort
  | Para of var * expression
and expression =
  | IVar of var
  | Const of scalar
  | IteForm of formula * expression * expression
and formula = 
  | Eqn of expression * expression
  | AndForm of formula * formula
  | Neg of formula
  | OrForm of formula * formula
  | ImplyForm of formula * formula
  | Chaos


(** term level scalar to boolean vector of 0/1 *)
let termScalar2bitVecConst (c : scalar) : scalar list =
  match c with
    | IntC (i_value, i_size) -> ( 
      let helper = function
        | 1 -> BoolC true
        | 0 -> BoolC false
        | _  -> raise (Failure "how")
            in
      let decToBin x =
        let rec d2b y lst = 
        match y with 
        |0 -> lst
        |_ -> d2b (y/2) ((helper (y mod 2))::lst)
        in
        d2b x [] 
      in
      let intToBinVec value size =
        let res = Array.make size (BoolC false) in
        let bin = List.rev (decToBin value) in 
        let indexes = upt 0 (List.length bin-1) in 
        List.iter (fun (v, i) -> Array.set res (size-1-i) v) (List.combine bin indexes);
        Array.to_list res
      in 
      intToBinVec i_value i_size
    )
    | BoolC b -> [(BoolC b)]
	| _ -> raise (Failure "In termScalar2bitVecConst: WRONG USAGE: can't work on symbolic const data")
	
         
(** mapping function*)
let formatMapVIS ?axis1:(a1=(-1)) ?axis2:(a2=(-1)) (name : string) =
  if a1 < 0 then name ^ "0"
  else if a2 < 0 then name ^ "<" ^ (string_of_int a1) ^ ">0"
  else name ^ "<*" ^ (string_of_int a1) ^ "*>" ^ "<" ^ (string_of_int a2) ^ ">0"

(** term variables to boolean variables *)
let rec termVar2bitVecVar (v : var) : var list = 
  match v with
	| Ident (str, typ) -> (
		match typ with
		| Int size -> List.map (fun i -> Ident (formatMapVIS ~axis1:i str, Bool)) (dwt (size-1) 0 )
		| Bool -> [v]
		| Array (index_size, typ1) -> (
			match typ1 with 
			| Int data_size -> (
					let twoDimension =List.map (fun i -> (
										List.map (fun j -> Ident (formatMapVIS ~axis1:i ~axis2:j str, Bool)) (dwt (data_size-1) 0 )
									)
								) (dwt (index_size-1) 0 ) 
					in
					List.flatten twoDimension
				)
			| Bool -> (
				List.map (fun i -> Ident (formatMapVIS ~axis1:i str, Bool) ) (dwt (index_size -1) 0) 
				)
			| _ ->raise (Failure "Not Supported Nested Array Yet")
		)
	)
	| Para (v, expr) ->(
		match v,expr with
		|(Ident (str_v, Array(i_size, Int d_size)), Const (IntC(value, size))) ->(
			if ( i_size != size ) then raise (Failure " size not match ")
			else List.map (fun j -> Ident (formatMapVIS ~axis1:value ~axis2:j str_v, Bool))  (dwt (d_size-1) 0)
		)
		| (Ident (str_v, Array(i_size, Bool)), Const (IntC(value, size))) -> (
			if( i_size != size ) then raise (Failure " size not match ")
			else [Ident (formatMapVIS ~axis1:value str_v, Bool)]
		)
		| _ -> raise (Failure "Not Supported Expression")
	)
 
 
(** 
  termVar2bitVecVar's return type : var list
  termScalar2bitVecConst's return type : scalar list
  termExp2bitExp's return type : expression list
  termForm2bitForm's return type : formula
*)
let rec termExp2bitExp (e : expression) =
	match e with 
		| IVar v -> List.map (fun elem -> IVar elem ) (termVar2bitVecVar v)
		| Const sclr ->  List.map (fun elem -> Const elem) (termScalar2bitVecConst sclr)
		| IteForm (f, e1, e2) -> (
			let b_f = termForm2bitForm f in
			let b_e1 = termExp2bitExp e1 in
			let b_e2 = termExp2bitExp e2 in
			List.map2 (fun t1 t2 -> IteForm(b_f, t1, t2)) b_e1 b_e2
		)	
and termForm2bitForm (f : formula) =
	match f with 
	| Eqn (e1, e2) -> (
		let e1_expr_list = termExp2bitExp e1 in
		let e2_expr_list = termExp2bitExp e2 in
		let fs = List.map2 (fun a b -> Eqn(a,b)) e1_expr_list e2_expr_list in
		List.fold_right (fun f1 f2 -> AndForm(f1, f2)) fs Chaos
	)
	| AndForm (f1, f2) -> (
		let e1_form = termForm2bitForm f1 in
		let e2_form = termForm2bitForm f2 in
		AndForm (e1_form, e2_form)
	)
	| Neg f -> (
		let form = termForm2bitForm f in
		Neg form
	)
	| OrForm (f1, f2) -> (
		let e1_form = termForm2bitForm f1 in
		let e2_form = termForm2bitForm f2 in
		OrForm (e1_form, e2_form)
	)
	| ImplyForm (f1, f2) -> (
		let e1_form = termForm2bitForm f1 in
		let e2_form = termForm2bitForm f2 in
		ImplyForm (e1_form, e2_form)
	)
	| Chaos -> Chaos


(** tools for printing the variables, expression, formula *)
let rec bvar2str v = 
  match v with 
  | Ident (str , srt) -> (
      match srt with
      | Int i -> Printf.sprintf "%s(Int %d) " str i
      | Bool -> Printf.sprintf "%s(Bool) " str
      | Array ( i, Int c) -> Printf.sprintf "%s(Array(%d, Int(%d) "  str i c
      | Array (i ,Bool) -> Printf.sprintf "%s(Array(%d, Bool) " str i 
      | _ -> raise (Failure "not supported type"
                   )
    )
  | Para (v1 , expression) -> bvar2str v1
 
let rec bitExpr2str e =
  match e with
  | IVar v -> bvar2str v
  | Const sclr -> (
    match sclr with
    | IntC (value, size) -> Printf.sprintf "Const(%d, Int(%d)) " value size
    | BoolC b -> Printf.sprintf "Const(Bool(%B)) " b
	| SymbIntC (name, size) ->Printf.sprintf "SymbIntC:(%s,%d)" name size
	| SymbBoolC name -> Printf.sprintf "SymbBoolC:%s" name
  )
  | IteForm (f, e1, e2) -> "IteForm (" ^ ( bitForm2str  f) ^ ", "^ (bitExpr2str e1)^ ", "^(bitExpr2str e2) ^ " )"
and bitForm2str f = 
  match f with
  | Eqn (e1, e2) -> "Eqn ("^(bitExpr2str e1) ^", "^ (bitExpr2str e2)^" )"
  | AndForm (f1, f2) -> "AndForm ("^(bitForm2str f1)^", "^(bitForm2str f2)^")"
  | Neg f ->  "Neg ("^ (bitForm2str f)^ " )"
  | OrForm (f1, f2) -> "OrForm ("^(bitForm2str f1) ^ ", " ^ (bitForm2str f2) ^ " )"
  | ImplyForm (f1, f2) -> "ImplyForm (" ^ (bitForm2str f1) ^", " ^ (bitForm2str f2) ^ " )"
  | Chaos -> "Chaos "



let symbScalar2TrajVar cnst =
	match cnst with
	| SymbIntC (name, size) -> (
		List.map (fun i -> Bvariable (formatMapVIS ~axis1:i name)) (dwt (size-1) 0)
	)
	| SymbBoolC name -> (
		[Bvariable (formatMapVIS name)]
	)
	|_ -> raise (Failure "In boolScalar2TrajVar: unsupported type")

(*
	transform bitForm in ocaml to types accepted by forte 
	tag function
	
*)
let bitForm2trajForm form = 
	let rec toIsList f = 
		match f with
		| Chaos -> []
		| Eqn (IVar (Ident (name, Bool)), Const (BoolC b)) -> [(if b then Is1 (Tnode name) else Is0 (Tnode name))]
		| AndForm (Eqn (IVar (Ident (name, Bool)), Const (BoolC b)), nestedForm ) -> (if b then Is1 (Tnode name) else Is0 (Tnode name)) :: (toIsList nestedForm)
		| _ -> raise (Failure "In bitForm2trajForm: error ") 
	in
	let res = toIsList form in
	match res with
	|[] -> TChaos
	|t::[] -> t
	|ts -> TAndList ts
	


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