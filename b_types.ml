(** Constant Type*)
type scalar = 
	| IntC of int * int
	| BoolC of bool

(* variables' type*)	
type sort = 
	| Int of int
	| Bool
	| Array of int * sort (** index_size, data_type *)

type var =
	| Ident of string * sort
	| Para of var * expression
	| Field of var * string
and expression =
	| IVar of var
	| Const of scalar
	| IteForm of formula * expression * expression
	| Uif of string * expression list
	| Top 
	| Unknown
and formula = 
	| Eqn of expression * expression
	| AndForm of formula * formula
	| Neg of formula
	| OrForm of formula * formula
	| ImplyForm of formula * formula
	| Uip of string * expression list
	| Chaos

let rec upt (f : int) (t : int) : int list =
	if f > t then []
	else f :: upt (f+1) t 

(** term variables to boolean variables *)
let rec termVar2bitVecVar (v : var) = 
	match v with
	| Ident (str, typ) -> (
		match typ with
		| Int size -> List.map (fun i -> Ident (str ^ (string_of_int i), Bool)) (upt 0 (size-1))
		| Bool -> [v]
		| Array (index_size, typ1) -> (
			match typ1 with 
			| Int data_size -> (
				let first_dimension = List.map (fun i -> str ^ "<*" ^ (string_of_int i) ^ "*>") (upt 0 (index_size-1)) in
				let append_second_dimension (str : string) (second_dimension_size : int) = List.map (fun i ->  Ident (str ^ "<" ^ (string_of_int i) ^ ">", Bool)) (upt 0 (second_dimension_size-1)) in
				let twoDimension = List.map (fun s -> append_second_dimension s data_size) first_dimension in
				List.flatten twoDimension
			)
			| Bool -> (
				List.map (fun i -> Ident (str ^ (string_of_int i), Bool) ) (upt 0 (index_size -1))
			)
			| _ -> raise (Failure "Not Supported Nested Array Yet")
		)
	)
	| Para (v, expr) ->(
		match v,expr with
		|(Ident (str_v, Array(i_size, Int d_size)), Const (IntC(value, size))) ->(
			if ( i_size != size ) then raise (Failure " size not match ")
			else List.map (fun i -> Ident ((str_v ^ "<*" ^ (string_of_int value) ^ "*>" ^ "<" ^(string_of_int i) ^ ">"), Bool))  (upt 0 (d_size-1))
		)
		| (Ident (str_v, Array(i_size, Bool)), Const (IntC(value, size))) -> (
			if( i_size != size ) then raise (Failure " size not match ")
			else [Ident ((str_v ^ "<*" ^ (string_of_int value) ^ "*>", Bool))]
		)
		| _ -> raise (Failure "Not Supported Expression")
	)
	|_ -> raise (Failure  "Not Supported Variable Type")