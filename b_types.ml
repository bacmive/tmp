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

(** mapping function*)
let formatMapVIS ?axis1:(a1=(-1)) ?axis2:(a2=(-1)) (name : string) =
	if a1 < 0 then name ^ "0"
	else if a2 < 0 then name ^ "<" ^ (string_of_int a1) ^ ">"
	else name ^ "<*" ^ (string_of_int a1) ^ "*>" ^ "<" ^ (string_of_int a2) ^ ">"

(** term variables to boolean variables *)
let rec termVar2bitVecVar (v : var) = 
  match v with
  | Ident (str, typ) -> (
      match typ with
      | Int size -> List.map (fun i -> Ident (formatMapVIS ~axis1:i str, Bool)) (upt 0 (size-1))
      | Bool -> [v]
      | Array (index_size, typ1) -> (
          match typ1 with 
          | Int data_size -> (
				let twoDimension =List.map (fun i -> (
														List.map (fun j -> Ident (formatMapVIS ~axis1:i ~axis2:j str, Bool)) (upt 0 (data_size-1))
													)
										) (upt 0 (index_size-1)) 
				in
				List.flatten twoDimension
            )
          | Bool -> (
				List.map (fun i -> Ident (formatMapVIS ~axis1:i str, Bool) ) (upt 0 (index_size -1))
            )
          | _ ->raise (Failure "Not Supported Nested Array Yet")
        )
    )
  | Para (v, expr) ->(
      match v,expr with
      |(Ident (str_v, Array(i_size, Int d_size)), Const (IntC(value, size))) ->(
          if ( i_size != size ) then raise (Failure " size not match ")
          else List.map (fun j -> Ident (formatMapVIS ~axis1:value ~axis2:j str_v, Bool))  (upt 0 (d_size-1))
        )
      | (Ident (str_v, Array(i_size, Bool)), Const (IntC(value, size))) -> (
          if( i_size != size ) then raise (Failure " size not match ")
          else [Ident (formatMapVIS ~axis1:value str_v, Bool)]
        )
      | _ -> raise (Failure "Not Supported Expression")
    )
  |_ -> raise (Failure  "Not Supported Variable Type")

let rec print_var v = 
  match v with 
  | Ident (str , srt) -> (
      match srt with
      | Int i -> Printf.printf "%s --Type: Int--size: %d\n" str i
      | Bool -> Printf.printf "%s -- Type: Bool\n" str
      | Array ( i, Int c) -> Printf.printf "%s --Type: Array--index_size: %d--data_type: Int(size: %d)\n"  str i c
      | Array (i ,Bool) -> Printf.printf "%s --Type: Array -- index_size: %d --data_type : Bool" str i 
      | _ -> raise (Failure "not supported type"
                   )
    )
  | Para (v1 , expression) -> print_var v1
  | _ -> () 
 


(**
let rec termVar2bitVecVar mapping (v : var) = 
 match v with
 | Ident (str, typ) -> (
  match typ with
  | Int size -> List.map (fun i -> Ident (mapping str i, Bool)) (upt 0 (size-1))
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

let rec termExp2bitexp mapping e=
  case e of 
    |(Var v) -> 
      case v of 
        |simple v --> ...
        |error
    |IteForm(f,e1,e2) ->
    let f'=termForm2bitForm mapping f in
    let e1'= termExp2bitexp mapping e1 in 
    let e2'= termExp2bitexp mapping e2 in 
    List.map2 ~f:(fun a b -> iteForm f' a b) e1' e2'
    |uif --> error

and let rec termForm2bitForm mapping f=
  case f of 
  eqn(e1,e2)->
    let es1=termExp2bitexp mapping e1 in
    let es2=termExp2bitexp mapping e2 in
    let fs=List.map2 ~f:(fun a b -> eqn(a,b)) es1 es2 in
    List.fold ~f(fun a b->and(a,b)) fs true
*)