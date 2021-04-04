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

(** mapping function*)
let formatMapVIS ?axis1:(a1=(-1)) ?axis2:(a2=(-1)) (name : string) =
  if a1 < 0 then name ^ "0"
  else if a2 < 0 then name ^ "<" ^ (string_of_int a1) ^ ">"
  else name ^ "<*" ^ (string_of_int a1) ^ "*>" ^ "<" ^ (string_of_int a2) ^ ">"

(** term variables to boolean variables *)
let rec termVar2bitVecVar (v : var) : var list = 
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
  | _ -> raise (Failure "not supported yet")
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
  | _ -> raise (Failure "not supported yet")

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
