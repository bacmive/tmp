open Tools
open Types
open Trajectory

let visCLKname = "CLK"
let yosysCLKname = "CLK"
 
(** mapping function*)
let formatMapVIS ?axis1:(a1=(-1)) ?axis2:(a2=(-1)) (name : string) =
  if a1 < 0 then name ^ "0"
  else if a2 < 0 then name ^ "<" ^ (string_of_int a1) ^ ">0"
  else name ^ "<*" ^ (string_of_int a1) ^ "*>" ^ "<" ^ (string_of_int a2) ^ ">0"

let formatMapYosys ?axis1:(a1=(-1)) ?axis2:(a2=(-1)) (name : string) =
	if (a1<0) then name
	else if a2 < 0 then name ^ "<" ^ (string_of_int a1) ^ ">"
	else name ^ "<" ^ (string_of_int a1) ^ ">" ^ "<" ^ (string_of_int a2) ^ ">"
	

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
	| SymbIntC (name, i_size) -> (
		List.map (fun i -> SymbBoolC (formatMapVIS ~axis1:i name)) (dwt (i_size-1) 0)
	)
	| SymbBoolC name -> [(SymbBoolC (formatMapVIS name))]
	(*| _ -> raise (Failure "In termScalar2bitVecConst: WRONG USAGE: can't work on symbolic const data")*)
	

(** term variables to boolean variables *)
let rec termVar2bitVecVar (v : var) : var list = 
  match v with
	| Ident (str, typ) -> (
		match typ with
		| Int size -> List.map (fun i -> Ident (formatMapVIS ~axis1:i str, Bool)) (dwt (size-1) 0 )
		| Bool -> [Ident (formatMapVIS str, Bool)]
		| Array (length, typ1) -> (
			match typ1 with 
			| Int data_size -> (
					let twoDimension =List.map (fun i -> (
										List.map (fun j -> Ident (formatMapVIS ~axis1:i ~axis2:j str, Bool)) (dwt (data_size-1) 0 )
									)
								) (dwt (length-1) 0) 
					in
					List.flatten twoDimension
				)
			| Bool -> (
				List.map (fun i -> Ident (formatMapVIS ~axis1:i str, Bool) ) (dwt (length -1) 0) 
			)
			| _ ->raise (Failure "In termVar2bitVecVar: Not Supported Nested Array Yet")
		)
	)
	| Para (v, expr) ->(
		match v,expr with
		|(Ident (str_v, Array(length, Int d_size)), Const (IntC(value, size))) ->(
			List.map (fun j -> Ident (formatMapVIS ~axis1:value ~axis2:j str_v, Bool))  (dwt (d_size-1) 0)
		)
		| (Ident (str_v, Array(length, Bool)), Const (IntC(value, size))) -> (
			[Ident (formatMapVIS ~axis1:value str_v, Bool)]
		)
		| _ -> raise (Failure "In termVar2bitVecVar: Not Supported Expression")
	)
 
 
(** 
  termScalar2bitVecConst's return type : scalar list
  termVar2bitVecVar's return type : var list
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
		| _-> raise (Invalid_argument "In termExp2bitExp: you may use uninterpreted function")
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
  | Uif (name, exprs) -> Printf.sprintf "Uif%s (%s)" name (String.concat "," (List.map (fun e -> bitExpr2str e) exprs)) 
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
		| Eqn (IVar (Ident (name1, Bool)), IVar (Ident (name2, Bool))) -> []
		| Eqn (IVar (Ident (name1, Int size1)), IVar (Ident (name2, Int size2))) -> []
		| Eqn (IVar (Ident (name, Bool)), Const (BoolC b)) -> [(if b then Is1 (Tnode name) else Is0 (Tnode name))]
		| Eqn (IVar (Ident (name, Bool)), Const (SymbBoolC v_str)) -> [isb (EVar (Bvariable v_str)) (Tnode name)]
		(*| AndForm (Eqn (IVar (Ident (name, Bool)), Const (BoolC b)), nestedForm ) -> (if b then Is1 (Tnode name) else Is0 (Tnode name)) :: (toIsList nestedForm)*)
		| AndForm (nestedForm1, nestedForm2) -> (toIsList nestedForm1)@(toIsList nestedForm2)
		| _ -> raise (Failure "In bitForm2trajForm: error ") 
	in
	let res = toIsList form in
	match res with
	|[] -> TChaos
	|t::[] -> t
	|ts -> TAndList ts

 
(*********************************** tag to forte input language *******************************************)
(** transform trajectory formula in Ocaml to trajectory formual in (GSTE) Forte list *)
let rec trajOcaml2trajFL trajf = 
		match trajf with 
		| TChaos -> []
		| Is1 (Tnode name) -> [Printf.sprintf "Is1 \"%s\"" name]
		| Is0 (Tnode name) -> [Printf.sprintf "Is0 \"%s\"" name]
		| Guard (be, tf) -> (
			match (trajOcaml2trajFL tf) with 
			|[] -> [Printf.sprintf "Guard (%s) (%s)" (bExpr2FLbExprList be) "Chaos"]
			| f::[] -> [Printf.sprintf "Guard (%s) (%s)" (bExpr2FLbExprList be) f]
			| fs -> [Printf.sprintf "Guard (%s) (%s)" (bExpr2FLbExprList be) "TAndList [" ^ (String.concat "," fs) ^ "]"]
		)			
		| TAndList ts ->  List.flatten (List.map (fun f -> trajOcaml2trajFL f) ts)
		| _ -> raise (Invalid_argument "this is for boolean level trajectory formula")


(*********************************** to forte input file *******************************************)

(** gsteSpec to forte input AG *)
let toFL gs model_name binNodes=   
	let out_channel = open_out (Printf.sprintf "%s_gste.fl" model_name) in
	let main_assertion_graph init_node node_set edge_set =
		match init_node with (Vertex inum) -> Printf.fprintf out_channel "let vertexI = Vertex %d;\n" inum ;
		Printf.fprintf out_channel "%s" ("let vertexL = [" ^ (String.concat "," (List.map (fun (Vertex i) -> Printf.sprintf "Vertex %d" i) node_set)) ^ "];\n" );
		Printf.fprintf out_channel "%s" ("let edgeL = [" ^ (String.concat "," (List.map (fun (Edge ((Vertex f),(Vertex t))) -> Printf.sprintf "Edge (Vertex %d) (Vertex %d)" f t) edge_set)) ^ "];\n");
	in
	let ant_function init_node node_set edge_set ants =
		let ants_traj e = 
			let term_f = ants e in
			let bit_f = termForm2bitForm term_f in
			let traj_f = bitForm2trajForm bit_f in
			let add_myclk fs =
				match fs with 
				| [] -> "TAndList []"
				| s -> "TAndList ["^ (String.concat "," (s@[Printf.sprintf "Is0 \"%s\"" visCLKname ; Printf.sprintf "Next (Is1 \"%s\")" visCLKname])) ^"]"
			in
			add_myclk (trajOcaml2trajFL traj_f)
		in
		Printf.fprintf out_channel "%s" (
"let ant aEdge = 
	val (Edge (Vertex from) (Vertex to)) = aEdge
	in 
"^		(
			let items = List.map (fun e -> ( 
											match e with 
											|Edge ((Vertex f),(Vertex t)) -> Printf.sprintf "((from = %d) AND (to = %d)) => %s " f t (ants_traj e) 
										)
							) edge_set 
			in 
			let cases = String.concat "\n\t| " items in
			let body = cases ^ "\n\t| error \"In cons: missing case\"" in
			Printf.sprintf "\t%s\n;\n\n" body
		)
	)
	in 
	let cons_function init_node node_set edge_set cons =
		let cons_traj e = 
			let term_f = cons e in
			let bit_f = termForm2bitForm term_f in
			let traj_f = bitForm2trajForm bit_f in 
			let add_tandlist ts =
				match ts with
				| [] -> "TAndList []"
				| s -> "TAndList ["^ (String.concat "," s) ^"]"
			in
			add_tandlist (trajOcaml2trajFL traj_f)
		in 
		Printf.fprintf out_channel "%s" (
"let cons aEdge = 
	val (Edge (Vertex from) (Vertex to)) = aEdge
	in 
"^		(	
			let items = List.map (fun e -> ( 
											match e with 
											|Edge ((Vertex f),(Vertex t)) -> Printf.sprintf "((from = %d) AND (to = %d)) => %s " f t (cons_traj e) 
										)
							) edge_set 
			in 
			let cases = String.concat "\n\t| " items in
			let body = cases ^ "\n\t| error \"In cons: missing case\"" in
			Printf.sprintf "\t%s\n;\n\n" body
		) 
	)
	in 
	let binNodesPart binNodes = 
		let ivar2str iv =
			match iv with
			| IVar (Ident (str, Int n)) -> List.map (fun i -> formatMapVIS ~axis1:i str) (upt 0 (n-1))
			| IVar (Ident (str, Bool)) -> [formatMapVIS str]
			| _ -> raise (Invalid_argument "In toFL binNodesPart: sry la")
		in
		match binNodes with
		| []  -> "[]"
		| bns -> (
			let strOfbns = (List.flatten (List.map (fun nd -> ivar2str nd) bns)) in
			let addQuoteMark = List.map (fun sob -> "\""^sob^"\"") strOfbns in
			"[" ^ (String.concat "," addQuoteMark) ^"]"
		)
	in
	match gs with Graph (init_node , node_set, edge_set, ants, cons) -> (
		Printf.fprintf out_channel 
"let ckt = load_exe \"%s.exe\";
load \"gsteSymReduce.fl\";
loadModel ckt;
" model_name;
		main_assertion_graph init_node node_set edge_set;
		ant_function init_node node_set edge_set ants;
		cons_function init_node node_set edge_set cons ;
		Printf.fprintf out_channel
"
let mainGoal = Goal [] (TGraph (Graph vertexL vertexI edgeL (Edge2Form ant) (Edge2Form cons)));
let binNodes = %s;
lemma \"lemmaTMain\" mainGoal;
	by (gsteSymbSim binNodes);
done 0;
quit;
"  (binNodesPart binNodes)
	);
	close_out out_channel
