open Tools
open Types
open Trajectory
open Z3Utils

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
		let rec dec2Bin k n=
			if(n=0) then []
			else (helper (k mod 2)) :: (dec2Bin (k/2) (n-1))
		in
		List.rev (dec2Bin i_value i_size)
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
		| Eqn (IVar (Ident (name1, Bool)), IVar (Ident (name2, Bool))) -> [] (**)
		| Eqn (IVar (Ident (name1, Int size1)), IVar (Ident (name2, Int size2))) -> [] (**)
		| Eqn (IVar (Ident (name, Bool)), Const (BoolC b)) -> [(if b then Is1 (Tnode name) else Is0 (Tnode name))]
		| Eqn (IVar (Ident (name, Bool)), Const (SymbBoolC v_str)) -> [isb (EVar (Bvariable v_str)) (Tnode name)]
		| AndForm (nestedForm1, nestedForm2) -> (toIsList nestedForm1)@(toIsList nestedForm2)
		| _ -> raise (Failure "In bitForm2trajForm: error, unsupported bitForm format") 
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

(*
let to_tag_in_forte tag node_set =
	let tagInv_to_forteInv tag node =
		let TAGINV (vars, forms) = tag node in
		let ctx = make_z3_context () in
		let z3ExprOfForm = and_all_exprs ctx (List.map (fun f -> form2z3expr ctx f) forms) in
		get_all_models ctx z3ExprOfForm (List.map (fun f -> form2z3expr ctx f) vars)
	in
	List.iter (fun n -> taginv_to_forteInv tag n) node_set
*)
let tag_inv_to_bit_form form args concret_list =
		let hash = Hashtbl.create 4 in
		let () = List.iter2 (fun arg v -> Hashtbl.add hash arg v ) args concret_list in
		match form with
		| Eqn (e1, e2) -> (
			match e1,e2 with
			| (IteForm (f, e11, e12), Const (SymbIntC (c_name, data_size))) -> (
				match f, e11 with
				| (Eqn (fe1, fe2), IVar (Para (Ident (arrname, Array (len, sort)), Const (IntC (i, index_size))))) -> (
					match fe1, fe2 with
					| (Const (IntC (value, index_size1)), Const (IntC (i2, index_size2))) -> (
						termForm2bitForm (Eqn (IVar (Para (Ident (arrname, Array (len, sort)), Const (IntC (value, index_size1)))),  Const (SymbIntC (c_name, data_size))))
					)
					| (IVar (Ident (var_name, Int index_size1)), Const (IntC (i2, index_size2))) when (expr_in_args fe1 args)-> (
						termForm2bitForm (Eqn (IVar (Para (Ident (arrname, Array (len, sort)), Const (IntC ((Hashtbl.find hash fe1), index_size1)))), Const (SymbIntC (c_name, data_size))))
					)
					| (Uif (op_str, opex1::opex2::[]), Const (IntC (i2, index_size2))) when ((expr_in_args opex1 args)|| (expr_in_args opex2 args)) ->(
						match opex1,opex2 with
						| (IVar (Ident (opex1_name, Int opex1_size)), IVar (Ident (opex2_name, Int opex2_size))) when ((expr_in_args opex1 args) && (expr_in_args opex2 args))->(
							termForm2bitForm (Eqn (IVar (Para (Ident (arrname, Array (len, sort)), Const (IntC (((Hashtbl.find hash opex1)+(Hashtbl.find hash opex2)) mod len, opex1_size)))), Const (SymbIntC (c_name, data_size))))
						)
						| (IVar (Ident (opex1_name, Int opex1_size)), Const (IntC (opex2_value, opex2_size))) -> (
							termForm2bitForm (Eqn (IVar (Para (Ident (arrname, Array (len, sort)), Const (IntC (((Hashtbl.find hash opex1)+ opex2_value) mod len, opex1_size)))), Const (SymbIntC (c_name, data_size))))
						)
						| (Const (IntC (opex1_value, opex1_size)),IVar (Ident (opex2_name, Int opex2_size))) -> (
							termForm2bitForm (Eqn (IVar (Para (Ident (arrname, Array (len, sort)), Const (IntC (((Hashtbl.find hash opex2)+ opex1_value) mod len, opex1_size)))), Const (SymbIntC (c_name, data_size))))
						)
						| _-> raise (Invalid_argument "In tag_inv_to_bit_form-> form -> e1e2 ->fe11->fe1fe2->opex1opex2")
					)
					| _-> raise (Invalid_argument "In tag_inv_to_bit_form-> form -> e1e2 ->fe11->fe1fe2")
				)
				| _-> raise (Invalid_argument "In tag_inv_to_bit_form-> form -> e1e2 ->fe11")
			)	
			| (IVar (Ident (str, Bool)), Const (BoolC b)) when ((expr_not_in_args e1 args)&&(expr_not_in_args e2 args)) ->(
				termForm2bitForm (Eqn (e1, e2))
			)
			| (Const (BoolC b), IVar (Ident (str, Bool))) when ((expr_not_in_args e1 args)&&(expr_not_in_args e2 args)) -> (
				termForm2bitForm (Eqn (e2, e1))
			)
			| (IVar (Ident (str1, Int size1)), IVar (Ident (str2, Int size2))) when ((expr_in_args e1 args)&&(expr_in_args e2 args)) -> (
				termForm2bitForm (AndForm (Eqn (IVar (Ident (str1, Int size1)), Const (IntC (Hashtbl.find hash e1, size1))), 
										Eqn (IVar (Ident (str2, Int size2)), Const (IntC (Hashtbl.find hash e2, size2))) 
									))
			)
			| (IVar (Ident (str1, Int size1)), Uif (str, e21::e22::[])) when ((expr_in_args e1 args)&&((expr_in_args e21 args || expr_in_args e22 args))) -> (
				match e21, e22 with 
				| (IVar (Ident (e21_name, Int e21_size)), Const (IntC (e22_value, e22_size))) -> (
					termForm2bitForm (AndForm (Eqn (IVar (Ident (str1, Int size1)), Const (IntC (Hashtbl.find hash e1, size1))), 
										Eqn (IVar (Ident (e21_name, Int e21_size)), Const (IntC (Hashtbl.find hash e21, e21_size))) 
									))
				)
				| (Const (IntC (e21_value, e21_size)), IVar (Ident (e22_name, Int e22_size))) -> (
					termForm2bitForm (AndForm (Eqn (IVar (Ident (str1, Int size1)), Const (IntC (Hashtbl.find hash e1, size1))), 
										Eqn (IVar (Ident (e22_name, Int e22_size)), Const (IntC (Hashtbl.find hash e22, e22_size))) 
									))
				)
				| _ -> raise (Invalid_argument "In tag_inv_to_bit_form-> form -> e1e2->e21e22")
			)
			| _ -> raise (Invalid_argument "In tag_inv_to_bit_form-> form -> e1e2")
		)
		| _-> raise (Invalid_argument "In tag_inv_to_bit_form-> form") 

let handle_mem_special form =
	match form with
	| Eqn (e1, e2) -> (
		match e1, e2 with
		| (IteForm (f, e11, e12), Const (SymbIntC (c_name, data_size))) -> (
			match f,e11 with
			| (Eqn (fe1, fe2), IVar (Para (Ident (arrname, Array (len, sort)), Const (IntC (i, index_size))))) -> (
				match fe1, fe2 with
				| (Const (SymbIntC (str, index_size)), Const (IntC (i, index_size1))) -> (
					let values = upt 0 ((pow 2 index_size)-1) in
					let actions = List.map (fun v -> 
									trajOcaml2trajFL (
										bitForm2trajForm (
											termForm2bitForm (Eqn (IVar (Para (Ident (arrname, Array (len, sort)), Const (IntC (v, index_size1)))),  Const (SymbIntC (c_name, data_size))))
										)
									)
								) values
					in
					let actionsFL = List.map (
									( fun subFLlist -> Printf.sprintf "TAndList [%s]" (String.concat "," subFLlist))
								) actions 
					in
					let index = List.map (fun i -> EVar (Bvariable (formatMapVIS ~axis1:i str))) (dwt (index_size-1) 0) in
					let indexIs k size= 
						let rec dec2Bin k n=
							if(n=0) then []
							else (k mod 2) :: (dec2Bin (k/2) (n-1))
						in 
						let bins = List.rev (dec2Bin k size) in
						List.map2 (fun i b -> (if (b=1) then i else (ENeg (i)))) index bins
					in
					let cases = List.map (fun k -> indexIs k index_size) values in
					let guards = List.map (fun case -> 
											"(" ^ (String.concat " bAND " ( List.map (fun be -> bExpr2FLbExprList be) case )) ^ ")"
										) cases
					in
					let terms = List.map2 (fun g s -> "Guard (" ^ g ^ ") (" ^ s ^ ")") guards actionsFL  in
					Printf.sprintf "TAndList [\n %s \n]"  (String.concat ",\n" terms)			
				)
				| (Const (IntC (i, index_size1)), Const (SymbIntC (str, index_size))) -> (
					let values = upt 0 ((pow 2 index_size)-1) in
					let actions = List.map (fun v -> 
									trajOcaml2trajFL (
										bitForm2trajForm (
											termForm2bitForm (Eqn (IVar (Para (Ident (arrname, Array (len, sort)), Const (IntC (v, index_size1)))),  Const (SymbIntC (c_name, data_size))))
										)
									)
								) values
					in
					let actionsFL = List.map (
									( fun subFLlist -> Printf.sprintf "TAndList [%s]" (String.concat "," subFLlist))
								) actions 
					in
					let index = List.map (fun i -> EVar (Bvariable (formatMapVIS ~axis1:i str))) (dwt (index_size-1) 0) in
					let indexIs k size= 
						let rec dec2Bin k n=
							if(n=0) then []
							else (k mod 2) :: (dec2Bin (k/2) (n-1))
						in 
						let bins = List.rev (dec2Bin k size) in
						List.map2 (fun i b -> (if (b=1) then i else (ENeg (i)))) index bins
					in
					let cases = List.map (fun k -> indexIs k index_size) values in
					let guards = List.map (fun case -> 
											"(" ^ (String.concat " bAND " ( List.map (fun be -> bExpr2FLbExprList be) case )) ^ ")"
										) cases
					in
					let terms = List.map2 (fun g s -> "Guard (" ^ g ^ ") (" ^ s ^ ")") guards actionsFL  in
					Printf.sprintf "TAndList [\n %s \n]"  (String.concat ",\n" terms)
				)
				| _ -> raise (Invalid_argument "In handle_mem_special->form->e1e2->fe11->fe1fe2")
			)
			| _ -> raise (Invalid_argument "In handle_mem_special->form->e1e2->fe11")
		)
		| _ -> raise (Invalid_argument "In handle_mem_special->form->e1e2")
	)
	| _ -> raise (Invalid_argument "In handle_mem_special: Handle only memory special formula")
	
let tag2FL tag node_set = 
	let t2f_helper tag node = 
		let ctx = make_z3_context () in
		let TAGINV (vars, forms) = tag node in
		(* directly solved *)
		let solveDirect = TFormulaSet.elements (
							List.fold_right TFormulaSet.add 
								(List.filter (fun f -> 
												(form_solve_directly f)||((contain_symbolic_const f)&&(form_not_contain_args f vars)&&(contain_vars f))) forms
								) TFormulaSet.empty 
						)
		in
		let solveDirectBitForms = List.map (fun f -> termForm2bitForm f) solveDirect in
		(*memory special*)
		let memorySpecial = TFormulaSet.elements (
							List.fold_right TFormulaSet.add 
								(List.filter (fun f -> 
												(form_not_solve_directly f)&&(contain_symbolic_const f)&&(form_not_contain_args f vars) && (not_contain_vars f)) forms
								) TFormulaSet.empty 
						)
		in
		(* SMT solved *)
		let formsToSolve = TFormulaSet.elements (List.fold_right TFormulaSet.add 
						(List.filter (fun f -> (not_contain_symbolic_const f) && (form_contain_args f vars)) forms) TFormulaSet.empty 
					)
		in 
		let formsToConcrete = TFormulaSet.elements (List.fold_right TFormulaSet.add 
						(List.filter (fun f -> ((form_contain_args f vars)&&(form_not_solve_directly f))) forms) TFormulaSet.empty 
					)
		in 
		if(forms = []) then "[[]]"
		else if ((List.length forms == 1) && ((List.hd forms)=Chaos)) then "[[]]"
		else if(vars = [] &&  ((List.length memorySpecial)!= 0)) then (
			(* special case: only one element in the list memorySpecial*)
			(* otherwise, must consider Cartesian product *)
			let fls = List.map (fun f -> handle_mem_special f) memorySpecial in
			(Printf.sprintf "[[%s]]" (String.concat "," fls))	
		)
		else if ((List.length formsToSolve) = 0) then (
			let solveDirectTrajForms = List.map (fun form -> bitForm2trajForm form ) solveDirectBitForms in
			let solveDirectTrajFLForms = List.flatten (List.map (fun traj -> (trajOcaml2trajFL traj)) solveDirectTrajForms) 
			in 
			(Printf.sprintf "[[%s]]" (String.concat "," solveDirectTrajFLForms))
		)
		else (
			let concreteList = getAllModels ctx formsToSolve vars in
			let concreteBitForms = List.map (fun sublist -> (
										(List.map (fun form -> (tag_inv_to_bit_form form vars sublist)) formsToConcrete)@solveDirectBitForms
								)) concreteList 
			in
			let concreteTrajForms = List.map ( fun subforms ->
										List.map (fun form -> bitForm2trajForm form ) subforms 
									) concreteBitForms
			in
			let concreteTrajFLForms = List.map (fun subtrajs -> (
										List.flatten (List.map (fun traj -> (trajOcaml2trajFL traj)) subtrajs)
								)) concreteTrajForms
			in
			let flattenOneD = List.map (
									( fun subFLlist -> Printf.sprintf "[%s]" (String.concat "," subFLlist))
								) concreteTrajFLForms
			in 
			(Printf.sprintf "[%s]" (String.concat "," flattenOneD))
			(*
			let concreteTrajFLForms = List.map (fun subtrajs -> (
										List.flatten (List.map (fun traj -> (trajOcaml2trajFL traj)) subtrajs)
								)) concreteTrajForms
			in
			List.iter (fun subtrajs -> (
				List.iter (fun traj -> Printf.printf "%s " traj) subtrajs;
				print_endline ""
			))concreteTrajFLForms
			*)
		)
	in
	let tag_set = List.map (fun (Vertex i) -> Printf.sprintf "(n = %d) => %s" i (t2f_helper tag (Vertex i))) node_set in
	let body = String.concat "\n    |" tag_set in
	Printf.sprintf 
"	
let tag aVert=
    val (Vertex n) = aVert in
    %s 
    | error \"no such node\"
;
"
body
		(*
		if ((List.length forms == 1) && ((List.hd forms)=Chaos)) then "[]"
		else if ((List.length vars) = 0) then "[]"
		else (
			let concreteList = getAllModels ctx forms vars in
			let concreteBitForms = List.map (fun sublist -> (
										List.map (fun form -> (tag_inv_to_bit_form form vars sublist)) forms 
								)) concreteList 
			in
			let concreteTrajForms = List.map ( fun subforms ->
										List.map (fun form -> bitForm2trajForm form ) subforms 
									) concreteBitForms
			in
			let concreteTrajFLForms = List.map (fun subtrajs -> (
										List.flatten (List.map (fun traj -> (trajOcaml2trajFL traj)) subtrajs)
								)) concreteTrajForms
			in
			let flattenOneD = List.map (
									( fun subFLlist -> Printf.sprintf "[%s]" (String.concat "," subFLlist))
								) concreteTrajFLForms
			in 
			(Printf.sprintf "[%s]" (String.concat "," flattenOneD))
			(*
			let concreteTrajFLForms = List.map (fun subtrajs -> (
										List.flatten (List.map (fun traj -> (trajOcaml2trajFL traj)) subtrajs)
								)) concreteTrajForms
			in
			List.iter (fun subtrajs -> (
				List.iter (fun traj -> Printf.printf "%s " traj) subtrajs;
				print_endline ""
			))concreteTrajFLForms
			*)
		)
	in
	let tag_set = List.map (fun (Vertex i) -> Printf.sprintf "(n = %d) => %s" i (t2f_helper tag (Vertex i))) node_set in
	let body = String.concat "\n    |" tag_set in
	Printf.sprintf 
"	
let tag aVert=
    val (Vertex n) = aVert in
    %s 
    | error \"no such node\"
;
"
body

*)

(*********************************** to forte input file *******************************************)

(** gsteSpec to forte verification program using STE with tag invariant *)
let toSTEfl model_name gs tag=
	let oc = open_out (Printf.sprintf "%s_toforte.fl" model_name) in
	let main_assertion_graph init_node node_set edge_set =
		match init_node with (Vertex inum) -> Printf.fprintf oc "let vertexI = Vertex %d;\n" inum ;
		Printf.fprintf oc "%s" ("let vertexL = [" ^ (String.concat "," (List.map (fun (Vertex i) -> Printf.sprintf "Vertex %d" i) node_set)) ^ "];\n" );
		Printf.fprintf oc "%s" ("let edgeL = [" ^ (String.concat "," (List.map (fun (Edge ((Vertex f),(Vertex t))) -> Printf.sprintf "Edge (Vertex %d) (Vertex %d)" f t) edge_set)) ^ "];\n");
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
		Printf.fprintf oc "%s" (
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
		Printf.fprintf oc "%s" (
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
	match gs with Graph (init_node , node_set, edge_set, ants, cons) -> (
		Printf.fprintf oc 
"
let ckt = load_exe \"%s.exe\";
load \"gsteSymReduce.fl\";
loadModel ckt;
" model_name;
		main_assertion_graph init_node node_set edge_set;
		ant_function init_node node_set edge_set ants;
		cons_function init_node node_set edge_set cons ;
		Printf.fprintf oc "%s" (tag2FL tag node_set);
		Printf.fprintf oc
"
let steSymbSimGoalfDirct ckt goal =
    	val (Goal bAnts assert) = goal 
in    val (TImply ant cons) = assert 
in    let ant = trajForm2FiveTuples ant 
in    let bP = itlist (\\bant. \\b. bant bAND b) bAnts bT 
in    let cons = trajForm2FiveTuples (Guard bP cons) 
then
  	//print (\"ant\\n\"^(trajFiveTuple2Str ant)^\"\\n\") fseq
  	//print (\"cons\\n\"^(trajFiveTuple2Str cons)^\"\\n\") fseq
   	(STE \"-w -y -S\"  ckt [] ant cons []) => T  
|	F
;

letrec check_helper [] ant0 = F
/\\ check_helper (cons0:conss) ant0 =
	let tmpGoal = (Goal [] (TImply ant0 (cons0))) in
	(steSymbSimGoalfDirct ckt tmpGoal) => T 
	| check_helper conss ant0
;
letrec check_next_ant conss [] = T 
/\\ check_next_ant conss (ant:antss) =
	let res = check_helper conss ant in
	res => check_next_ant conss antss
	| F
;
let check aEdge = 
	val (Edge from to) = aEdge in
	val (Vertex n) = from in
	val (Vertex n\') = to in
	let antss = map (\\x. TAndList ((ant aEdge): x)) (tag from) in
	let conss = map (\\x. TAndList ((cons aEdge):(map Next x))) (tag to) in
	check_next_ant conss antss => T 
	| F
;
itlist (\\x. \\y. x AND y) (map (\\e. check e) edgeL) T;
quit;
");
	close_out oc