type bVar = Bvariable of string
type bExpr = 
	| EVar of bVar
	| EAnd of bExpr * bExpr
	| EOr of bExpr * bExpr
	| ENeg of bExpr

type trajNode = Tnode of string

type trajForm = 
	| Is1 of trajNode
	| Is0 of trajNode
	| Next of trajForm
	| Guard of bExpr * trajForm
	| TAndList of trajForm list
	| TChaos


let rec bExpr2str be =
	match be with
	| EVar (Bvariable str) -> Printf.sprintf " %s(EVar) " str
	| EAnd (be1, be2) -> Printf.sprintf " EAnd (%s, %s) " (bExpr2str be1) (bExpr2str be2)
	| EOr (be1, be2) -> Printf.sprintf " EOr (%s, %s) " (bExpr2str be1) (bExpr2str be2)
	| ENeg e -> Printf.sprintf " ENeg (%s) " (bExpr2str e)


let print_trajForm form  =
	let rec trajForm2str f =
		match f with
		| Is1 (Tnode str) -> Printf.sprintf " Is1 %s " str
		| Is0 (Tnode str) -> Printf.sprintf " Is0 %s " str
		| Next tf 		  -> Printf.sprintf " Next (%s)" (trajForm2str tf)
		| Guard (be, tf)  -> Printf.sprintf " Guard (%s, %s)" (bExpr2str be) (trajForm2str tf)
		| TAndList ts     -> List.fold_right (^) (List.map (fun t -> trajForm2str t) ts) ""
		| TChaos 		  -> "TChaos"
	in
	print_endline (trajForm2str form)
