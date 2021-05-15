(** 在ocaml中定义用于转化的变量、公式类型，对应于forte中的表达式 *)

type bVar = Bvariable of string
type bExpr = 
	| EVar of bVar
	| EAnd of bExpr * bExpr
	| EOr of bExpr * bExpr
	| ENeg of bExpr

let rec bExpr2FLbExprList be =
	match be with
	| EVar (Bvariable name)-> Printf.sprintf "(bvariable \"%s\")" name
	| EAnd (be1, be2) -> "(" ^ (bExpr2FLbExprList be1)^ "bAND" ^(bExpr2FLbExprList be2)^ ")"
	| EOr (be1, be2) -> "(" ^ (bExpr2FLbExprList be1)^ "bOR" ^(bExpr2FLbExprList be2) ^ ")"
	| ENeg be0 -> "(bNOT (" ^ (bExpr2FLbExprList be0) ^"))"

type trajNode = Tnode of string

type trajForm = 
	| Is1 of trajNode
	| Is0 of trajNode
	| Next of trajForm
	| Guard of bExpr * trajForm
	| TAndList of trajForm list
	| TChaos

let isb p tnode = TAndList [Guard (p, Is1 tnode); Guard ((ENeg p), Is0 tnode)]

(** 将trajectory formula 转换为字符串*)
let rec bExpr2str be =
	match be with
	| EVar (Bvariable str) -> Printf.sprintf " %s(EVar) " str
	| EAnd (be1, be2) -> Printf.sprintf " EAnd (%s, %s) " (bExpr2str be1) (bExpr2str be2)
	| EOr (be1, be2) -> Printf.sprintf " EOr (%s, %s) " (bExpr2str be1) (bExpr2str be2)
	| ENeg e -> Printf.sprintf " ENeg (%s) " (bExpr2str e)

let rec trajForm2str f =
	match f with
	| Is1 (Tnode str) -> Printf.sprintf " Is1 %s" str
	| Is0 (Tnode str) -> Printf.sprintf " Is0 %s" str
	| Next tf 		  -> Printf.sprintf " Next (%s)" (trajForm2str tf)
	| Guard (be, tf)  -> Printf.sprintf " Guard (%s,%s)" (bExpr2str be) (trajForm2str tf)
	| TAndList ts     -> Printf.sprintf " TAndList [%s]" (String.concat ";" (List.map (fun t -> trajForm2str t) ts))
	| TChaos 		  -> "TChaos"

