type bVar = Bvariable of string
type bExpr =
    EVar of bVar
  | EAnd of bExpr * bExpr
  | EOr of bExpr * bExpr
  | ENeg of bExpr
val bExpr2FLbExprList : bExpr -> string
type trajNode = Tnode of string
type trajForm =
    Is1 of trajNode
  | Is0 of trajNode
  | Next of trajForm
  | Guard of bExpr * trajForm
  | TAndList of trajForm list
  | TChaos
val isb : bExpr -> trajNode -> trajForm
val bExpr2str : bExpr -> string
val trajForm2str : trajForm -> string
