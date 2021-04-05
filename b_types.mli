type scalar = IntC of int * int | BoolC of bool
type sort = Int of int | Bool | Array of int * sort
type var =
    Ident of string * sort
  | Para of var * expression
  | Field of var * string
and expression =
    IVar of var
  | Const of scalar
  | IteForm of formula * expression * expression
  | Uif of string * expression list
  | Top
  | Unknown
and formula =
    Eqn of expression * expression
  | AndForm of formula * formula
  | Neg of formula
  | OrForm of formula * formula
  | ImplyForm of formula * formula
  | Uip of string * expression list
  | Chaos
val termScalar2bitVecConst : scalar -> scalar list
val formatMapVIS : ?axis1:int -> ?axis2:int -> string -> string
val termVar2bitVecVar : var -> var list
val termExp2bitExp : expression -> expression list
val termForm2bitForm : formula -> formula
val print_var : var -> string
val print_expr : expression -> string
val print_form : formula -> string
type node = Vertex of int
type edge = Edge of node * node
type edgeToFormula = edge -> formula
type gsteSpec = Graph of node * edge list * edgeToFormula * edgeToFormula
val source : edge -> node
val sink : edge -> node
val nodeToInt : node -> int
