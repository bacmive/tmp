type scalar =
    IntC of int * int
  | BoolC of bool
  | SymbIntC of string * int
  | SymbBoolC of string
type sort = Int of int | Bool | Array of int * sort
type var = Ident of string * sort | Para of var * expression
and expression =
    IVar of var
  | Const of scalar
  | Uif of string * expression list
  | IteForm of formula * expression * expression
and formula =
    Eqn of expression * expression
  | AndForm of formula * formula
  | Neg of formula
  | OrForm of formula * formula
  | ImplyForm of formula * formula
  | Chaos
val number : int -> int -> expression
val plus : expression -> expression -> expression
type formulaExpPair = formula * expression
val caseExpression : formulaExpPair list -> expression
val readArray : var -> expression -> expression
type node = Vertex of int
type edge = Edge of node * node
type edgeToFormula = edge -> formula
type gsteSpec =
    Graph of node * node list * edge list * edgeToFormula * edgeToFormula
val source : edge -> node
val sink : edge -> node
val nodeToInt : node -> int
type tag_invs = TAGINV of expression list * formula list
type vertexToTagInv = node -> tag_invs
module TExpressionSet :
  sig
    type elt = expression
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
  end
module TFormulaSet :
  sig
    type elt = formula
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
  end
val contain_vars : formula -> bool
val not_contain_vars : formula -> bool
val contain_symbolic_const : formula -> bool
val not_contain_symbolic_const : formula -> bool
val expr_in_expr : expression -> expression -> bool
val expr_in_form : formula -> expression -> bool
val expr_not_in_form : formula -> expression -> bool
val expr_in_args : 'a -> 'a list -> bool
val expr_not_in_args : 'a -> 'a list -> bool
val form_contain_args : formula -> expression list -> bool
val form_not_contain_args : formula -> expression list -> bool
val form_solve_directly : formula -> bool
val form_not_solve_directly : formula -> bool
