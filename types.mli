type circuit_node = 
  | Input of string 
  | Latch of string 
  | Output of string

type scalar = 
  | IntC of int * int 
  | BoolC of bool

type sort = 
  | Int of int 
  | Bool 
  | Array of int * int

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

type formulaExpPair = formula * expression

val caseExpression : formulaExpPair list -> expression

type assign = Assign of var * expression

type statement =
  | Parallel of assign list
  | If of formula * statement * statement

type formulaStaPair = formula * statement

val skip : statement

val caseStatement : formulaStaPair list -> statement

val down : int -> int list

val upt : int -> int -> int list

type node = Vertex of int

type edge = Edge of node * node

type edgeToFormula = edge -> formula

type gsteSpec = Graph of node * edge list * edgeToFormula * edgeToFormula

val source : edge -> node

val sink : edge -> node

val nodeToInt : node -> int
