(** Constant Type*)
type scalar = 
	| IntC of int * int
	| BoolC of bool

(* variables' type*)	
type sort = 
	| Int of int
	| Bool
	| Array of int * int (*(index, data) BitVector * BitVector)*)

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

(* case expression *)
type formulaExpPair = formula * expression

let rec caseExpression : formulaExpPair list -> expression = function
  | (f, e)::[] -> IteForm (f, e, e)
  | (f, e)::t -> IteForm (f, e, (caseExpression t))
  | _ -> raise (Failure "case expression format error")
 
(* assignment *)
type assign = Assign of var * expression

(* statement *)
type statement =
  | Parallel of assign list
  | If of formula * statement * statement

(* case statement *)
type formulaStaPair = formula * statement

let skip = Parallel []

let rec caseStatement : formulaStaPair list -> statement = function
  | [] -> skip
  | (f, gS)::t -> If (f, gS, (caseStatement t))


(*********************************** GSTE assertion graph *******************************************)
type node = Vertex of int
type edge = Edge of node * node

type edgeToFormula = edge -> formula
(* gste Graph *)
type gsteSpec = Graph of node * edge list * edgeToFormula * edgeToFormula

(** the source node of an edge and the sink node of an edge*)
let source : edge -> node = function
    | Edge (n, _) -> n

let sink : edge -> node = function
    | Edge (_, n) -> n

(** retrive the int of a node *)
let nodeToInt : node -> int = function
	| Vertex n -> n
