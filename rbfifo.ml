(********************************* Types ********************************************)
(** Variable Type*)
type var = 
	| Ident of string
	| Para of var * int 
	| Field of var * string 
	

(** Constant Type*)
type scalar = 
	| Int of int 
	| BoolV of bool
	| TopVal 
	| BottomVal

(* expression and formula *)
type expression = 
    | IVar of var 
    | Const of scalar
    | IteForm of formula * expression * expression
    | Uif of string * expression list
    | Top
    | Unknown
and formula = 
    | Eqn of expression * expression 
    | Uip of string * expression list 
    | AndForm of formula * formula 
    | Neg of formula 
    | OrForm of formula * formula 
    | ImplyForm of formula * formula 
    | Chaos

(* case expression *)
type formulaExpPair = formula * expression

let rec caseExpression : formulaExpPair list -> expression = function
  | [] -> Unknown
  | (f, e)::t -> IteForm (f, e, (caseExpression t))


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

(** auxiliary tools: down and upt*)
let rec down : int -> int list = function 
	| 0 -> [0]
	| n -> (down (n-1))@[n]

let rec upt (f : int) (t : int) : int list =
	if f > t then []
	else f :: upt (f+1) t
	
(** Array Manipulation with Expression and Statement*)
let readArray (v : var) (bound : int) (e : expression) : expression =
	caseExpression (List.map (fun index -> (Eqn (e, (Const (Int index))), IVar (Para (v, index)))) (down bound) )

let writeArray (v : var) (bound : int) (addressE : expression) (ce : expression) : assign list =
	List.map 
        (fun i -> Assign((Para(v, i)), IteForm (Eqn (addressE, Const (Int i)), ce, (IVar (Para (v, i)))))) 
        (down bound)

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


(*********************************** rbFIFO GSTE assertion graph *******************************************)
let mem : var = Ident "mem"

let rst : expression = IVar (Ident "reset")
let push : expression = IVar (Ident "push")
let pop : expression = IVar (Ident "pop")
let dataIn : expression = IVar (Ident "dataIn")
let low : expression = Const (BoolV false)
let high : expression = Const (BoolV true)
let empty : expression = IVar (Ident "empty")
let full : expression = IVar (Ident "full")
let tail : expression = IVar (Ident "tail")
let head : expression = IVar (Ident "head")

let fullFormula : formula = Eqn (full, high)
let rstFormula : formula = Eqn (rst, high)
let emptyFormula : formula = Eqn (empty, high)
let pushFormula : formula = AndForm (AndForm (Eqn (rst, low), Eqn (push, high)), Eqn (pop, low)) 
let popFormula : formula = AndForm (AndForm (Eqn (rst, low), Eqn (push, low)), Eqn (pop, high))
let noPopPushFormula : formula = AndForm (AndForm (Eqn (rst, low), Eqn (push, low)), Eqn (pop, low))
let noFullFormula :formula = Neg fullFormula
let noEmptyFormula : formula = Neg emptyFormula

let dataOut : int -> expression = function 
	| depth -> readArray mem depth head

let pushDataFormula : int -> formula = function 
	| d -> AndForm (pushFormula, Eqn (dataIn, Const (Int d)))

let popDataFormula (d : int)  (depth : int) : formula = 
	Eqn ((dataOut depth), Const (Int d)) 

let last = 3
let vectexI = Vertex 0
let vectexL = [vectexI; (Vertex 1)]@(List.map (fun i -> Vertex i) (upt 3 (2*last+4)))

let edgeL = 
	let e1 = Edge (Vertex 0, Vertex 1) in
	let e2 = Edge (Vertex 1, Vertex 3) in 
	let e3 = Edge (Vertex 1, Vertex 4) in
	let e4_list = List.map (fun i -> Edge (Vertex (2*i+1), Vertex (2*i+1) )) (upt 0 (last+1)) in (*self loop*)
	let e5_list = List.map (fun i -> Edge (Vertex (2*i+2), Vertex (2*i+2) )) (upt 1 (last+1)) in (*self loop*)
	let e6_list = List.map (fun i -> Edge (Vertex (2*i+1), Vertex (2*i+3) )) (upt 1 (last)) in  
	let e7_list = List.map (fun i -> Edge (Vertex (2*i+1), Vertex (2*i+4) )) (upt 0 (last)) in
	let e8_list = List.map (fun i -> Edge (Vertex (2*i+3), Vertex (2*i+1) )) (upt 0 (last)) in
	let e9_list = List.map (fun i -> Edge (Vertex (2*i+4), Vertex (2*i+2) )) (upt 1 (last)) in
	let e10 = Edge (Vertex 4, Vertex 1) in
	[e1; e2; e3; e10]@e4_list@e5_list@e6_list@e7_list@e8_list@e9_list

let antOfRbFifo (d : int) (e : edge) : formula =
	let f = nodeToInt (source e) in
	let t = nodeToInt (sink e) in 
	(	
		if (f = 0) then rstFormula 
		else if (f = t) then noPopPushFormula 
		else if ((f mod 2)=1) then 
		(	
			if ((f + 2)=t) then pushFormula 
			else if (f=(t+2)) then popFormula
			else pushDataFormula d
		)
		else popFormula
	)

let consOfRbFifo (d : int) (e :edge) : formula =
	let f = nodeToInt (source e) in
	let t = nodeToInt (sink e) in
	(
		if ( (f mod 2) = 1 && (t mod 2) = 1 ) then 
		(
			if f = 1 then AndForm (emptyFormula, noFullFormula)
			else if f = (2*last +3) then AndForm (noEmptyFormula, fullFormula)
			else AndForm (noEmptyFormula, noFullFormula)
		)
		else if ( f = 4 && t = 1 ) then popDataFormula last d
		else if ( f = (2*last+4) ) then AndForm (noEmptyFormula, fullFormula)
		else if ( f = 1 ) then AndForm (emptyFormula, noFullFormula)
		else if ( f <> 0) then AndForm (noEmptyFormula, noFullFormula)
		else Chaos
	)
	
let rbFifoGsteSpec ( d : int ) : gsteSpec = 
	Graph (vectexI, edgeL, antOfRbFifo d, consOfRbFifo d)

(*********************************** rbFIFO GSTE tag function *******************************************)
let rec applyPlusN (e : expression) (n : int) : expression = 
	if n = 0 then e 
	else Uif ("+", [applyPlusN e (n-1) ; Const (Int 1)])

let tagFunOfRbFifo (d : int) (n : node) : formula list = 
	let dataI = Const (Int d) in 
	let lastV = Const (Int last) in 
	let x = nodeToInt n in
	(
		if (x=0) then []
		else 
		(
			if ( (x mod 2)=1 ) then 
			(
				if ( x=1 ) then [ Eqn (tail, head); Eqn (empty, high); Eqn (full, low); Uip ("le", [head; lastV]) ] 
				else 
				(
					if ( x=(2*last+3) ) then [Eqn (tail, head); Eqn (empty, low); Eqn (full, high); Uip ("le", [head; lastV])]
					else [Eqn (tail, applyPlusN head (x/2)); Eqn (empty, low); Eqn (full, low)]
				)
			)
			else
			(
				if( x=(2*last +4) ) then [Eqn (tail, head); Eqn (empty, low); Eqn (full, high); Eqn (readArray mem last (applyPlusN head last), dataI); Uip ("le", [head; lastV])]
				else [Eqn (tail, applyPlusN head (x/2-1)); Eqn (empty, low); Eqn (full, low); Eqn (readArray mem last (applyPlusN head (x/2-2)), dataI); Uip ("le", [head; lastV])]
			)
		)
	)




let () = 
	let rec prt vls = 
	match vls with
	[] -> ()
	| (Vertex i) :: t -> print_endline (string_of_int i);  prt t in
	prt vectexL
	