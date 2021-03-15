(********************************* Types ********************************************)
(** Variable Type*)
type var = 
	| Ident of string
	| Para of var * int 
	| Filed of var * string 


(** Constant Type*)
type scalar = 
	| Enum of string * string 
	| Index of int 
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

type statement1 = 
  | Parallel1 of statement list
  | If1 of formula * statement * statement
  | AtomStatement of assign

(* case statement *)
type formulaStaPair = formula * statement

let skip = Parallel []

let rec caseStatement : formulaStaPair list -> statement = function
  | [] -> skip
  | (f, gS)::t -> If (f, gS, (caseStatement t))

(* 
(* From Isabelle *)
(* assignment *)
type assign = 
  | Assign of var * expression

(* statement *)
type statement = assign

type generalizeStatement =
  | Parallel of statement list
  | If of formula * generalizeStatement * generalizeStatement

type generalizeStatement1 = 
  | Parallel1 of generalizeStatement1 list
  | If1 of formula * generalizeStatement1 * generalizeStatement1
  | AtomStatement of statement


(* case statement *)
type formulaStaPair = formula * generalizeStatement

let skip = Parallel []

let rec caseStatement : formulaStaPair list -> generalizeStatement = function
  | [] -> skip
  | (f, gS)::t -> If (f, gS, (caseStatement t))

 *)

(********************************* GSTE graph ********************************************)
type node = Vertex of int
type edge = Edge of node * node

type edgeToFormula = edge -> formula

(* gste Graph *)
type gsteSpec = Graph of node * edge list * edgeToFormula * edgeToFormula

(** the antecedent formula of an edge and the consequent formula of an edge*)
let antOf (init, edges, ant, cons) e  = ant e
let consOf (init, edges ,ant, cons) e = cons e

(** the source node of an edge and the sink node of an edge*)
let source : edge -> node = function
    | Edge (n, _) -> n

let sink : edge -> node = function
    | Edge (_, n) -> n

(** retrive the int of a node *)
let nodeToInt : node -> int = function
	| Vertex n -> n


(********************************* Semantics of Protocol ********************************************)
(** 
    circuit's formal model : statements
    construct gste assertion graph basing on the above model : graph
    check gste assertion graph : precondition, general induction and instantaneous implication
*)
(* circuit state *)
type state =  var -> scalar

let valueOfstate (v : var) (s : state) = s v


(* circuit *)
type circuit = Circuit of var list * var list * var list * statement list * statement list


(* function definition and interpretation function definition *)
type func = scalar list -> scalar
type interprete_func = string -> func

(* predicator definition and interpretation predicator definition *)
type predicate = scalar list -> bool
type interprete_pred = string -> predicate


(** auxiliary method for scalar *)
(** scalar to bool *)
let rec scalarToBool : scalar -> bool = function
	| BoolV b -> b
	| Index _ -> false
	| Enum (_, _) -> false
	| _ -> false

(** auxiliary method for assignment, statement *)
(** retrive corresponding expression of var in assignment list *)
let rec valOf (asgns : assign list) (v :var) : expression =
	match asgns with
	[] -> IVar v
	| x::xs -> let Assign (vx, ex) = x in 
				if vx = v then ex
				else valOf xs v


(** formal semantics of expression and formula *)
let rec expEval (itf : interprete_func) (e : expression) (s : state) : scalar =
	match e with
	IVar v -> s v
	| Const i -> i
	| IteForm (f, e1, e2) -> if (formEval itf f s) then (expEval itf e1 s) else (expEval itf e2 s)
	| Top -> TopVal
	| Unknown -> BottomVal
	| Uif (f, es) -> (itf f) (List.map (fun e -> expEval itf e s) es)
and formEval (itf : interprete_func) (f :formula) (s : state) : bool =
	match f with 
	Eqn (e1, e2) -> (expEval itf e1 s) = (expEval itf e2 s)
	| Uip (p, es) -> scalarToBool ((itf p) (List.map (fun e -> expEval itf e s) es))
	| AndForm (f1, f2) -> (formEval itf f1 s) && (formEval itf f2 s)
	| Neg f1 -> not ( formEval itf f1 s )
	| OrForm (f1, f2) -> (formEval itf f1 s) || (formEval itf f2 s)
	| ImplyForm (f1, f2) -> (not (formEval itf f1 s)) || (formEval itf f2 s)
	| Chaos -> true


(* substitution, weakest precondition, weakest preexpression *)
let rec substExp (e : expression) (asgns : assign list ) : expression =
	match e with
	IVar v -> valOf asgns v
	| Const i -> Const i
	| Top -> Top
	| Unknown -> Unknown
	| IteForm (f, e1, e2) -> IteForm ( substForm f asgns, substExp e1 asgns, substExp e2 asgns)
	| Uif (funcName, es) -> Uif (funcName, (List.map (fun e -> substExp e asgns) es))
and substForm (f : formula) (asgns : assign list) : formula = 
	match f with
	Eqn (l, r) -> Eqn (substExp l asgns, substExp r asgns)
	| AndForm (f1, f2) -> AndForm (substForm f1 asgns, substForm f2 asgns)
	| Neg f -> Neg (substForm f asgns)
	| OrForm (f1, f2) -> OrForm (substForm f1 asgns, substForm f2 asgns)
	| ImplyForm (f1, f2)  -> ImplyForm (substForm f1 asgns, substForm f2 asgns)
	| Uip (p, es) -> Uip (p, (List.map (fun e -> substExp e asgns) es))
	| Chaos -> Chaos

let rec preCond (f : formula) (st : statement) : formula =
	match st with
	Parallel asgns -> substForm f asgns
	| If (b, s1, s2) -> OrForm ( AndForm (b, preCond f s1), AndForm ( Neg b, preCond f s2) )
	
let rec preExp (e : expression) (st : statement) : expression = 
	match st with
	Parallel asgns -> substExp e asgns
	| If (b, s1, s2) -> IteForm (b, preExp e s1, preExp e s2)


let rec down : int -> int list = function 
	| 0 -> [0]
	| n -> (down (n-1))@[n]

let rec upt (f : int) (t : int) : int list =
	if f > t then []
	else f :: upt (f+1) t

let readArray (v : var) (bound : int) (e : expression) : expression =
	caseExpression (List.map (fun i -> (Eqn (e, (Const (Index i))), IVar (Para (v, i)))) (down bound) )

let writeArray (v : var) (bound : int) (addressE : expression) (ce : expression) : assign list =
	List.map 
        (fun i -> Assign((Para(v, i)), IteForm (Eqn (addressE, Const (Index i)), ce, (IVar (Para (v, i)))))) 
        (down bound)
					

(********************************* ring-buffer FIFO ********************************************)
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
	| depth -> readArray (Ident "mem") depth (IVar (Ident "head"))

let pushDataFormula : int -> formula = function 
	| i -> AndForm (pushFormula, Eqn (dataIn, Const (Index i)))

let popDataFormula (i : int)  (depth : int) : formula = 
	Eqn ((dataOut depth), Const (Index i)) 


(********************************* ring-buffer FIFO model ********************************************)
let branch1 = 
	let s1 = Assign (Ident "head", Const (Index 0)) in
	let s2 = Assign (Ident "tail", Const (Index 0)) in
	let s3 = Assign (Ident "empty", high) in
	let s4 = Assign (Ident "full", low) in
	Parallel [s1; s2; s3; s4]
 
let branch2 = 
	let s1 = List.map (fun i -> Assign ( Para (Ident "mem", i), IteForm (Eqn (tail, Const (Index i)), dataIn, IVar (Para(Ident "mem", i))))) (down 3) in
	let tailPlus = Uif("+", [tail; Const (Index 1)]) in
	let s2 = Assign (Ident "tail", tailPlus) in
	let s3 = Assign (Ident "empty",low) in
	let s4 = Assign (Ident "full", IteForm (Eqn (tailPlus, head), high, full)) in
	Parallel (List.append [s2; s3; s4] s1)

let branch3 = 
	let headPlus= Uif("+", [head; (Const (Index 1))]) in
	Parallel [
		Assign (Ident "empty",IteForm (Eqn(tail, headPlus), high, empty));
		Assign (Ident "full", low);
		Assign (Ident "head",  headPlus)] 

let ringBuffifo = 
	caseStatement [(Eqn (rst, high), branch1);
				(AndForm(Eqn (push, high), noFullFormula), branch2);
				(AndForm(Eqn (pop, high), Eqn (empty, low)), branch3)]


(********************************* ring-buffer FIFO gsteSpec ********************************************)
let last = 3
let vectexI = Vertex 0
let vectexL = vectexI :: (List.map (fun i -> Vertex i) (upt 1 3))

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


(********************************* ring-buffer FIFO gste ********************************************)
let rec applyPlusN (e : expression) (n : int) : expression = 
	if n = 0 then e 
	else Uif ("+", [applyPlusN e (n-1) ; Const (Index 1)])

type nodeTagFunList = node -> formula list

let tagFunOfRbFifo (i : int) (n : node): formula list =
	let x = nodeToInt n in
	let dataE = Const (Index i) in
	let lastE = Const (Index last) in
	(* let twiceLastPlus3 = Const (Index (2*last+3)) in *)
	(* let tailPlus = Uif ("+", [tail; Const (Index 1)]) in *)
	(* let headPlus = Uif ("+", [head; Const (Index 1)]) in *)
	(
		if (x=0) then [] 
		else
		(	
			if ((x mod 2)=1) then 
			( 
				if (x=1) then [Eqn (tail, head); Eqn (empty, high);  Eqn (full, low); Uip ("le", [head; lastE])]
				else 
				(
					if (x=2*last+3) then [Eqn (tail, applyPlusN head (x/2)); Eqn (empty, low); Eqn (full, high); Uip ("le", [head; lastE])]
					else [Eqn (tail, applyPlusN head (x/2)); Eqn (empty, low); Eqn (full, low); Uip ("le", [head; lastE])]
				)
			)
			else 
			(
				if (x=2*last +4) then [Eqn (tail, applyPlusN head (x/2 -1)); Eqn (empty, low); Eqn (full, high); Uip ("le", [head; lastE]); Eqn (readArray mem last (applyPlusN head (x/2 -2)), dataE)]
				else [Eqn (tail, applyPlusN head (x/2 -1)); Eqn (empty, low); Eqn (full, low); Uip ("le", [head; lastE]); Eqn (readArray mem last (applyPlusN head (x/2 -2)), dataE)]
			)
		)
	)

(** verification on SMT*)

(** expression TO Z3 expression *)
