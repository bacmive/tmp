(** circuit node type *)
type c_node = CNode of string 

(** gate type *)
type gate = 
	| Input of c_node
	| Output of c_node
	| AndGate of gate * gate * gate
	| OrGate of gate * gate * gate
	| NegGate of gate * gate
	| Latch of gate * gate * bool

(** circuit model type *)
type circuit = Circuit of gate list
	