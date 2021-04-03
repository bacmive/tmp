(** boolean assertion graph types *)
type boolvar = BoolVar of string
type boolvec = BoolVec of string * boolvar list

type trajForm = 
	| Is1 of boolvar
	| Is0 of boolvar
	| IsN of boolvec * int
	| And of trajForm * trajForm
	| Chaos
