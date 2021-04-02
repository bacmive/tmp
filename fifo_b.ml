(** without tag invariants *)
type boolvar = BoolVar of string

(*verilog's variable ==> ocaml's variable *)
let dataOfInput = List.map (fun i -> "dataIn"^ (string_of_int i)) (upt 0 (data_size-1))
let dataOfOutput = List.map (fun i -> "dataOut"^ (string_of_int i)) (upt 0 (data_size-1))
let varOfDataIn = List.map (fun s -> BoolVar s) dataOfInput
let varOfDataOut =  List.map (fun s -> BoolVar s) dataOfOutput
let clk0 = BoolVar "CLK0" 
let rst0 = BoolVar "rst0"
let push0 =  BoolVar "push0"
let pop0 = BoolVar "pop0"
let full0 = BoolVar "full0"
let empty0 = BoolVar "empty0"


type trajForm = 
	| Is1 of boolvar
	| Is0 of boolvar
	| Next of trajForm
	| Guard of trajForm * trajForm
	| TAndList of trajForm list
	| Chaos

let isb (a : string) (b : boolvar ) =
	TAndList [Guard(Is1 b, Is1 (BoolVar a)); Guard(Is0 b, Is0 (BoolVar a))]

let bvAre (t1 : string list) (t2 : boolvar list) = 
	TAndList (List.map (fun (a,b) -> isb a b) (List.combine t1 t2)) 


(* *B, e.g. myclkB, for boolean level*)
let myclkB = TAndList [Is0 clk0; Next (Is1 clk0)]                	 							(*时钟信号的模拟*)
let rstB = TAndList [Is1 rst0; myclkB]                         									(*低电平时复位*)
let pushB = TAndList [Is0 rst0; Is1 push0; Is0 pop0; myclkB]     								(*上升沿时push*)
let popB = TAndList [Is0 rst0; Is0 push0; Is1 pop0; myclkB]       								(*上升沿时pop*)
let nPushPopB = TAndList [Is0 rst0; Is0 push0; Is0 pop0; myclkB]								(*无操作*)
let pushDataB d = TAndList [pushB; bvAre dataOfInput d; myclkB]            						(*push一个数据D*)
let pushDataBi d i = TAndList [pushB; isb (List.nth dataOfInput i) (List.nth d i); myclkB]		(*push数据D的某一位*)
let fullB = Is1 full0                                             								(*标志位的判断*)
let nFullB = Is0 full0
let emptyB = Is1 empty0
let nEmptyB = Is0 empty0
let popDataB d = bvAre dataOfOutput d                                    						(*判断pop*)
let popDataBi d i = isb (List.nth dataOfOutput i) (List.nth d i)                        		(*判断pop数据的某一位*)

let antOfRbFifo_bool (e : edge) : trajForm =
	let f = nodeToInt (source e) in
	let t = nodeToInt (sink e) in 
	(	
		if (f = 0) then rstB 
		else if (f = t) then nPushPopB 
		else if ((f mod 2)=1) then 
		(	
			if ((f + 2)=t) then pushB 
			else if (f=(t+2)) then popB
			else pushDataB varOfDataIn
		)
		else popB
	)

let consOfRbFifo_bool (e : edge) : trajForm =
	let f = nodeToInt (source e) in
	let t = nodeToInt (sink e) in
	(
		if ( (f mod 2) = 1 && (t mod 2) = 1 ) then 
		(
			if f = 1 then TAndList [emptyB; nFullB]
			else if f = (2*last +3) then TAndList [nEmptyB; fullB]
			else TAndList [nEmptyB; nFullB]
		)
		else if ( f = 4 && t = 1 ) then popDataB varOfDataIn
		else if ( f = (2*last+4) ) then TAndList [nEmptyB; fullB]
		else if ( f = 1 ) then TAndList [emptyB; nFullB]
		else if ( f <> 0) then TAndList [nEmptyB; nFullB]
		else Chaos
	)