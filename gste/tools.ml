(** auxiliary tools: down and upt*)
let rec down : int -> int list = function 
    | 0 -> [0]
    | n -> (down (n-1))@[n]

(** e.g. upt 0 3 ==> [0;1;2;3] *)
let rec upt (f : int) (t : int) : int list =
    if f > t then []
    else f :: upt (f+1) t

(** e.g. dwt 3 0 ==> [3;2;1;0] *)
let rec dwt (t : int) (f : int) : int list =
	if f > t then []
	else t :: (dwt (t-1) f) 

(** power function *)	
let rec pow x n = 
	if n=0 then 1 
	else if n=1 then x
	else x * (pow x (n-1)) 