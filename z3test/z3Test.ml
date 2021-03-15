let context = Z3.mk_context []
let solver = Z3.Solver.mk_solver context None

let xsy = Z3.Symbol.mk_string context "x"
let x = Z3.Boolean.mk_const context xsy

let () = Z3.Solver.add solver [x]

let main () =
    match Z3.Solver.check solver [] with
    | UNSATISFIABLE -> Printf.printf "unsat\n"
    | UNKNOWN -> Printf.printf "unknown"
    | SATISFIABLE ->
        match Z3.Solver.get_model solver with
        | None -> ()
        | Some model ->
            Printf.printf "%s\n"
                (Z3.Model.to_string model)

let () = main ()

let ctx=Z3.mk_context  [("model", "true"); ("proof", "false")] in 
	let v1=(Z3.Arithmetic.Integer.mk_const_s ctx "x") in
	let v2=(Z3.Arithmetic.Integer.mk_const_s ctx "y") in
	let res=(Z3.Arithmetic.Integer.mk_const_s ctx "z") in
	let sum=Z3.Arithmetic.mk_add ctx [ v1 ; v2] in 
	let phi=Z3.Boolean.mk_eq ctx sum res in
	let solver = (Z3.Solver.mk_solver ctx None) in
	let _= Z3.Solver.add solver [phi] in
	let is_sat=Z3.Solver.check solver [] in 
	match is_sat with 
		| UNSATISFIABLE -> Printf.printf "unsat";
		| SATISFIABLE -> Printf.printf "sat";
		| UNKNOWN -> Printf.printf "unknown";