val visCLKname : string
val yosysCLKname : string
val formatMapVIS : ?axis1:int -> ?axis2:int -> string -> string
val formatMapYosys : ?axis1:int -> ?axis2:int -> string -> string
val termScalar2bitVecConst : Types.scalar -> Types.scalar list
val termVar2bitVecVar : Types.var -> Types.var list
val termExp2bitExp : Types.expression -> Types.expression list
val termForm2bitForm : Types.formula -> Types.formula
val bvar2str : Types.var -> string
val bitExpr2str : Types.expression -> string
val bitForm2str : Types.formula -> string
val symbScalar2TrajVar : Types.scalar -> Trajectory.bVar list
val bitForm2trajForm : Types.formula -> Trajectory.trajForm
val trajOcaml2trajFL : Trajectory.trajForm -> string list
val tag_inv_to_bit_form :
  Types.formula -> Types.expression list -> int list -> Types.formula
val handle_mem_special : Types.formula -> string
val tag2FL : (Types.node -> Types.tag_invs) -> Types.node list -> string
val toSTEfl :
  string -> Types.gsteSpec -> (Types.node -> Types.tag_invs) -> unit
