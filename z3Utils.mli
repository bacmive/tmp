module Solver = Z3.Solver
module Boolean = Z3.Boolean
module Expr = Z3.Expr
module Z3Array = Z3.Z3Array
module Arithmetic = Z3.Arithmetic
module BitVector = Z3.BitVector
module Model = Z3.Model

exception InvalidExpression
exception UnfoundFunction
exception UnMatchedExpr
exception UnMatchedIVar
exception UnMatchedPara
exception UnMatchedIndex
exception UnMatchedUIF

val expr2z3Expr : Z3.context -> Types.expression -> Expr.expr

val form2z3expr : Z3.context -> Types.formula -> Z3.Expr.expr

module ExprSet :
  sig
    type elt = Expr.expr
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
  end
  
(** get all models from an expression (Z3.Expr.expr)*)  
val get_all_models : Z3.context -> Z3.Expr.expr -> Expr.expr list -> unit
