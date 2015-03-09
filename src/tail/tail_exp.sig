(** Tail expression terms. This interface exposes the structure of
    TAIL expressions. It also provides evaluation and type
    checking/inference functionality.
*)

signature TAIL_EXP = sig
  
  structure T : TAIL_TYPE

  type rnk = T.rnk
  type typ = T.typ
  type opr = string

  (* Variables are initially immutable, but if we discover an
     index-assignment, we change the variable it is updating to be
     mutable. *)
  eqtype var
  val newVar : unit -> var         (* create new immutable variable *)
  val mutableVar : var -> bool ref (* get reference to get/set whether this variable is mutable *)
  val ppVar  : var -> string

  (* Finite map's keyed by TAIL-variables *)
  structure FM : sig
     type dom = var
     type 'b map       
     val empty      : 'b map
     val singleton  : dom * 'b -> 'b map
     val lookup     : 'b map -> dom -> 'b option
     val add        : dom * 'b * 'b map -> 'b map
     val plus       : 'a map * 'a map -> 'a map
     val mergeMap   : ('a * 'a -> 'a) -> 'a map -> 'a map -> 'a map
     val composemap : ('a -> 'b) -> 'a map -> 'b map
  end

  (* TAIL AST *)
  datatype exp =
           Var of var * typ
         | I of Int32.int
         | D of real
         | B of bool
         | C of word
         | Iff of exp * exp * exp * typ
         | Vc of exp list * typ
         | Op of opr * exp list * typ
         | Let of var * typ * exp * exp * typ
         | Fn of var * typ * exp * typ

  (* Type environment *)
  type env
  val lookup   : env -> var -> typ option
  val empEnv   : env
  val add      : env -> var -> typ -> env

  (* Type-checking *)
  datatype 't report = OK of 't | ERR of string
  val typeExp  : env -> exp -> typ report

  (* Walk the syntax tree, mark each operator that works on
     shape-types (appends "V" to the end of the operation name) *)
  val resolveShOpr : exp -> exp

  (* Alternative constructors that checks the types upon
     construction. May raise Fail. *)
  val Iff_e    : exp * exp * exp -> exp
  val Vc_e     : exp list -> exp
  val Op_e     : opr * exp list -> exp
  val Let_e    : var * typ * exp * exp -> exp
  val Fn_e     : var * typ * exp -> exp

  (* Get the type of a TAIL-expression *)
  val typeOf   : exp -> typ

  (* Values & Stores *)
  type value
  type denv
  val empDEnv  : denv
  val addDE    : denv -> var -> value -> denv
  val Dvalue   : real -> value
  val unDvalue : value -> real
  val Uvalue   : value          (* = Dvalue 0.0 ? *)

  (* Evaluate an expression in the given environment *)
  val eval : denv -> exp -> value

  (* Pretty printing values and characters *)
  val pr_value : value -> string
  val pr_char : word -> string

end
