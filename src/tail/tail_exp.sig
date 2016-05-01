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
     val remove     : dom * 'a map -> 'a map option
  end

  (* TAIL AST *)
  datatype uexp =
           Var of var * typ
         | I of Int32.int
         | D of real
         | B of bool
         | C of word
         | Iff of uexp * uexp * uexp * typ
         | Vc of uexp list * typ
         | Op of opr * uexp list * typ
         | Let of var * typ * uexp * uexp * typ
         | Fn of var * typ * uexp * typ
         | Tuple of uexp list * typ
         | Prj of int * uexp * typ
                                        
  (* Type environment *)
  type env
  val lookup   : env -> var -> typ option
  val emptyEnv : env
  val add      : env -> var -> typ -> env

  (* Type-checking *)
  datatype 't report = OK of 't | ERR of string
  val typeExp  : env -> uexp -> typ report

  (* Walk the syntax tree, mark each operator that works on
     shape-types (appends "V" to the end of the operation name) *)
  val resolveShOpr : uexp -> uexp

  (* Alternative constructors that checks the types upon
     construction. May raise Fail. *)
  val Iff_e    : uexp * uexp * uexp -> uexp
  val Vc_e     : uexp list -> uexp
  val Op_e     : opr * uexp list -> uexp
  val Let_e    : var * typ * uexp * uexp -> uexp
  val Fn_e     : var * typ * uexp -> uexp
  val Tuple_e  : uexp list -> uexp
  val Prj_e    : int * uexp -> uexp
  val unTuple  : uexp -> uexp list option
                                   
  (* Get the type of a TAIL-expression *)
  val typeOf   : uexp -> typ

  (* Values & Stores *)
  type value
  type denv
  val emptyDEnv  : denv
  val addDE    : denv -> var -> value -> denv
  val Dvalue   : real -> value
  val unDvalue : value -> real
  val Uvalue   : value          (* = Dvalue 0.0 ? *)

  (* Evaluate an expression in the given environment *)
  val eval : denv -> uexp -> value

  (* Pretty printing variables, values and characters *)
  val ppVar  : var -> string
  val pr_value : value -> string
  val pr_char : word -> string
end
