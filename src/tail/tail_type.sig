(** Type structure for TAIL programs. The interface is highly
    imperative as types may be unified using operations such as
    relateR, relateR2, unifyR, unifyB, unify, and subtype.

      r ::=                          Ranks
             rv                         rank variable
             i                          immediate rank
      k ::=                          Base types
             bv                         base type variable
             bool                       boolean
             int                        integer
             double                     double
             complex                    complex
             char                       character
      t ::=                          Types
             tv                         type variable
             [k]r                       array type with base type b and rank r
             <k>r                       vector type with base type b and length r (i.e., shape vector)
             S_k(r)                     singleton type of base type b and value r
             SV_k(r)                    singleton one-element vector type containing the value r of type b
             t -> t                     function type
             t1*...*tn                  tuple
*)

signature TAIL_TYPE = sig
  datatype unify_result  = SUCCESS | ERROR of string

  (* Ranks (\rho) *)
  type rnk
  val rnk       : int -> rnk
  val unRnk     : rnk -> int option
  val RnkVar    : unit -> rnk (* generate new shape variable *)
  val RnkVarCon : (int-> unify_result) -> rnk
  (* val relateR   : (int -> int) * (int -> int) -> rnk -> rnk -> string option *)
  (* val relateR2  : {f12: int*int->int,f13:int*int->int,f23:int*int->int} ->  *)
  (*                 rnk -> rnk -> rnk -> string option *)
  val unifyR    : rnk -> rnk -> unify_result (* unify two shape variables. *)
  val prRnk     : rnk -> string

  (* Base types (\kappa) *)
  type bty
  val IntB     : bty
  val BoolB    : bty
  val DoubleB  : bty
  val ComplexB : bty
  val CharB    : bty
  val isInt    : bty -> bool
  val isDouble : bty -> bool
  val isComplex: bty -> bool
  val isBool   : bty -> bool
  val isChar   : bty -> bool
  val TyVarB   : unit -> bty    (* generate new base type variable *)
  val unifyB   : bty -> bty -> unify_result (* unify two base type variables *)
  val prBty    : bty -> string

  (* Types (\tau) *)
  type typ
  val Arr      : bty -> rnk -> typ         (* Array type [k]r *)
  val Vcc      : bty -> rnk -> typ         (* Vector type <k>r *)
  val S        : bty -> rnk -> typ         (* singleton *)
  val SV       : bty -> rnk -> typ         (* singleton vector *)
  val Fun      : typ * typ -> typ
  val Tup      : typ list -> typ
  val prj      : int -> typ -> typ         (* takes the projection type as argument and returns the type of the tuple as a type variable *) 
                                 
  (* Type abbreviations *)
  val Int      : typ
  val Bool     : typ
  val Double   : typ
  val Complex  : typ
  val Char     : typ
  val Scl      : bty -> typ                (* [bty]0 *)
  val VecB     : bty -> typ                (* [bty]1 *)
  val Vec      : typ -> typ                (* asserts argument is scalar *)

  (* Type deconstructors *)
  val unArr    : typ -> (bty * rnk) option
  val unVcc    : typ -> (bty * rnk) option
  val unS      : typ -> (bty * rnk) option
  val unSV     : typ -> (bty * rnk) option
  val unFun    : typ -> (typ * typ) option
  val unArr'   : typ -> (bty * rnk) option    (* also returns values for Vcc,S,SV *)
  val unTup    : typ -> typ list option
                                    
  val TyVar    : unit -> typ  (* create type variable *)
  val subtype  : typ -> typ -> unify_result (* is subtype *)
  val unify    : typ -> typ -> unify_result (* unify two type variables *)
  val join     : typ -> typ -> typ  (* least common supertype; may raise Fail *)
  val prType   : typ -> string
end
