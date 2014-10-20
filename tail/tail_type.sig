(** Type structure for TAIL programs. The interface is highly
    imperative as types may be unified using the operations such as
    relateR, relateR2, unifyR, unifyB, unify, and subtype.

      r ::=                          Ranks
             rv                         rank variable
             i                          immediate rank
      b ::=                          Base types
             bv                         base type variable
             int                        integer
             double                     double
             bool                       boolean
      t ::=                          Types
             tv                         type variable
             [b]r                       array type with base type b and rank r
             Sh r                       integer vector of length r (i.e., shape vector)
             Si r                       singleton integer with value r
             Vi r                       singleton one-element vector containing the value r
             t -> t                     function type
*)

signature TAIL_TYPE = sig
  (* Ranks *)
  type rnk
  val rnk       : int -> rnk
  val unRnk     : rnk -> int option
  val RnkVar    : unit -> rnk
  val RnkVarCon : (int->string option) -> rnk
  val relateR   : (int -> int) * (int -> int) -> rnk -> rnk -> string option
  val relateR2  : {f12: int*int->int,f13:int*int->int,f23:int*int->int} -> 
                  rnk -> rnk -> rnk -> string option
  val unifyR    : rnk -> rnk -> string option
  val prRnk     : rnk -> string

  (* Base types *)
  type bty
  val IntB     : bty
  val BoolB    : bty
  val DoubleB  : bty
  val isInt    : bty -> bool
  val isDouble : bty -> bool
  val isBool   : bty -> bool
  val TyVarB   : unit -> bty
  val unifyB   : bty -> bty -> string option
  val prBty    : bty -> string

  (* Types *)
  type typ
  val Int      : typ
  val Bool     : typ
  val Double   : typ
  val Fun      : typ * typ -> typ
  val Sh       : rnk -> typ                (* Integer-vectors of a certain length *)
  val Si       : rnk -> typ                (* Singleton integer *)
  val Vi       : rnk -> typ                (* Singleton one-element integer-vector *)
  val Arr      : bty -> rnk -> typ
  val Vec      : typ -> typ                (* assert argument is a scalar type *)
  val unArr    : typ -> (bty * rnk) option
  val unSh     : typ -> rnk option
  val unSi     : typ -> rnk option
  val unVi     : typ -> rnk option
  val unFun    : typ -> (typ * typ) option
  val TyVar    : unit -> typ
  val subtype  : typ -> typ -> string option
  val unify    : typ -> typ -> string option
  val prType   : typ -> string
end
