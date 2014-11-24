(** Type structure for TAIL programs. The interface is highly
    imperative as types may be unified using the operations such as
    relateR, relateR2, unifyR, unifyB, unify, and subtype.

      r ::=                          Ranks
             rv                         rank variable
             i                          immediate rank
      k ::=                          Base types
             bv                         base type variable
             int                        integer
             double                     double
             bool                       boolean
      t ::=                          Types
             tv                         type variable
             [k]r                       array type with base type b and rank r
             <k>r                       vector type with base type b and length r (i.e., shape vector)
             S_k(r)                     singleton type of base type b and value r
             SV_k(r)                    singleton one-element vector type containing the value r of type b
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
  val CharB    : bty
  val isInt    : bty -> bool
  val isDouble : bty -> bool
  val isBool   : bty -> bool
  val isChar   : bty -> bool
  val TyVarB   : unit -> bty
  val unifyB   : bty -> bty -> string option
  val prBty    : bty -> string

  (* Types *)
  type typ
  val Arr      : bty -> rnk -> typ         (* Array type [k]r *)
  val Vcc      : bty -> rnk -> typ         (* Vector type <k>r *)
  val S        : bty -> rnk -> typ         (* singleton *)
  val SV       : bty -> rnk -> typ         (* singleton vector *)
  val Fun      : typ * typ -> typ

  (* Type abbreviations *)
  val Int      : typ
  val Bool     : typ
  val Double   : typ
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

  val TyVar    : unit -> typ
  val subtype  : typ -> typ -> string option
  val unify    : typ -> typ -> string option
  val join     : typ -> typ -> typ  (* least common supertype; may raise Fail *)
  val prType   : typ -> string
end
