signature TYPE = sig
  eqtype T                       (* Type constructors *)
  val Int      : T
  val Double   : T
  val Bool     : T
  val Vec      : T -> T
  val prType   : T -> string
  val vecElem  : T -> T    (* may fail *)
end
