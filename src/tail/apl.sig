(** APL style operations on multi-dimensional arrays *)

(* Scalar-extension and identity items are assumed already to be
 * resolved. *)

signature APL = sig
  type 'a APLArray
  val scl       : 'a -> 'a -> 'a APLArray     (* scl default value *)
  val unScl     : string -> 'a APLArray -> 'a
  val vec       : 'a -> 'a list -> 'a APLArray
  val zilde     : 'a -> 'a APLArray
  val liftU     : 'b -> ('a -> 'b) -> 'a APLArray -> 'b APLArray
  val liftB     : 'c -> ('a * 'b -> 'c) -> 'a APLArray * 'b APLArray -> 'c APLArray
  val map       : 'b -> ('a -> 'b) -> 'a APLArray -> 'b APLArray
                                                       
  val shape     : 'a APLArray -> Int32.int APLArray
  val reshape   : Int32.int APLArray * 'a APLArray -> 'a APLArray
  val ravel     : 'a APLArray -> 'a APLArray
  val iota      : Int32.int APLArray -> Int32.int APLArray
  val each      : 'b -> ('a APLArray -> 'b APLArray) -> 'a APLArray -> 'b APLArray
  val power     : ('a APLArray -> 'a APLArray) -> Int32.int APLArray -> 'a APLArray -> 'a APLArray
  val reduce    : ('a APLArray * 'a APLArray -> 'a APLArray) -> 'a APLArray -> 'a APLArray -> 'a APLArray
  val scan      : ('a APLArray * 'a APLArray -> 'a APLArray) -> 'a APLArray -> 'a APLArray
  val gradeUp   : ('a * 'a -> bool) -> 'a APLArray -> Int32.int APLArray
  val gradeDown : ('a * 'a -> bool) -> 'a APLArray -> Int32.int APLArray
  val catenate  : 'a APLArray * 'a APLArray -> 'a APLArray
  val cons      : 'a APLArray * 'a APLArray -> 'a APLArray
  val snoc      : 'a APLArray * 'a APLArray -> 'a APLArray
(*
  val dot       : ('c APLArray * 'c APLArray -> 'c APLArray) -> ('a APLArray * 'b APLArray -> 'c APLArray)
                  -> 'c APLArray -> 'a APLArray -> 'b APLArray -> 'c APLArray
*)
  val zipWith   : 'c -> ('a APLArray * 'b APLArray -> 'c APLArray) -> 'a APLArray -> 'b APLArray -> 'c APLArray
  val transpose : 'a APLArray -> 'a APLArray
  val transpose2: Int32.int APLArray * 'a APLArray -> 'a APLArray
  val vreverse  : 'a APLArray -> 'a APLArray
  val vrotate   : Int32.int APLArray * 'a APLArray -> 'a APLArray 
  val reverse   : 'a APLArray -> 'a APLArray
  val rotate    : Int32.int APLArray * 'a APLArray -> 'a APLArray
  val drop      : Int32.int APLArray * 'a APLArray -> 'a APLArray
  val take      : Int32.int APLArray * 'a APLArray -> 'a APLArray
  val first     : 'a APLArray -> 'a APLArray
  val iff       : bool APLArray * (unit -> 'a APLArray) * (unit -> 'a APLArray) -> 'a APLArray

  val compress  : bool APLArray * 'a APLArray -> 'a APLArray
  val replicate : Int32.int APLArray * 'a APLArray -> 'a APLArray
  val idxassign : Int32.int APLArray * 'a APLArray * 'a -> unit
  val idxS      : Int32.int APLArray * Int32.int APLArray * 'a APLArray -> 'a APLArray
(*  val idx       : Int32.int APLArray * Int32.int APLArray * 'a APLArray -> 'a APLArray *)

  val pr        : ('a -> string) * string -> 'a APLArray -> string
end
