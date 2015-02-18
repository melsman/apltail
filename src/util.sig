(** Utility library for general purpose small useful function. *)

signature UTIL = sig
  (* Functional utilities *)
  val curry   : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
  val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
  val $       : ('a -> 'b) * 'a -> 'b

  (* Iteration *)
  val iter    : (int*'a -> 'a) -> 'a -> int*int -> 'a

  (* Association lists *)
  type (''a,'b) alist = (''a * 'b) list
  val emptyAlist  : unit -> (''a,'b) alist
  val extendAlist : (''a,'b) alist -> ''a * 'b -> (''a,'b) alist
  val lookupAlist : (''a,'b) alist -> ''a -> 'b option
  val plusAlist   : (''a,'b) alist * (''a,'b) alist -> (''a,'b) alist

  (* Printing *)
  val intToCString  : int -> string
  val realToCString : real -> string
  val prln          : string -> unit

  (* Minimum and maximum values *)
  val minInt : int
  val maxInt : int

  (* File manipulation *)
  val readFile : string -> string


end

(**

[curry f a b] returns f(a,b).

[uncurry f (a,b)] returns f a b.

[f $ a] returns f a. Useful when the right-hand-side is a complex
expression.

[iter f a (i,j)] returns the result of accumulating the result of
applying f over the interval [i;j]. Identical to
f(j,...f(i+1,f(i,a))...) if i <= j. Returns a if i > j.

[intToCString i] returns a C syntactic string representation of the
integer i (as a C int).

[realToCString r] returns a C syntactic string representation of the
real r (as a C double).

[readfile f] returns the content of the file s. Raises an exception if
the file does not exists or cannot be opened for reading.

*)
