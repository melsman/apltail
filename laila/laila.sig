signature LAILA = sig

  include TYPE

  type 'a M    (* monad encapsulating program construction *)
  val >>=      : 'a M * ('a -> 'b M) -> 'b M
  val ret      : 'a -> 'a M

  (* Terms *)
  type t                      (* terms *)
  type v                      (* vector terms *)

  val I        : int -> t
  val D        : real -> t
  val B        : bool -> t
  val i2d      : t -> t
  val b2i      : t -> t
  val If       : t * t * t -> t
  val fromList : T -> t list -> v
  val fromListM : T -> t list -> v M

  (* Compiled Programs *)
  type prog
  val runM     : {verbose: bool, optlevel: int} -> T -> t M -> prog
  val runF     : T * T -> (t -> t M) -> prog
  val outprog  : string -> prog -> unit
 
  (* Values and Evaluation *)
  type V
  val Iv       : int -> V
  val unIv     : V -> int     (* may fail *)
  val Dv       : real -> V
  val unDv     : V -> real    (* may fail *)
  val Bv       : bool -> V
  val unBv     : V -> bool    (* may fail *)
  val Vv       : V list -> V
  val unVv     : V -> V list  (* may fail *)
  val Uv       : V 
  val eval     : prog -> V -> V
  val pp_prog  : prog -> string
  val ppV      : V -> string

  type m (* APL multi-dimensional arrays *)

  type INT = t
  type DOUBLE = t
  type BOOL = t

  val assert   : string -> BOOL -> 'a M -> 'a M

  val addi    : INT * INT -> INT
  val subi    : INT * INT -> INT
  val muli    : INT * INT -> INT
  val divi    : INT * INT -> INT
  val resi    : INT * INT -> INT
  val maxi    : INT * INT -> INT
  val mini    : INT * INT -> INT
  val lti     : INT * INT -> BOOL
  val ltei    : INT * INT -> BOOL
  val gti     : INT * INT -> BOOL
  val gtei    : INT * INT -> BOOL
  val eqi     : INT * INT -> BOOL
  val neqi    : INT * INT -> BOOL
  val negi    : INT -> INT
  val signi   : INT -> INT

  val addd    : DOUBLE * DOUBLE -> DOUBLE
  val subd    : DOUBLE * DOUBLE -> DOUBLE
  val muld    : DOUBLE * DOUBLE -> DOUBLE
  val divd    : DOUBLE * DOUBLE -> DOUBLE
  val resd    : DOUBLE * DOUBLE -> DOUBLE
  val maxd    : DOUBLE * DOUBLE -> DOUBLE
  val mind    : DOUBLE * DOUBLE -> DOUBLE
  val ltd     : DOUBLE * DOUBLE -> BOOL
  val lted    : DOUBLE * DOUBLE -> BOOL
  val gtd     : DOUBLE * DOUBLE -> BOOL
  val gted    : DOUBLE * DOUBLE -> BOOL
  val eqd     : DOUBLE * DOUBLE -> BOOL
  val neqd    : DOUBLE * DOUBLE -> BOOL
  val negd    : DOUBLE -> DOUBLE
  val signd   : DOUBLE -> INT

  val zilde   : T -> m
  val scl     : T -> t -> m
  val vec     : v -> m
  val iota    : INT -> m
  val iota'   : m -> m

  val siz     : m -> INT
  val dim     : m -> INT

  val rav     : m -> m
  val rav0    : m -> v

(*  val index   : Int Num v -> 'a m -> 'a m M *)
  val each    : T -> (t -> t M) -> m -> m

  val meq     : T -> (t * t -> BOOL) -> m -> m -> BOOL M  

  val mif     : BOOL * m * m -> m
  val lett    : T -> t -> t M
  val letm    : m -> m M

  val zipWith : T -> (t * t -> t M) -> m -> m -> m M

  val scan    : T -> T -> (t * t -> t) -> t -> m -> m M

  val catenate : m -> m -> m M

  val take    : INT -> m -> m
  val drop    : INT -> m -> m

  val first   : m -> t

  val mem     : m -> m M

  val rotate  : INT -> m -> m
  val reverse : m -> m
  val reshape : v -> m -> m M
  val shape   : m -> v

  val reduce  : (t * t -> t M) -> t -> m -> (t -> 'b) -> (m -> 'b) -> 'b M

  val transpose : m -> m M
  val transpose2 : int list -> m -> m M

  val compress  : m * m -> m M

end
