signature LAILA = sig

  include TYPE

  type 'a M    (* monad encapsulating program construction *)
  val >>=      : 'a M * ('a -> 'b M) -> 'b M
  val ret      : 'a -> 'a M

  (* Terms *)
  type t                      (* terms *)

  val I        : Int32.int -> t
  val D        : real -> t
  val B        : bool -> t
  val C        : word -> t
  val i2d      : t -> t
  val b2i      : t -> t
  val If       : t * t * t -> t

  (* Compiled Programs *)
  type prog
  val runM     : {verbose: bool, optlevel: int} -> T -> t M -> prog
  val runF     : T * T -> (t -> t M) -> prog
  val outprog  : string -> prog -> unit
 
  (* Values and Evaluation *)
  type value
  val Iv       : int -> value
  val unIv     : value -> int     (* may fail *)
  val Dv       : real -> value
  val unDv     : value -> real    (* may fail *)
  val Bv       : bool -> value
  val unBv     : value -> bool    (* may fail *)
  val Vv       : value list -> value
  val unVv     : value -> value list  (* may fail *)
  val Uv       : value
  val eval     : prog -> value -> value
  val pp_prog  : prog -> string
  val ppV      : value -> string

  type m (* APL multi-dimensional arrays *)

  type INT = t
  type DOUBLE = t
  type BOOL = t
  type CHAR = t

  val fromListM : T -> t list -> m M

  val assert    : string -> BOOL -> 'a M -> 'a M

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
  val absi    : INT -> INT

  val ori     : INT * INT -> INT
  val andi    : INT * INT -> INT
  val xori    : INT * INT -> INT
  val shli    : INT * INT -> INT
  val shri    : INT * INT -> INT
  val shari   : INT * INT -> INT

  val addd    : DOUBLE * DOUBLE -> DOUBLE
  val subd    : DOUBLE * DOUBLE -> DOUBLE
  val muld    : DOUBLE * DOUBLE -> DOUBLE
  val divd    : DOUBLE * DOUBLE -> DOUBLE
  val resd    : DOUBLE * DOUBLE -> DOUBLE
  val maxd    : DOUBLE * DOUBLE -> DOUBLE
  val mind    : DOUBLE * DOUBLE -> DOUBLE
  val powd    : DOUBLE * DOUBLE -> DOUBLE
  val ltd     : DOUBLE * DOUBLE -> BOOL
  val lted    : DOUBLE * DOUBLE -> BOOL
  val gtd     : DOUBLE * DOUBLE -> BOOL
  val gted    : DOUBLE * DOUBLE -> BOOL
  val eqd     : DOUBLE * DOUBLE -> BOOL
  val neqd    : DOUBLE * DOUBLE -> BOOL
  val negd    : DOUBLE -> DOUBLE
  val absd    : DOUBLE -> DOUBLE
  val ln      : DOUBLE -> DOUBLE
  val sin     : DOUBLE -> DOUBLE
  val cos     : DOUBLE -> DOUBLE
  val tan     : DOUBLE -> DOUBLE
  val signd   : DOUBLE -> INT
  val floor   : DOUBLE -> INT
  val ceil    : DOUBLE -> INT
  val pi      : DOUBLE
  val roll    : INT -> DOUBLE   (* 0 -> [0;1[ ; n -> [0;n] whole number *)

  val eqc     : CHAR * CHAR -> BOOL

  val eqb     : BOOL * BOOL -> BOOL
  val andb    : BOOL * BOOL -> BOOL
  val orb     : BOOL * BOOL -> BOOL
  val xorb    : BOOL * BOOL -> BOOL
  val notb    : BOOL -> BOOL

  val zilde   : T -> m
  val scl     : T -> t -> m  (* scalar value *) 
  val enclose : t -> m       (* one dimensional vector with one element *)
  val iota    : INT -> m
  val iota'   : m -> m

  val siz     : m -> INT
  val rank    : m -> INT

  val rav     : m -> m

  val dimincr : m -> m (* shape(dimincr(m)) = shape(m)@[1] *)

  val each    : T -> (t -> t M) -> m -> m

  val lett    : t -> t M
  val letm    : m -> m M

  val zipWith : T -> (t * t -> t M) -> m -> m -> m M

  val scan    : T -> T -> (t * t -> t) -> t -> m -> m M

  val catenate   : m -> m -> m M

  val take       : INT -> m -> m M
  val drop       : INT -> m -> m M

  val first      : m -> t M

  val mem        : m -> m M

  val rotate     : INT -> m -> m  (* ok for vectors *)

  val vreverse   : m -> m M
  val vrotate    : INT -> m -> m M

  val reshape    : m -> m -> m M
  val shape      : m -> m

  val reduce     : (t * t -> t M) -> t -> m -> (t -> 'b) -> (m -> 'b) -> 'b M

  val transpose  : m -> m M
  val transpose2 : int list -> m -> m M

  val compress   : m * m -> m M
  val replicate  : t * m * m -> m M

  val power      : (m -> m M) -> INT -> m -> m M
  val powerScl   : (t -> t M) -> INT -> t -> t M

  (* Indexing *)
  val idxS       : INT -> INT -> m -> (t -> 'b) -> (m -> 'b) -> 'b M

  (* Printing routines *)
  val prArr      : m -> unit M
  val printf     : string * t list -> unit M

  (* Multi-dimensional mutable arrays - used for idxassign *)
  type mm
  val mk_mm      : m -> mm M
  val idxassign  : m -> mm -> t -> unit M
  val mm2m       : mm -> m
end
