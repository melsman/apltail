(** This signature specifies operations for constructing,
    pretty-printing, and evaluating TAIL programs. TAIL is an acronym
    for "Typed Array Intermediate Language".
*)

signature TAIL = sig

  val optimisationLevel : int ref

  structure Exp : TAIL_EXP

  (* Types (phantom types) *)
  eqtype Int and Double and Complex and 'a Num  (* numeric types *)
     and Bool                                   (* booleans *)
     and Char                                   (* characters *)
     and 'a Vec                                 (* vectors *)

  (* TAIL types (= TAIL_TYPE.typ) *)
  type 'a T                          (* Type constructors *)
  val Int       : Int Num T
  val Double    : Double Num T
  val Complex   : Complex Num T
  val Bool      : Bool T
  val Char      : Char T
  val Vec       : 'a T -> 'a Vec T
  val prType    : 'a T -> string

  (* Monadic encapsulation of program construction. Allows for
     introducing target-language let-constructs using the lett and
     letm combinators below.
   *)
  type 'a M
  val >>=       : 'a M * ('a -> 'b M) -> 'b M
  val ret       : 'a -> 'a M

  (* Terms *)
  type 'a exp
  type 'a tvector = 'a Vec exp        (* vector terms *)
  val typeOf : 'a exp -> Exp.typ

  (* TAIL terms w. added phantom types *)
  type 'a NUM   = 'a Num exp        (* basic term types *)
  type INT      = Int NUM
  type DOUBLE   = Double NUM
  type COMPLEX  = Complex NUM
  type BOOL     = Bool exp
  type CHAR     = Char exp

  val I         : Int32.int -> INT
  val D         : real -> DOUBLE
  val X         : real*real -> COMPLEX
  val B         : bool -> BOOL
  val C         : word -> CHAR
  val %         : INT * INT -> INT
  val d2x       : DOUBLE -> COMPLEX
  val i2d       : INT -> DOUBLE
  val b2i       : BOOL -> INT
  val If        : BOOL * 'a exp * 'a exp -> 'a exp
  val fromList  : 'a T -> 'a exp list -> 'a tvector
  val fromChars : word list -> Char tvector

  (* Compiled Programs
        'a - the input type of the expression (often just "unit")
        'b - the return type of the expression *)
  type ('a,'b) prog

  (* [runM flags t0 c] Runs the monadic expression c, type checks it
       and optimizes it.

       Prints type errors and intermediate representations if the
       'verbose' flag (-v) is set.

       The expression is expected to return a value of type 't0'.
   *)
  val runM      : {verbose: bool, optlevel: int, prtype: bool, materialize: bool}
                  -> 'b T -> 'b exp M -> (unit,'b) prog

  (* Run the monadic expression, the function will be given a variable
     where to store the final result *)
  val runF      : 'a T * 'b T -> ('a exp -> 'b exp M) -> ('a,'b) prog

  (* [outprog prtype filename c]
       'prtype' indicates whether the output should include instance lists. *)
  val outprog   : bool -> string -> ('a,'b) prog -> unit
  val runHack   : 'a M -> 'a option
  val toExp     : (unit,'b) prog -> 'b exp

  (* Values and Evaluation *)
  type 'a value
  val Iv        : Int32.int -> Int Num value
  val unIv      : Int Num value -> Int32.int
  val Dv        : real -> Double Num value
  val unDv      : Double Num value -> real
  val Xv        : real*real -> Complex Num value
  val unXv      : Complex Num value -> real*real
  val Bv        : bool -> Bool value
  val unBv      : Bool value -> bool
  val Vv        : 'a value list -> 'a Vec value
  val unVv      : 'a Vec value -> 'a value list
  val Uv        : unit value

  (* Evaluate TAIL expression to a value *)
  val eval      : ('a,'b) prog -> 'a value -> 'b value

  (* Pretty printing *)
  val pp_prog   : bool -> ('a,'b) prog -> string
  val pp_exp    : bool -> 'a exp -> string
  val ppValue   : 'a value -> string

  type 'a ndarray (* APL multi-dimensional arrays *)

  (* TAIL built-in operations *)
  val ceil      : DOUBLE -> INT
  val floor     : DOUBLE -> INT
  val ln        : DOUBLE -> DOUBLE
  val cos       : DOUBLE -> DOUBLE
  val sin       : DOUBLE -> DOUBLE
  val tan       : DOUBLE -> DOUBLE
  val acos      : DOUBLE -> DOUBLE
  val asin      : DOUBLE -> DOUBLE
  val atan      : DOUBLE -> DOUBLE
  val cosh      : DOUBLE -> DOUBLE
  val sinh      : DOUBLE -> DOUBLE
  val tanh      : DOUBLE -> DOUBLE
  val pi        : unit   -> DOUBLE
  val addi      : INT * INT -> INT
  val subi      : INT * INT -> INT
  val muli      : INT * INT -> INT
  val divi      : INT * INT -> INT
  val resi      : INT * INT -> INT
  val lti       : INT * INT -> BOOL
  val ltei      : INT * INT -> BOOL
  val gti       : INT * INT -> BOOL
  val gtei      : INT * INT -> BOOL
  val eqi       : INT * INT -> BOOL
  val neqi      : INT * INT -> BOOL
  val maxi      : INT -> INT -> INT
  val mini      : INT -> INT -> INT
  val negi      : INT -> INT
  val absi      : INT -> INT
  val signi     : INT -> INT
  val addd      : DOUBLE * DOUBLE -> DOUBLE
  val subd      : DOUBLE * DOUBLE -> DOUBLE
  val muld      : DOUBLE * DOUBLE -> DOUBLE
  val divd      : DOUBLE * DOUBLE -> DOUBLE
  val resd      : DOUBLE * DOUBLE -> DOUBLE
  val powd      : DOUBLE * DOUBLE -> DOUBLE
  val ltd       : DOUBLE * DOUBLE -> BOOL
  val lted      : DOUBLE * DOUBLE -> BOOL
  val gtd       : DOUBLE * DOUBLE -> BOOL
  val gted      : DOUBLE * DOUBLE -> BOOL
  val eqd       : DOUBLE * DOUBLE -> BOOL
  val neqd      : DOUBLE * DOUBLE -> BOOL
  val maxd      : DOUBLE -> DOUBLE -> DOUBLE
  val mind      : DOUBLE -> DOUBLE -> DOUBLE
  val negd      : DOUBLE -> DOUBLE
  val absd      : DOUBLE -> DOUBLE
  val expd      : DOUBLE -> DOUBLE
  val signd     : DOUBLE -> INT

  val andb      : BOOL * BOOL -> BOOL
  val orb       : BOOL * BOOL -> BOOL
  val xorb      : BOOL * BOOL -> BOOL
  val eqb       : BOOL * BOOL -> BOOL
  val nandb     : BOOL * BOOL -> BOOL
  val norb      : BOOL * BOOL -> BOOL
  val notb      : BOOL -> BOOL

  val addx      : COMPLEX * COMPLEX -> COMPLEX
  val subx      : COMPLEX * COMPLEX -> COMPLEX
  val mulx      : COMPLEX * COMPLEX -> COMPLEX
  val rex       : COMPLEX -> DOUBLE
  val imx       : COMPLEX -> DOUBLE
  val injx      : DOUBLE * DOUBLE -> COMPLEX
  val expx      : COMPLEX -> COMPLEX

  val roll      : INT -> DOUBLE

  val ltc       : CHAR * CHAR -> BOOL
  val ltec      : CHAR * CHAR -> BOOL
  val gtc       : CHAR * CHAR -> BOOL
  val gtec      : CHAR * CHAR -> BOOL
  val eqc       : CHAR * CHAR -> BOOL
  val neqc      : CHAR * CHAR -> BOOL

  val zilde     : unit -> 'a ndarray
  val scl       : 'a exp -> 'a ndarray   (* identity! *)
  val scalar    : 'a exp -> 'a ndarray
  val vec       : 'a tvector -> 'a ndarray
  val iota      : INT -> Int Num ndarray
  val iota'     : Int Num ndarray -> Int Num ndarray

  val siz       : 'a ndarray -> INT
  val dim       : 'a ndarray -> INT

  (* Ravel *)
  val rav       : 'a ndarray -> 'a ndarray
  val rav0      : 'a ndarray -> 'a tvector

(*  val index   : Int Num tvector -> 'a m -> 'a m M *)
  val each      : ('a exp -> 'b exp M) -> 'a ndarray -> 'b ndarray

  val red       : ('a exp * 'b exp -> 'b exp) -> 'b exp -> 'a ndarray -> 'b exp

  val mif       : BOOL * 'a ndarray * 'a ndarray -> 'a ndarray
  val lett      : 'a exp -> 'a exp M
  val letm      : 'a ndarray -> 'a ndarray M

  val zipWith   : ('a exp * 'b exp -> 'c exp M) -> 'a ndarray -> 'b ndarray -> 'c ndarray

  val scan      : ('a exp * 'b exp -> 'a exp M) -> 'a exp -> 'b ndarray -> 'a ndarray

  val catenate  : 'a ndarray -> 'a ndarray -> 'a ndarray
  val catenate_first : 'a ndarray -> 'a ndarray -> 'a ndarray

  val take      : INT -> 'a ndarray -> 'a ndarray
  val drop      : INT -> 'a ndarray -> 'a ndarray

  val first     : 'a ndarray -> 'a exp

  (* Memoize *)
  val mem       : 'a ndarray -> 'a ndarray
  val memScl    : 'a exp -> 'a exp

  val rotate    : INT -> 'a ndarray -> 'a ndarray
  val reverse   : 'a ndarray -> 'a ndarray
  val vreverse  : 'a ndarray -> 'a ndarray
  val vrotate   : INT -> 'a ndarray -> 'a ndarray
  val reshape   : Int Num tvector -> 'a ndarray -> 'a ndarray
  val shape     : 'a ndarray -> Int Num tvector
  val gradeUp   : 'a ndarray -> Int Num ndarray
  val gradeDown : 'a ndarray -> Int Num ndarray

  val reduce    : ('a exp * 'a exp -> 'a exp M) -> 'a exp -> 'a ndarray -> ('a exp -> 'b) -> ('a ndarray -> 'b) -> 'b

  val compress  : Bool ndarray -> 'a ndarray -> 'a ndarray
  val replicate : 'a exp -> Int Num ndarray -> 'a ndarray -> 'a ndarray

  val power     : ('a ndarray -> 'a ndarray M) -> INT -> 'a ndarray -> 'a ndarray
  val powerScl  : ('a exp -> 'a exp M) -> INT -> 'a exp -> 'a exp
  val condScl   : ('a exp -> 'a exp M) -> BOOL -> 'a exp -> 'a exp

  type 'a tuple
  type nil
  type ('a,'e) tupleIdx
  val powerN    : ('a tuple -> 'a tuple M) -> INT -> 'a tuple -> 'a tuple
  val empTuple  : nil tuple
  val consTuple : 'a ndarray -> 'b tuple -> ('a->'b)tuple
  val Zero      : unit -> ('a->'b,'a)tupleIdx
  val Succ      : ('a,'e)tupleIdx -> ('a->'b,'e)tupleIdx            (* Succ(Zero()) : ('a->'b->'c,'b)tupleIdx *)
  val prjTuple  : ('a,'e)tupleIdx -> 'a tuple -> 'e ndarray

  structure Unsafe : sig
    type utuple
    type uexp
    val letUtuple    : utuple -> utuple M
    val upowerN      : (utuple -> utuple M) -> INT -> utuple -> utuple
    val empUtuple    : utuple
    val consUtuple   : uexp -> utuple -> utuple
    val prjUtuple    : int -> utuple -> uexp   (* zero-indexed *)
    val toUexp       : 'a exp -> uexp
    val toUexpA      : 'a ndarray -> uexp
    val fromUexp     : uexp -> 'a exp
    val fromUexpA    : uexp -> 'a ndarray
  end

  val transpose : 'a ndarray -> 'a ndarray
  val transpose2 : Int Num tvector -> 'a ndarray -> 'a ndarray

  val idxS      : Int32.int -> INT -> 'a ndarray -> ('a exp -> 'b) -> ('a ndarray -> 'b) -> 'b
  val idx       : Int32.int -> Int Num ndarray -> 'a ndarray -> 'a ndarray

  val idxassign : Int Num ndarray -> 'a ndarray -> 'a exp -> BOOL

  (* Printing routines *)
  val prArrI    : Int Num ndarray -> Int Num ndarray
  val prArrB    : Bool ndarray -> Bool ndarray
  val prArrD    : Double Num ndarray -> Double Num ndarray
  val prArrX    : Complex Num ndarray -> Complex Num ndarray
  val prArrC    : Char ndarray -> Char ndarray
  val prSclI    : INT -> INT
  val prSclB    : BOOL -> BOOL
  val prSclD    : DOUBLE -> DOUBLE
  val prSclX    : COMPLEX -> COMPLEX
  val prSclC    : CHAR -> CHAR
  val formatI   : INT -> Char ndarray
  val formatD   : DOUBLE -> Char ndarray
  val formatX   : COMPLEX -> Char ndarray

  (* File access *)
  val readFile  : Char ndarray -> Char ndarray
  val readIntVecFile : Char ndarray -> Int Num ndarray
  val readDoubleVecFile : Char ndarray -> Double Num ndarray

  (* Int32 binary operations *)
  val andi      : INT * INT -> INT
  val ori       : INT * INT -> INT
  val shri      : INT * INT -> INT
  val shari     : INT * INT -> INT
  val shli      : INT * INT -> INT
  val xori      : INT * INT -> INT
  val noti      : INT -> INT

  (* Time.now *)
  val nowi      : INT -> INT   (* nowi 0 returns time in milliseconds since process start *)

  val bench     : ('a exp -> 'a exp M) -> INT -> 'a exp -> 'a exp   (* as powerScl, but for benchmarking *)

end
