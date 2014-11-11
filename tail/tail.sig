(** This signature specifies operations for constructing,
    pretty-printing, and evaluating TAIL programs. TAIL is an acronym
    for "Typed Array Intermediate Language".
*)

signature TAIL = sig

  (* Types *)
  eqtype Int and Double and 'a Num  (* numeric types *)
     and Bool                       (* booleans *)
     and 'a Vec                     (* vectors *)

  eqtype 'a T                       (* Type constructors *)
  val Int       : Int Num T
  val Double    : Double Num T
  val Bool      : Bool T
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
  type 'a t                    (* terms *)
  type 'a v     = 'a Vec t        (* vector terms *)

  type 'a NUM   = 'a Num t        (* basic term types *)
  type INT      = Int NUM
  type DOUBLE   = Double NUM
  type BOOL     = Bool t

  val I         : int -> INT
  val D         : real -> DOUBLE
  val B         : bool -> BOOL
  val %         : INT * INT -> INT
  val i2d       : INT -> DOUBLE
  val b2i       : BOOL -> INT
  val If        : BOOL * 'a t * 'a t -> 'a t
  val fromList  : 'a T -> 'a t list -> 'a v

  (* Compiled Programs *)
  type ('a,'b) prog
  val runM      : {verbose: bool, optlevel: int, prtype: bool} 
                  -> 'b T -> 'b t M -> (unit,'b) prog
  val runF      : 'a T * 'b T -> ('a t -> 'b t M) -> ('a,'b) prog
  val outprog   : bool -> string -> ('a,'b)prog -> unit
  val runHack   : 'a M -> 'a option 

  (* Values and Evaluation *)
  type 'a V
  val Iv        : int -> Int Num V
  val unIv      : Int Num V -> int
  val Dv        : real -> Double Num V
  val unDv      : Double Num V -> real
  val Bv        : bool -> Bool V
  val unBv      : Bool V -> bool
  val Vv        : 'a V list -> 'a Vec V
  val unVv      : 'a Vec V -> 'a V list
  val Uv        : unit V 
  val eval      : ('a,'b) prog -> 'a V -> 'b V
  val pp_prog   : bool -> ('a,'b) prog -> string
  val ppV       : 'a V -> string

  type 'a m (* APL multi-dimensional arrays *)

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
  val addd      : DOUBLE * DOUBLE -> DOUBLE
  val subd      : DOUBLE * DOUBLE -> DOUBLE
  val muld      : DOUBLE * DOUBLE -> DOUBLE
  val divd      : DOUBLE * DOUBLE -> DOUBLE
  val resd      : DOUBLE * DOUBLE -> DOUBLE
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

  val andb      : BOOL * BOOL -> BOOL
  val orb       : BOOL * BOOL -> BOOL
  val xorb      : BOOL * BOOL -> BOOL
  val eqb       : BOOL * BOOL -> BOOL
  val nandb     : BOOL * BOOL -> BOOL
  val norb      : BOOL * BOOL -> BOOL
  val notb      : BOOL -> BOOL

  val zilde     : unit -> 'a m
  val scl       : 'a t -> 'a m   (* identity! *)
  val scalar    : 'a t -> 'a m
  val vec       : 'a v -> 'a m
  val iota      : INT -> Int Num m
  val iota'     : Int Num m -> Int Num m

  val siz       : 'a m -> INT
  val dim       : 'a m -> INT

  val rav       : 'a m -> 'a m
  val rav0      : 'a m -> 'a v

(*  val index   : Int Num v -> 'a m -> 'a m M *)
  val each      : ('a t -> 'b t M) -> 'a m -> 'b m

  val red       : ('a t * 'b t -> 'b t) -> 'b t -> 'a m -> 'b t

  val mif       : BOOL * 'a m * 'a m -> 'a m
  val lett      : 'a t -> 'a t M
  val letm      : 'a m -> 'a m M

  val zipWith   : ('a t * 'b t -> 'c t M) -> 'a m -> 'b m -> 'c m

  val scan      : ('a t * 'b t -> 'a t) -> 'a t -> 'b m -> 'a m

  val catenate  : 'a m -> 'a m -> 'a m
  val catenate_first : 'a m -> 'a m -> 'a m

  val take      : INT -> 'a m -> 'a m
  val drop      : INT -> 'a m -> 'a m

  val first     : 'a m -> 'a t

  val mem       : 'a m -> 'a m

  val rotate    : INT -> 'a m -> 'a m
  val reverse   : 'a m -> 'a m
  val vreverse  : 'a m -> 'a m 
  val vrotate   : INT -> 'a m -> 'a m
  val reshape   : Int Num v -> 'a m -> 'a m
  val shape     : 'a m -> Int Num v

  val prod      : ('a t * 'a t -> 'a t M) -> ('a t * 'a t -> 'a t M) -> 'a t
                  -> 'a m -> 'a m -> ('a t -> 'b) -> ('a m -> 'b) -> 'b

  val reduce    : ('a t * 'a t -> 'a t M) -> 'a t -> 'a m -> ('a t -> 'b) -> ('a m -> 'b) -> 'b

  val compress  : Bool m -> 'a m -> 'a m
  val replicate : 'a t -> Int Num m -> 'a m -> 'a m

  val pow       : ('a m -> 'a m M) -> INT -> 'a m -> 'a m

  val transpose : 'a m -> 'a m
  val transpose2 : Int Num v -> 'a m -> 'a m

  (* Printing routines *)
  val prArrI    : Int Num m -> Int Num m
  val prArrB    : Bool m -> Bool m
  val prArrD    : Double Num m -> Double Num m
  val prSclI    : Int Num t -> Int Num t
  val prSclB    : Bool t -> Bool t
  val prSclD    : Double Num t -> Double Num t
end
