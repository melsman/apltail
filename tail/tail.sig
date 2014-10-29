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
  val Int      : Int Num T
  val Double   : Double Num T
  val Bool     : Bool T
  val Vec      : 'a T -> 'a Vec T
  val prType   : 'a T -> string

  (* Monadic encapsulation of program construction *)
  type 'a M    
  val >>=      : 'a M * ('a -> 'b M) -> 'b M
  val ret      : 'a -> 'a M

  (* Terms *)
  type 'a t                      (* terms *)
  type 'a v    = 'a Vec t        (* vector terms *)

  type 'a NUM  = 'a Num t        (* basic term types *)
  type INT     = Int NUM
  type DOUBLE  = Double NUM
  type BOOL    = Bool t

  val I        : int -> INT
  val D        : real -> DOUBLE
  val B        : bool -> BOOL
  val %        : INT * INT -> INT
  val i2d      : INT -> DOUBLE
  val b2i      : BOOL -> INT
  val If       : BOOL * 'a t * 'a t -> 'a t
  val fromList : 'a T -> 'a t list -> 'a v
  val fromListM : 'a T -> 'a t list -> 'a v M

  (* Compiled Programs *)
  type ('a,'b) prog
  val runM     : {verbose: bool, optlevel: int, prtype: bool} 
                 -> 'b T -> 'b t M -> (unit,'b) prog
  val runF     : 'a T * 'b T -> ('a t -> 'b t M) -> ('a,'b) prog
  val outprog  : bool -> string -> ('a,'b)prog -> unit
  val runHack  : 'a M -> 'a option 


  (* Values and Evaluation *)
  type 'a V
  val Iv       : int -> Int Num V
  val unIv     : Int Num V -> int
  val Dv       : real -> Double Num V
  val unDv     : Double Num V -> real
  val Bv       : bool -> Bool V
  val unBv     : Bool V -> bool
  val Vv       : 'a V list -> 'a Vec V
  val unVv     : 'a Vec V -> 'a V list
  val Uv       : unit V 
  val eval     : ('a,'b) prog -> 'a V -> 'b V
  val pp_prog  : bool -> ('a,'b) prog -> string
  val ppV      : 'a V -> string

  type 'a m (* APL multi-dimensional arrays *)

  val addi    : INT * INT -> INT
  val subi    : INT * INT -> INT
  val muli    : INT * INT -> INT
  val divi    : INT * INT -> INT
  val resi    : INT * INT -> INT
  val lti     : INT * INT -> BOOL
  val leqi    : INT * INT -> BOOL
  val eqi     : INT * INT -> BOOL
  val maxi    : INT -> INT -> INT
  val mini    : INT -> INT -> INT
  val negi    : INT -> INT
  val absi    : INT -> INT
  val addd    : DOUBLE * DOUBLE -> DOUBLE
  val subd    : DOUBLE * DOUBLE -> DOUBLE
  val muld    : DOUBLE * DOUBLE -> DOUBLE
  val divd    : DOUBLE * DOUBLE -> DOUBLE
  val resd    : DOUBLE * DOUBLE -> DOUBLE
  val ltd     : DOUBLE * DOUBLE -> BOOL
  val leqd    : DOUBLE * DOUBLE -> BOOL
  val eqd     : DOUBLE * DOUBLE -> BOOL
  val maxd    : DOUBLE -> DOUBLE -> DOUBLE
  val mind    : DOUBLE -> DOUBLE -> DOUBLE
  val negd    : DOUBLE -> DOUBLE
  val absd    : DOUBLE -> DOUBLE

  val zilde   : 'a T -> 'a m
  val scl     : 'a T -> 'a t -> 'a m
  val scalar  : 'a t -> 'a m
  val vec     : 'a v -> 'a m
  val iota    : INT -> Int Num m
  val iota'   : Int Num m -> Int Num m

  val siz     : 'a m -> INT
  val dim     : 'a m -> INT

  val rav     : 'a m -> 'a m
  val rav0    : 'a m -> 'a v

(*  val index   : Int Num v -> 'a m -> 'a m M *)
  val each    : 'a T -> 'b T -> ('a t -> 'b t M) -> 'a m -> 'b m

  val red     : 'a T -> 'b T -> ('a t * 'b t -> 'b t M) -> 'b t -> 'a m -> 'b t M

  val meq     : 'a T -> ('a t * 'a t -> BOOL) -> 'a m -> 'a m -> BOOL M  

  val mif     : BOOL * 'a m * 'a m -> 'a m
  val lett    : 'a T -> 'a t -> 'a t M
  val letm    : 'a T -> 'a m -> 'a m M
  val letm_asgn : 'a T -> 'a m -> 'a m M

  val zipWith : 'a T -> 'b T -> 'c T -> ('a t * 'b t -> 'c t M) -> 'a m -> 'b m -> 'c m M

  val scan    : 'a T -> 'b T -> ('a t * 'b t -> 'a t) -> 'a t -> 'b m -> 'a m M

  val catenate : 'a m -> 'a m -> 'a m M
  val catenate_first : 'a m -> 'a m -> 'a m M

  val take    : INT -> 'a m -> 'a m
  val drop    : INT -> 'a m -> 'a m

  val first   : 'a m -> 'a t

  val mem     : 'a m -> 'a m M

  val rotate  : INT -> 'a m -> 'a m
  val reverse : 'a m -> 'a m
  val reshape : Int Num v -> 'a m -> 'a m M
  val shape   : 'a m -> Int Num v

  val prod    : 'a T -> ('a t * 'a t -> 'a t M) -> ('a t * 'a t -> 'a t M) -> 'a t
                -> 'a m -> 'a m -> ('a t -> 'b) -> ('a m -> 'b) -> 'b M 

  val outer   : 'a T -> 'b T -> ('a t * 'b t -> 'c t M) -> 'a m -> 'b m -> 'c m M

  val reduce  : 'a T -> ('a t * 'a t -> 'a t M) -> 'a t -> 'a m -> ('a t -> 'b) -> ('a m -> 'b) -> 'b M

  val compress  : Bool m -> 'a m -> 'a m
  val replicate : 'a t -> Int Num m -> 'a m -> 'a m

  val transpose : 'a m -> 'a m
  val transpose2 : Int Num v -> 'a m -> 'a m
end
