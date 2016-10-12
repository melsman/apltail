functor Apl2Tail (T : TAIL) : APL2TAIL =
struct

(* Export TAIL-structure *)
structure T = T

(* Compiler flags relevant for translation to TAIL *)
type flags = {verbose : bool, optlevel : int, prtype : bool, materialize : bool}

local
  open T

  datatype 'a identity_item = Lii of 'a
                            | Rii of 'a
                            | LRii of 'a
                            | NOii

  type id_item = Int32.int identity_item * real identity_item * bool identity_item * (real*real) identity_item
                 
  fun id_item ii v =
      case ii of
        Lii v => v
      | Rii v => v
      | LRii v => v
      | NOii => v
  val noii : id_item = (NOii,NOii,NOii,NOii)
  fun id_item_int (ii : id_item) = id_item (#1 ii) 0
  fun id_item_double (ii : id_item) = id_item (#2 ii) 0.0
  fun id_item_bool (ii : id_item) = id_item (#3 ii) false
  fun id_item_complex (ii : id_item) = id_item (#4 ii) (0.0,0.0)

  infix >>=
  (* Tag expressions with their types, such that we can discover
     e.g. when to perform scalar extension and insert conversions
     (i2d, b2i) *)
  datatype tagged_exp =                (* Terms *)
      Bs of BOOL                       (*   boolean *)
    | Is of INT                        (*   integer *)
    | Ds of DOUBLE                     (*   double *)
    | Xs of COMPLEX                    (*   complex *)
    | Cs of CHAR                       (*   char *)
    | Abs of Bool ndarray              (*   boolean array *)
    | Ais of Int Num ndarray           (*   integer array *)
    | Ads of Double Num ndarray        (*   double array *)
    | Axs of Complex Num ndarray       (*   complex array *)
    | Acs of Char ndarray              (*   char array *)
    | Ts of tagged_exp list            (*   tuple *)
    | Fs of (tagged_exp list -> tagged_exp M) * id_item  (*   function in-lining *)

  local
  fun itemize [] = ""
    | itemize [s1, s2] = s1 ^ " and " ^ s2
    | itemize ss = 
      let fun item [] = raise Fail "itemize.impossible"
            | item [s] = "and " ^ s
            | item (s::ss) = s ^ ", " ^ item ss
      in item ss
      end
  in
  fun pp_tagged_exp s =
      case s of
          Bs _ => "a scalar boolean"
        | Is _ => "a scalar integer"
        | Ds _ => "a scalar double"
        | Xs _ => "a scalar complex"
        | Cs _ => "a scalar character"
        | Abs _ => "a boolean array"
        | Ais _ => "an integer array"
        | Ads _ => "a double array"
        | Axs _ => "a complex array"
        | Acs _ => "a character array"
        | Ts ss => "a tuple containing " ^ itemize (List.map pp_tagged_exp ss)
        | Fs _ => "a function"
  end

  fun lets (s : tagged_exp) : tagged_exp M = case s of
                   Bs x => (*ret s*)lett x >>= (ret o Bs)
                 | Is x => (*ret s*)lett x >>= (ret o Is)
                 | Ds x => (*ret s*)lett x >>= (ret o Ds)
                 | Xs x => (*ret s*)lett x >>= (ret o Xs)
                 | Cs x => (*ret s*)lett x >>= (ret o Cs)
                 | Abs mb => letm mb >>= (ret o Abs)
                 | Ais mi => letm mi >>= (ret o Ais)
                 | Ads md => letm md >>= (ret o Ads)
                 | Axs md => letm md >>= (ret o Axs)
                 | Acs md => letm md >>= (ret o Acs)
                 | Ts _ => ret s
                 | Fs _ => ret s

  open AplAst
  type env = (id, tagged_exp) Util.alist
  val lookup = Util.lookupAlist
  val empty : env = Util.emptyAlist ()
  infix ++ 
  val op ++ = Util.plusAlist
  fun repair s = String.translate (fn #"-" => "~"
                                    | c => String.str c) s
  fun StoI s = Int32.fromString (repair s)
  fun StoD s = Real.fromString (repair s)
in

(* Error functions, raises Fail *)
fun compError msg =
    raise Fail ("Compile Error: " ^ msg)

fun compErr r msg =
    compError (Region.pp r ^ ".\n  " ^ msg)

fun compErrS r s msg =
    compErr r (msg ^ ", but got " ^ pp_tagged_exp s)

(***** Compile support functions *****)

(* Compile monadic, polymorphic, operation from arrays to arrays
 *  Used for: reverse, vreverse, mem
 *)
fun compOpr_a2a e (opr1 : Int Num ndarray -> Int Num ndarray)
                  (opr2 : Double Num ndarray -> Double Num ndarray)
                  (opr3 : Bool ndarray -> Bool ndarray)
                  (opr4 : Char ndarray -> Char ndarray)
                  (opr5 : Complex Num ndarray -> Complex Num ndarray)
                  (r : reg)
                    : tagged_exp -> tagged_exp M =
 fn (Ais a) => ret (Ais(opr1 a))
  | (Ads a) => ret (Ads(opr2 a))
  | (Axs a) => ret (Axs(opr5 a))
  | (Abs a) => ret (Abs(opr3 a))
  | (Acs a) => ret (Acs(opr4 a))
  | (Is x) => ret (Is x)
  | (Ds x) => ret (Ds x)
  | (Xs x) => ret (Xs x)
  | (Bs x) => ret (Bs x)
  | (Cs x) => ret (Cs x)
  | _ => compErr r ("expects an array as argument in " ^ pr_exp e)

fun compOpr_a2a' e (opr1 : Int Num ndarray -> Int Num ndarray)
                   (opr2 : Double Num ndarray -> Double Num ndarray)
                   (opr3 : Complex Num ndarray -> Complex Num ndarray)
                   (opr4 : Bool ndarray -> Bool ndarray)
                   (opr5 : Char ndarray -> Char ndarray)
                   (opr6 : Int Num T.exp -> Int Num T.exp)
                   (opr7 : Double Num T.exp -> Double Num T.exp)
                   (opr8 : Complex Num T.exp -> Complex Num T.exp)
                   (opr9 : Bool T.exp -> Bool T.exp)
                   (opr10 : Char T.exp -> Char T.exp)
                   (r : reg)
                    : tagged_exp -> tagged_exp M =
 fn (Ais a) => ret (Ais(opr1 a))
  | (Ads a) => ret (Ads(opr2 a))
  | (Axs a) => ret (Axs(opr3 a))
  | (Abs a) => ret (Abs(opr4 a))
  | (Acs a) => ret (Acs(opr5 a))
  | (Is x) => ret (Is(opr6 x))
  | (Ds x) => ret (Ds(opr7 x))
  | (Xs x) => ret (Xs(opr8 x))
  | (Bs x) => ret (Bs(opr9 x))
  | (Cs x) => ret (Cs(opr10 x))
  | _ => compErr r ("expects an array as argument in " ^ pr_exp e)

(* Compile dyadic operations with an integer as first argument and
 * an array as second argument.
 *
 *  Used for: horizontal and vertical rotation
 *)
fun compOpr2_i8a2a e (opr1 : INT -> Int Num ndarray -> Int Num ndarray)
                     (opr2 : INT -> Double Num ndarray -> Double Num ndarray)
                     (opr3 : INT -> Complex Num ndarray -> Complex Num ndarray)
                     (opr4 : INT -> Bool ndarray -> Bool ndarray)
                     (opr5 : INT -> Char ndarray -> Char ndarray)
                     (r : reg)
                    : tagged_exp * tagged_exp -> tagged_exp M =
 fn (Is i1, Ais a2) => ret (Ais(opr1 i1 a2))
  | (Is i1, Ads a2) => ret (Ads(opr2 i1 a2))
  | (Is i1, Axs a2) => ret (Axs(opr3 i1 a2))
  | (Is i1, Abs a2) => ret (Abs(opr4 i1 a2))
  | (Is i1, Acs a2) => ret (Acs(opr5 i1 a2))
  | (Is i1, Is i2) => ret (Is i2)
  | (Is i1, Ds d2) => ret (Ds d2)
  | (Is i1, Xs x2) => ret (Xs x2)
  | (Is i1, Bs b2) => ret (Bs b2)
  | (Is i1, Cs c2) => ret (Cs c2)
  | (Bs b1, e2) => compOpr2_i8a2a e opr1 opr2 opr3 opr4 opr5 r (Is(b2i b1),e2)
  | _ => compErr r ("expects integer and array arguments in " ^ pr_exp e)

(* Compile dyadic operations with an integer as first argument and
 * an array or a scalar as second argument. Always returns an array.
 *
 *  Used for: take and drop
 *)
fun compOpr2_i8a2a_td e (opr1 : INT -> Int Num ndarray -> Int Num ndarray)
                        (opr2 : INT -> Double Num ndarray -> Double Num ndarray)
                        (opr3 : INT -> Complex Num ndarray -> Complex Num ndarray)
                        (opr4 : INT -> Bool ndarray -> Bool ndarray)
                        (opr5 : INT -> Char ndarray -> Char ndarray)
                        (r : reg)
                       : tagged_exp * tagged_exp -> tagged_exp M =
 fn (Is i1, Ais a2) => ret (Ais(opr1 i1 a2))
  | (Is i1, Ads a2) => ret (Ads(opr2 i1 a2))
  | (Is i1, Axs a2) => ret (Axs(opr3 i1 a2))
  | (Is i1, Abs a2) => ret (Abs(opr4 i1 a2))
  | (Is i1, Acs a2) => ret (Acs(opr5 i1 a2))
  | (Is i1, Is i2) => ret (Ais(opr1 i1 (vec(fromList Int [i2]))))
  | (Is i1, Ds d2) => ret (Ads(opr2 i1 (vec(fromList Double [d2]))))
  | (Is i1, Xs x2) => ret (Axs(opr3 i1 (vec(fromList Complex [x2]))))
  | (Is i1, Bs b2) => ret (Abs(opr4 i1 (vec(fromList Bool [b2]))))
  | (Is i1, Cs c2) => ret (Acs(opr5 i1 (vec(fromList Char [c2]))))
  | (Bs b1, e2) => compOpr2_i8a2a_td e opr1 opr2 opr3 opr4 opr5 r (Is(b2i b1),e2)
  | _ => compErr r ("expects integer and array arguments in " ^ pr_exp e)

(* Compile int * double -> double operations *)
fun compOpr2_i8d2d e (opr : INT -> DOUBLE -> DOUBLE) r : tagged_exp * tagged_exp -> tagged_exp M =
 fn (Is i1,Is i2) => ret (Ds (opr i1 (i2d i2)))
  | (Is i1,Ds d2) => ret (Ds (opr i1 d2))
  | (Is i1,Bs b2) => ret (Ds (opr i1 (i2d (b2i b2))))
  | (Bs b1, e2) => compOpr2_i8d2d e opr r (Is(b2i b1),e2)
  | _ => compErr r ("expects integer and double arguments in " ^ pr_exp e)

(* Compile dyadic array operations, perform scalar extension if necessary 

    Used for: catenate
 *)
fun compCat (opr1 : Int Num ndarray -> Int Num ndarray -> Int Num ndarray)
            (opr2 : Double Num ndarray -> Double Num ndarray -> Double Num ndarray)
            (opr3 : Complex Num ndarray -> Complex Num ndarray -> Complex Num ndarray)
            (opr4 : Bool ndarray -> Bool ndarray -> Bool ndarray)
            (opr5 : Char ndarray -> Char ndarray -> Char ndarray) 
            (r :reg)
           : tagged_exp * tagged_exp -> tagged_exp M =
 fn (Ais a1, Ais a2) => ret (Ais(opr1 a1 a2))
  | (Ads a1, Ads a2) => ret (Ads(opr2 a1 a2))
  | (Axs a1, Axs a2) => ret (Axs(opr3 a1 a2))
  | (Abs a1, Abs a2) => ret (Abs(opr4 a1 a2))
  | (Acs a1, Acs a2) => ret (Acs(opr5 a1 a2))

  | (Is v1,Is v2) => ret(Ais(opr1 (scl v1) (scl v2)))
  | (Ds v1,Ds v2) => ret(Ads(opr2 (scl v1) (scl v2)))
  | (Xs v1,Xs v2) => ret(Axs(opr3 (scl v1) (scl v2)))
  | (Bs v1,Bs v2) => ret(Abs(opr4 (scl v1) (scl v2)))
  | (Cs v1,Cs v2) => ret(Acs(opr5 (scl v1) (scl v2)))

  | (Is v1,Ais a2) => ret(Ais(opr1 (scl v1) a2))
  | (Ds v1,Ads a2) => ret(Ads(opr2 (scl v1) a2))
  | (Xs v1,Axs a2) => ret(Axs(opr3 (scl v1) a2))
  | (Bs v1,Abs a2) => ret(Abs(opr4 (scl v1) a2))
  | (Cs v1,Acs a2) => ret(Acs(opr5 (scl v1) a2))
  | (Ais a1,Is v2) => ret(Ais(opr1 a1 (scl v2)))
  | (Ads a1,Ds v2) => ret(Ads(opr2 a1 (scl v2)))
  | (Axs a1,Xs v2) => ret(Axs(opr3 a1 (scl v2)))
  | (Abs a1,Bs v2) => ret(Abs(opr4 a1 (scl v2)))
  | (Acs a1,Cs v2) => ret(Acs(opr5 a1 (scl v2)))

  | (Abs a1, e2) => compCat opr1 opr2 opr3 opr4 opr5 r (Ais(each (ret o b2i) a1),e2)
  | (e1, Abs a2) => compCat opr1 opr2 opr3 opr4 opr5 r (e1,Ais(each (ret o b2i) a2))

  | (Bs v1, e2) => compCat opr1 opr2 opr3 opr4 opr5 r (Is(b2i v1),e2)
  | (e1,Bs v2) => compCat opr1 opr2 opr3 opr4 opr5 r (e1,Is(b2i v2))

  | (Is v1, e2) => compCat opr1 opr2 opr3 opr4 opr5 r (Ds(i2d v1),e2)
  | (e1,Is v2) => compCat opr1 opr2 opr3 opr4 opr5 r (e1,Ds(i2d v2))

  | (Ais a1, e2) => compCat opr1 opr2 opr3 opr4 opr5 r (Ads(each (ret o i2d) a1),e2)
  | (e1, Ais a2) => compCat opr1 opr2 opr3 opr4 opr5 r (e1,Ads(each (ret o i2d) a2))

  | (Ds v1, e2) => compCat opr1 opr2 opr3 opr4 opr5 r (Xs(d2x v1),e2)
  | (e1,Ds v2) => compCat opr1 opr2 opr3 opr4 opr5 r (e1,Xs(d2x v2))
                          
  | (Ads a1, e2) => compCat opr1 opr2 opr3 opr4 opr5 r (Axs(each (ret o d2x) a1),e2)
  | (e1, Ads a2) => compCat opr1 opr2 opr3 opr4 opr5 r (e1,Axs(each (ret o d2x) a2))
                            
  | (e1,e2) => compErr r ("expects arrays of compatible shape - received " ^
                          pp_tagged_exp e1 ^ " and " ^ pp_tagged_exp e2)

(* Compile dyadic operation, perform scalar extension if necessary *)
fun compOpr2' (opi : INT * INT -> INT)
              (opd : DOUBLE * DOUBLE -> DOUBLE)
              (opx : COMPLEX * COMPLEX -> COMPLEX)
              (err : unit -> tagged_exp M)
             : tagged_exp * tagged_exp -> tagged_exp M =
    let val rec F =
         fn (Is i1, Is i2)   => ret(Is(opi(i1,i2)))
          | (Ds d1, Ds d2)   => ret(Ds(opd(d1,d2)))
          | (Xs x1, Xs x2)   => ret(Xs(opx(x1,x2)))
          | (Ais a1, Ais a2) => ret(Ais(zipWith (ret o opi) a1 a2))
          | (Ads a1, Ads a2) => ret(Ads(zipWith (ret o opd) a1 a2))
          | (Axs a1, Axs a2) => ret(Axs(zipWith (ret o opx) a1 a2))
          | (Ais a1, Is i2)  => ret(Ais(each (fn x => ret(opi(x,i2)))a1))
          | (Ads a1, Ds d2)  => ret(Ads(each (fn x => ret(opd(x,d2)))a1))
          | (Axs a1, Xs x2)  => ret(Axs(each (fn x => ret(opx(x,x2)))a1))
          | (Is i1, Ais a2)  => ret(Ais(each (fn x => ret(opi(i1,x)))a2))
          | (Ds d1, Ads a2)  => ret(Ads(each (fn x => ret(opd(d1,x)))a2))
          | (Xs x1, Axs a2)  => ret(Axs(each (fn x => ret(opx(x1,x)))a2))
          | (Bs b1, e2)      => F(Is(b2i b1),e2)
          | (e1, Bs b2)      => F(e1,Is(b2i b2))
          | (Is i1, e2)      => F(Ds(i2d i1),e2)
          | (e1, Is i2)      => F(e1,Ds(i2d i2))
          | (Abs a1, e2)     => F(Ais(each (ret o b2i) a1),e2)
          | (e1, Abs a2)     => F(e1,Ais(each (ret o b2i) a2))
          | (Ais a1, e2)     => F(Ads(each (ret o i2d) a1),e2)
          | (e1, Ais a2)     => F(e1,Ads(each (ret o i2d) a2))
          | (Ds d1, e2)      => F(Xs(d2x d1),e2)
          | (e1, Ds d2)      => F(e1,Xs(d2x d2))
          | (Ads a1, e2)     => F(Axs(each (ret o d2x) a1),e2)
          | (e1, Ads a2)     => F(e1,Axs(each (ret o d2x) a2))
          | _ => err()
    in F
    end

fun compOpr2 opi opd opx r : tagged_exp * tagged_exp -> tagged_exp M = 
    compOpr2' opi opd opx (fn() => compErr r "expects numerical argument arrays")

(* Compile dyadic operation on integers, raises Fail on reals *)
fun compOpr2III (opi : INT * INT -> INT) (err : unit -> unit) : tagged_exp * tagged_exp -> tagged_exp M =
    let fun er _ = (err(); raise Fail "compOpr2III.impossible")
    in compOpr2' opi er er er
    end

(* Compile dyadic operation on Booleans *)
fun compBoolOp (opb : BOOL * BOOL -> BOOL) (r : reg) : tagged_exp * tagged_exp -> tagged_exp M =
 fn (Bs b1, Bs b2) => ret(Bs(opb(b1,b2)))
  | (Abs bs1, Abs bs2) => ret(Abs(zipWith (ret o opb) bs1 bs2))
  | (Abs bs1, Bs b2) => ret(Abs(each (fn x => ret(opb(x,b2)))bs1))
  | (Bs b1, Abs bs2) => ret(Abs(each (fn x => ret(opb(b1,x)))bs2))
  | _ => compErr r "expects boolean argument arrays"

(* Compile dyadic compare-operations *)
fun compCompare (opi : INT * INT -> BOOL)
            (opd : DOUBLE * DOUBLE -> BOOL)
            (opc : CHAR * CHAR -> BOOL)
            (r : reg)
           : tagged_exp * tagged_exp -> tagged_exp M =
 fn (Is i1, Is i2) => ret(Bs(opi(i1,i2)))
  | (Ds d1, Ds d2) => ret(Bs(opd(d1,d2)))
  | (Cs c1, Cs c2) => ret(Bs(opc(c1,c2)))
  | (Ais a1, Ais a2) => ret(Abs(zipWith (ret o opi) a1 a2))
  | (Ads a1, Ads a2) => ret(Abs(zipWith (ret o opd) a1 a2))
  | (Acs a1, Acs a2) => ret(Abs(zipWith (ret o opc) a1 a2))
  | (Ais a1, Is i2) => ret(Abs(each (fn x => ret(opi(x,i2)))a1))
  | (Ads a1, Ds d2) => ret(Abs(each (fn x => ret(opd(x,d2)))a1))
  | (Acs a1, Cs c2) => ret(Abs(each (fn x => ret(opc(x,c2)))a1))
  | (Is i1, Ais a2) => ret(Abs(each (fn x => ret(opi(i1,x)))a2))
  | (Ds d1, Ads a2) => ret(Abs(each (fn x => ret(opd(d1,x)))a2))
  | (Cs c1, Acs a2) => ret(Abs(each (fn x => ret(opc(c1,x)))a2))
  | (Bs b1, e2) => compCompare opi opd opc r (Is(b2i b1),e2)
  | (e1, Bs b2) => compCompare opi opd opc r (e1,Is(b2i b2))
  | (Is i1, e2) => compCompare opi opd opc r (Ds(i2d i1),e2)
  | (e1, Is i2) => compCompare opi opd opc r (e1,Ds(i2d i2))
  | (Ais a1, e2) => compCompare opi opd opc r (Ads(each (ret o i2d) a1),e2)
  | (e1, Ais a2) => compCompare opi opd opc r (e1,Ads(each (ret o i2d) a2))
  | _ => compErr r "expects comparable argument arrays"

fun compCompare' (opi : INT * INT -> BOOL)
             (opd : DOUBLE * DOUBLE -> BOOL)
             (opb : BOOL * BOOL -> BOOL)
             (opc : CHAR * CHAR -> BOOL)
             (r : reg)
            : tagged_exp * tagged_exp -> tagged_exp M =
 fn (Bs b1, Bs b2) => ret(Bs(opb(b1,b2)))
  | (Abs bs1, Abs bs2) => ret(Abs(zipWith (ret o opb) bs1 bs2))
  | (Abs bs1, Bs b2) => ret(Abs(each (fn x => ret(opb(x,b2)))bs1))
  | (Bs b1, Abs bs2) => ret(Abs(each (fn x => ret(opb(b1,x)))bs2))
  | p => compCompare opi opd opc r p

(* Compile monadic operator with numeric argument *)
fun compOpr1' (opi : INT -> INT)
              (opd : DOUBLE -> DOUBLE)
              (opx : COMPLEX -> COMPLEX M)
              (err : unit -> tagged_exp M)
             : tagged_exp -> tagged_exp M =
    let val rec F =
         fn Is i => ret(Is(opi i))
          | Ds d => ret(Ds(opd d))
          | Xs x => opx x >>= (ret o Xs)
          | Bs b => F(Is(b2i b))
          | Ais a => ret(Ais(each (ret o opi) a))
          | Ads a => ret(Ads(each (ret o opd) a))
          | Axs a => ret(Axs(each opx a))
          | Abs a => F(Ais(each (ret o b2i) a))
          | s => err()
    in F
    end

fun compOpr1 opi opd opx r : tagged_exp -> tagged_exp M =
    compOpr1' opi opd opx (fn () => compErr r "expects numeric array argument")

(* compile monadic operator where the complex operation returns a double value *)              
fun compOpr1xd (opi : INT -> INT)
               (opd : DOUBLE -> DOUBLE)
               (opx : COMPLEX -> DOUBLE)
               r
             : tagged_exp -> tagged_exp M =
    let val rec F =
         fn Is i => ret(Is(opi i))
          | Ds d => ret(Ds(opd d))
          | Xs x => lett x >>= (fn x => ret(Ds(opx x)))
          | Bs b => F(Is(b2i b))
          | Ais a => ret(Ais(each (ret o opi) a))
          | Ads a => ret(Ads(each (ret o opd) a))
          | Axs a => ret(Ads(each (ret o opx) a))
          | Abs a => F(Ais(each (ret o b2i) a))
          | s => compErr r "expects numeric array argument"
    in F
    end

fun magnx c = (* assume c is let-bound *)
    powd(addd(muld(rex c,rex c),muld(imx c,imx c)),D 0.5)

fun conjx c =
    lett c >>= (fn c => ret(injx(rex c,negd(imx c))))

fun negx c =
    lett c >>= (fn c => ret(injx(negd(rex c),negd(imx c))))
         
(* Compile monadic operator working on Integers *)
fun compOpr1II (opi : INT -> INT) (err : unit -> unit) : tagged_exp -> tagged_exp M =
    let fun er _ = (err(); raise Fail "compOpr1II.impossible: the error-function supplied should raise an exception")
    in compOpr1' opi er er er
    end

(* Compile monadic operator working on reals and complex numbers *)
fun compOpr1d (opd : DOUBLE -> DOUBLE) (opx: COMPLEX -> COMPLEX) (r : reg) : tagged_exp -> tagged_exp M =
 fn Is i => ret(Ds(opd(i2d i)))
  | Ds d => ret(Ds(opd d))
  | Xs x => ret(Xs(opx x))
  | Bs b => compOpr1d opd opx r (Is(b2i b))
  | Ais a => ret(Ads(each (ret o opd o i2d) a))
  | Ads a => ret(Ads(each (ret o opd) a))
  | Axs a => ret(Axs(each (ret o opx) a))
  | Abs a => compOpr1d opd opx r (Ais(each (ret o b2i) a))
  | s => compErrS r s "expects numeric array argument"

(* Compile dyadic operator working on reals *)
fun compOpr2d (opd : DOUBLE * DOUBLE -> DOUBLE) (r : reg) : tagged_exp * tagged_exp -> tagged_exp M =
 fn (Ds d1,Ds d2) => ret(Ds(opd(d1,d2)))
  | (Ads a1,Ads a2) => ret(Ads(zipWith (ret o opd) a1 a2))
  | (Ads a1,Ds d2) => ret(Ads(each (ret o (fn x => opd(x,d2))) a1))
  | (Ds d1,Ads a2) => ret(Ads(each (ret o (fn x => opd(d1,x))) a2))
  | (Is i1,a2) => compOpr2d opd r (Ds(i2d i1),a2)
  | (a1,Is i2) => compOpr2d opd r (a1,Ds(i2d i2))
  | (Bs b1,a2) => compOpr2d opd r (Is(b2i b1),a2)
  | (a1,Bs b2) => compOpr2d opd r (a1,Is(b2i b2))
  | (Ais a1,a2) => compOpr2d opd r (Ads(each (ret o i2d) a1),a2)
  | (a1,Ais a2) => compOpr2d opd r (a1,Ads(each (ret o i2d) a2))
  | (Abs a1,a2) => compOpr2d opd r (Ads(each (ret o i2d o b2i) a1),a2)
  | (a1,Abs a2) => compOpr2d opd r (a1,Ads(each (ret o i2d o b2i) a2))
  | (s as Axs _, _) => compErrS r s "expects non-complex arguments"
  | (_, s as Axs _) => compErrS r s "expects non-complex arguments"
  | (s as Xs _, _) => compErrS r s "expects non-complex arguments"
  | (_, s as Xs _) => compErrS r s "expects non-complex arguments"
  | _ => compErr r "expects numeric array arguments"

(* Compile monadic operators with an integer result. 
   Arguments can be both integer and real.
 *)
fun compOpr1i (opi : INT -> INT)
              (opd : DOUBLE -> INT)
              (r : reg)
             : tagged_exp -> tagged_exp M =
 fn Is i => ret(Is(opi i))
  | Ds d => ret(Is(opd d))
  | s as Xs _ => compErrS r s "expects non-complex argument"
  | Bs b => compOpr1i opi opd r (Is(b2i b))
  | Ais a => ret(Ais(each (ret o opi) a))
  | Ads a => ret(Ais(each (ret o opd) a))
  | s as Axs _ => compErrS r s "expects non-complex argument"
  | Abs a => compOpr1i opi opd r (Ais(each (ret o b2i) a))
  | s => compErrS r s "expects numeric array argument"

fun compOpr1b opb r : tagged_exp -> tagged_exp M =
 fn Bs b => ret(Bs(opb b))
  | Abs a => ret(Abs(each (ret o opb) a))
  | s => compErrS r s "expects boolean array argument"

(* Compile the circular operator (○) *)
fun circularOp (r : AplAst.reg) (x : INT) (y : DOUBLE) : DOUBLE = 
    case Exp.T.unS (typeOf x) of
        NONE => compErr r ("incorrect type (" ^ Exp.T.prType (typeOf x) ^
                           ") of left-argument to circular-function")
      | SOME (_,rnk) => 
          (case Exp.T.unRnk rnk of
              NONE => compErr r "expects static argument to circular-function"
            | SOME ~4 => muld(addd(y, D 1.0),
                              powd(divd(subd(y, D 1.0), addd(y, D 1.0)), D 0.5))
            | SOME ~3 => atan y
            | SOME ~2 => acos y
            | SOME ~1 => asin y
            | SOME  0 => powd(subd(D 1.0, powd(y, D 2.0)), D 0.5)
            | SOME  1 => sin y
            | SOME  2 => cos y
            | SOME  3 => tan y
            | SOME  4 => powd(addd(D 1.0, powd(y, D 2.0)), D 0.5)
            | SOME  5 => sinh y
            | SOME  6 => cosh y
            | SOME  7 => tanh y
            | SOME xi => compErr r ("unsupported left-argument (" ^ Int.toString xi ^
                                    ") to circular-function"))

datatype classifier = BOOL_C | INT_C | DOUBLE_C | COMPLEX_C | CHAR_C | TUPLE_C of classifier list | UNKNOWN_C 
local
  fun class v =
      case v of
          Is _ => INT_C
        | Bs _ => BOOL_C
        | Ds _ => DOUBLE_C
        | Xs _ => COMPLEX_C
        | Cs _ => CHAR_C
        | Abs _ => BOOL_C
        | Ais _ => INT_C
        | Ads _ => DOUBLE_C
        | Axs _ => COMPLEX_C
        | Acs _ => CHAR_C
        | Ts vs => TUPLE_C(List.map class vs)
        | _ => UNKNOWN_C
  fun classify (l: tagged_exp list) (f: tagged_exp list -> tagged_exp M) : classifier =
      case runHack (f l) of
          SOME v => class v
        | NONE => UNKNOWN_C
in
  fun ppClass c =
      case c of
          BOOL_C => "BOOL_C"
        | INT_C => "INT_C"
        | DOUBLE_C => "DOUBLE_C"
        | COMPLEX_C => "COMPLEX_C"
        | CHAR_C => "CHAR_C"
        | TUPLE_C cs => "TUPLE_C(" ^ String.concatWith "," (List.map ppClass cs) ^ ")"
        | UNKNOWN_C => "UNKNOWN_C"
val dummyIntS = Is (I 0)
val dummyBoolS = Bs (B false)
val dummyDoubleS = Ds (D 0.0)
val dummyComplexS = Xs (X (0.0,0.0))
val dummyCharS = Cs (C 0w32)

(* Q: Shouldn't classify reduce try all combinations of boolean
      input-values to determine whether it always returns a boolean?

      (Simple example: {⍺ + ⍵})
 *)

val classifyReduce : (tagged_exp list -> tagged_exp M) -> classifier = classify [dummyBoolS,dummyBoolS]
val classifyEachMonadic : tagged_exp -> (tagged_exp list -> tagged_exp M) -> classifier = fn x => classify [x]
val classifyEachDyadic : (tagged_exp*tagged_exp) -> (tagged_exp list -> tagged_exp M) -> classifier = fn (x,y) => classify [x,y]
val classifyPower : tagged_exp -> (tagged_exp list -> tagged_exp M) -> classifier = fn x => classify [x]
end

(* Compile slash: reduction, compress, replicate  *)
fun compSlash (r : AplAst.reg) : tagged_exp list -> tagged_exp M =
    fn [Fs (f,ii)] =>
       ret(Fs (fn [Axs x] => ret(reduce (fn (x,y) =>
                                           (f[Xs x,Xs y] >>= (fn Xs z => ret z
                                                                  | _ => compErr r "expecting complex number as result of reduce")))
                                       (X(id_item_complex ii)) x Xs Axs)
                 | [Ads x] => ret(reduce (fn (x,y) =>
                                           (f[Ds x,Ds y] >>= (fn Ds z => ret z
                                                                  | _ => compErr r "expecting double as result of reduce")))
                                       (D(id_item_double ii)) x Ds Ads)
                 | [Ais x] => ret(reduce (fn (x,y) =>
                                           (f[Is x,Is y] >>= (fn Is z => ret z
                                                                  | _ => compErr r "expecting integer as result of reduce")))
                                       (I(id_item_int ii)) x Is Ais) 
                 | [Abs x] =>
                   (case classifyReduce f of
                        INT_C =>
                        ret(let val x = each (ret o b2i) x
                          in reduce (fn (x,y) =>
                                        (f[Is x,Is y] >>= (fn Is z => ret z
                                                               | _ => compErr r "expecting integer as result of reduce")))
                                    (I(id_item_int ii)) x Is Ais
                          end)
                      | BOOL_C => 
                        ret(reduce (fn (x,y) =>
                                     (f[Bs x,Bs y] >>= (fn Bs z => ret z
                                                            | _ => compErr r "expecting boolean as result of reduce")))
                                 (B(id_item_bool ii)) x Bs Abs)
                      | _ => compErr r "expecting boolean or integer as result of reduce")
                 | [Ds x] => ret(Ds x)
                 | [Xs x] => ret(Xs x)
                 | [Is x] => ret(Is x)
                 | [Cs x] => compErr r "char not supported as right argument to reduce"
                 | [Acs x] => compErr r "char array not supported as right argument to reduce"
                 | _ => compErr r "expecting array as right argument to reduce",
                noii))
  | [Abs x] => ret(Fs (fn [Ais y] => ret(Ais(compress x y))
                         | [Is y] => ret(Ais(compress x (scalar y)))
                         | [Abs y] => ret(Abs(compress x y))
                         | [Bs y] => ret(Abs(compress x (scalar y))) 
                         | [Ads y] => ret(Ads(compress x y))
                         | [Ds y] => ret(Ads(compress x (scalar y))) 
                         | [Acs y] => ret(Acs(compress x y))
                         | [Cs y] => ret(Acs(compress x (scalar y))) 
                         | _ => compErr r "compress does not support function arguments as right argument", 
                        noii))
  | [Ais x] => ret(Fs (fn [Ais y] => ret(Ais(replicate (I 0) x y))
                         | [Is y] => ret(Ais(replicate (I 0) x (scalar y)))
                         | [Abs y] => ret(Abs(replicate (B false) x y))
                         | [Bs y] => ret(Abs(replicate (B false) x (scalar y)))
                         | [Ads y] => ret(Ads(replicate (D 0.0) x y))
                         | [Ds y] => ret(Ads(replicate (D 0.0) x (scalar y)))
                         | [Acs y] => ret(Acs(replicate (C 0w32) x y))
                         | [Cs y] => ret(Acs(replicate (C 0w32) x (scalar y)))
                         | _ => compErr r "replicate does not support function arguments as right argument", 
                        noii))
  | [Ads _] => compErr r "replicate does not support double arrays as left argument"
  | [Axs _] => compErr r "replicate does not support complex arrays as left argument"
  | [Ds _] => compErr r "replicate does not support double scalars as left argument"
  | [Xs _] => compErr r "replicate does not support complex scalars as left argument"
  | [Is x] => compSlash r ([Ais(scalar x)])
  | [Bs x] => compSlash r ([Abs(scalar x)])
  | _ => compErr r "operator slash (reduce/replicate/compress) takes only one argument (monadic)"

(* Compile backslash: currently just scan *)
fun compBackslash (r : AplAst.reg) : tagged_exp list -> tagged_exp M =
    fn [Fs (f,ii)] =>
       ret(Fs (fn [Axs arr] => ret(Axs(scan (fn (x,y) =>
                                                (f[Xs x,Xs y] >>= (fn Xs z => ret z
                                                                  | _ => compErr r "expecting complex number as result of function applied to scan")))
                                            (X(id_item_complex ii))
                                            arr))
              | [Ads arr] => ret(Ads(scan (fn (x,y) =>
                                              (f[Ds x,Ds y] >>= (fn Ds z => ret z
                                                                | _ => compErr r "expecting double as result of function applied to scan")))
                                          (D(id_item_double ii))
                                          arr))
              | [Ais arr] => ret(Ais(scan (fn (x,y) =>
                                              (f[Is x,Is y] >>= (fn Is z => ret z
                                                                | _ => compErr r "expecting integer as result of function applied to scan")))
                                          (I(id_item_int ii))
                                          arr))
              | [Abs arr] =>
                (case classifyReduce f of
                     INT_C =>
                     let val x = each (ret o b2i) arr
                     in ret(Ais(scan (fn (x,y) =>
                                         (f[Is x,Is y] >>= (fn Is z => ret z
                                                           | _ => compErr r "expecting integer as result of function applied to scan")))
                                     (I(id_item_int ii))
                                     x))
                     end
                   | BOOL_C => 
                     ret(Abs(scan (fn (x,y) =>
                                      (f[Bs x,Bs y] >>= (fn Bs z => ret z
                                                        | _ => compErr r "expecting boolean as result of function applied to scan")))
                                  (B(id_item_bool ii))
                                  arr))
                   | _ => compErr r "expecting function as argument to scan that returns either an integer or a boolean")
              | _ => compErr r "Only arrays of integers, doubles, and complex numbers are supported as arguments to scan",noii))
  | _ => compErr r "This type of left-argument to scan not supported yet"

(* Compile power operator *)
fun compPower (benchFlag:{bench:bool}) r f n =
    let
        fun getPowerScl () = if #bench benchFlag then bench else powerScl
        fun getCondScl () = if #bench benchFlag then (fn f => fn b => bench f (b2i b)) else condScl
        fun getPower () = if #bench benchFlag then compError "bench operators works only on scalar values"
                          else power
        fun getPowerN () = if #bench benchFlag then compError "bench operators works only on scalar values"
                           else Unsafe.upowerN
        fun unAis (Ais a) = SOME a
          | unAis _ = NONE
        fun unIs (Is i) = SOME i
          | unIs (Bs b) = SOME(b2i b)
          | unIs _ = NONE
        fun unAbs (Abs a) = SOME a
          | unAbs _ = NONE
        fun unBs (Bs b) = SOME b
          | unBs _ = NONE
        fun unAds (Ads a) = SOME a
          | unAds _ = NONE
        fun unAcs (Acs a) = SOME a
          | unAcs _ = NONE
        fun unDs (Ds d) = SOME d
          | unDs _ = NONE
        fun unCs (Cs c) = SOME c
          | unCs _ = NONE
        fun unAxs (Axs a) = SOME a
          | unAxs _ = NONE
        fun unXs (Xs x) = SOME x
          | unXs _ = NONE
        fun unTs (Ts es) = SOME es
          | unTs _ = NONE
        fun doPower power cond ty g ung m =
            let fun h x = (f[g x] >>= (fn s => case ung s of SOME z => ret z
                                                           | NONE => compErrS r s ("expects " ^ ty ^ " as result of power")))
            in case n of
                   Is n => ret(g(power h n m))
                 | Bs b => ret(g(cond h b m))
                 | _ => compErrS r n "expects integer or boolean as right argument to power operator"
            end
        fun doPowerTup power ty g ung ts (es:tagged_exp list) =  (* ts is a list of classification types, which may turn out to be useful *)
            let fun packTexp (e,ut) =
                    case e of
                        Abs e => Unsafe.consUtuple(Unsafe.toUexpA e) ut
                      | Ais e => Unsafe.consUtuple(Unsafe.toUexpA e) ut
                      | Ads e => Unsafe.consUtuple(Unsafe.toUexpA e) ut
                      | Axs e => Unsafe.consUtuple(Unsafe.toUexpA e) ut
                      | Acs e => Unsafe.consUtuple(Unsafe.toUexpA e) ut
                      | Bs e => Unsafe.consUtuple(Unsafe.toUexp e) ut
                      | Is e => Unsafe.consUtuple(Unsafe.toUexp e) ut
                      | Ds e => Unsafe.consUtuple(Unsafe.toUexp e) ut
                      | Xs e => Unsafe.consUtuple(Unsafe.toUexp e) ut
                      | Cs e => Unsafe.consUtuple(Unsafe.toUexp e) ut
                      | _ => compErr r "expects non-function and non-tuple in pack for the power operator"
                fun pack es = List.foldr packTexp Unsafe.empUtuple es
                fun unpack es ut =
                    let fun unp i nil ut = nil
                          | unp i (e::es) ut =
                            let val v = Unsafe.prjUtuple i ut
                                val v = case e of
                                            Abs _ => Abs(Unsafe.fromUexpA v)
                                          | Ais _ => Ais(Unsafe.fromUexpA v)
                                          | Ads _ => Ads(Unsafe.fromUexpA v)
                                          | Axs _ => Axs(Unsafe.fromUexpA v)
                                          | Acs _ => Acs(Unsafe.fromUexpA v)
                                          | Bs _ => Bs(Unsafe.fromUexp v)
                                          | Is _ => Is(Unsafe.fromUexp v)
                                          | Ds _ => Ds(Unsafe.fromUexp v)
                                          | Xs _ => Xs(Unsafe.fromUexp v)
                                          | Cs _ => Cs(Unsafe.fromUexp v)
                                          | _ => compErr r "expects non-function and non-tuple in unpack for the power operator"
                            in v :: unp (i+1) es ut
                            end
                    in unp 0 es ut
                    end
                val g : Unsafe.utuple -> tagged_exp = g o (unpack es)
                val ung : tagged_exp -> Unsafe.utuple option = Option.map pack o ung
                fun h x = (f[g x] >>= (fn s => case ung s of SOME z => ret z
                                                           | NONE => compErrS r s ("expects " ^ ty ^ " as result of power")))
                val m = pack es
            in case n of
                   Is n => Unsafe.letUtuple(power h n m) >>= (ret o g)
                 | Bs b => Unsafe.letUtuple(power h (b2i b) m) >>= (ret o g)
                 | _ => compErrS r n "expects integer or boolean as right argument to power operator"
            end
        fun cond f b a = (getPower()) f (b2i b) a
        fun mkDummy e =
            case e of
                Ads _ => (Ads(zilde()), DOUBLE_C)
              | Abs _ => (Abs(zilde()), BOOL_C)
              | Ais _ => (Ais(zilde()), INT_C)
              | Axs _ => (Axs(zilde()), COMPLEX_C)
              | Acs _ => (Acs(zilde()), CHAR_C)
              | Is _ => (dummyIntS, INT_C)
              | Bs _ => (dummyBoolS, BOOL_C)
              | Ds _ => (dummyDoubleS, DOUBLE_C)
              | Xs _ => (dummyComplexS, COMPLEX_C)
              | Cs _ => (dummyCharS, CHAR_C)
              | Ts es =>
                let val (ds,ts) = mkDummies es
                in (Ts ds, TUPLE_C ts)
                end
              | Fs _ => compErr r "mkDummy not supporting functions"
        and mkDummies es =
            let val ps = List.map mkDummy es
                val ds = List.map #1 ps
                val ts = List.map #2 ps
            in (ds, ts)
            end
        fun lift_arg (t,te,e) =
            if t = te then e
            else case (t,te,e) of
                     (DOUBLE_C,INT_C,Ais m) => Ads(each (ret o i2d) m)
                   | (DOUBLE_C,INT_C,Is e) => Ds(i2d e)
                   | (INT_C,BOOL_C,Abs m) => Ais(each (ret o b2i) m)
                   | (INT_C,BOOL_C,Bs e) => Is(b2i e)
                   | (COMPLEX_C,DOUBLE_C,Ds e) => Xs(d2x e)                
                   | (COMPLEX_C,DOUBLE_C,Ads e) => Axs(each (ret o d2x) e)                
                   | (DOUBLE_C,BOOL_C,m) => lift_arg(DOUBLE_C,INT_C,lift_arg(INT_C,BOOL_C,m))
                   | (COMPLEX_C,INT_C,m) => lift_arg(COMPLEX_C,DOUBLE_C,lift_arg(DOUBLE_C,INT_C,m))
                   | (COMPLEX_C,BOOL_C,m) => lift_arg(COMPLEX_C,INT_C,lift_arg(INT_C,BOOL_C,m))
                   | _ => compErr r "lift_arg - failed to lift expression"
        fun lift_args (ts,tes,es) =
            case (ts,tes,es) of
                (nil,nil,nil) => nil
              | (t::ts,te::tes,e::es) => lift_arg(t,te,e) :: lift_args(ts,tes,es)
              | _ => compErr r "lift_args - mismatch between number of results and number of arguments in power function"
    in
    fn [Ads m] => doPower (getPower()) cond "double array" Ads unAds m
     | [Axs m] => doPower (getPower()) cond "complex array" Axs unAxs m
     | [Ais m] => doPower (getPower()) cond "integer array" Ais unAis m
     | [Acs m] => doPower (getPower()) cond "char array" Acs unAcs m
     | [Ds m] => doPower (getPowerScl()) (getCondScl()) "double scalar" Ds unDs m
     | [Cs m] => doPower (getPowerScl()) (getCondScl()) "char scalar" Cs unCs m
     | [Xs m] => doPower (getPowerScl()) (getCondScl()) "complex scalar" Xs unXs m
     | [Is m] =>
       (case classifyPower dummyIntS f of
            INT_C => doPower (getPowerScl()) (getCondScl()) "integer scalar" Is unIs m
          | DOUBLE_C => compPower benchFlag r f n [Ds(i2d m)]
          | _ => compErr r "expecting boolean or integer scalar as result of power")
     | [Bs m] =>
       (case classifyPower dummyBoolS f of
            INT_C => compPower benchFlag r f n [Is(b2i m)]
          | BOOL_C => doPower (getPowerScl()) (getCondScl()) "boolean scalar" Bs unBs m
          | DOUBLE_C => compPower benchFlag r f n [Ds(i2d(b2i m))]
          | _ => compErr r "expecting boolean, integer, or double scalar as result of power")
     | [Abs m] =>
       (case classifyPower (Abs(zilde())) f of
            INT_C => compPower benchFlag r f n [Ais(each (ret o b2i) m)]
          | BOOL_C => doPower (getPower()) cond "boolean array" Abs unAbs m
          | DOUBLE_C => compPower benchFlag r f n [Ads(each (ret o i2d o b2i) m)]
          | c => compErr r ("expecting boolean, integer, or double array as result of power - classified as " ^ ppClass c))
     | [Ts es] =>
       let val (dummies,argtypes) = mkDummies es
       in case classifyPower (Ts dummies) f of
              TUPLE_C ts => let val es = lift_args (ts,argtypes,es)
                                (* val () = print ("Compiling powerN: " ^ String.concatWith "; " (List.map ppClass ts) ^ "\n") *)
                            in doPowerTup (getPowerN()) "tuple" Ts unTs ts es
                            end
            | _ => compErr r "expecting tuple as result of power"
       end
     | [Fs _] => compErr r "function as argument to power operation is not support"
     | _ => compErr r "expecting scalar or array (boolean, integer, or double) as argument to power"
    end

(* Both monadic and dyadic versions *)
fun compPrimFunMD k r (mon,dya) ii =
    k(Fs (fn [x1,x2] => dya r (x1,x2)
           | [x] => mon r x
           | _ => compErr r "function expects one or two arguments",
          ii),
      empty) handle Fail msg => compErr r msg

(* Only monadic version *)
fun compPrimFunM k r mon =
    k(Fs (fn [x] => mon r x 
           | _ => compErr r "monadic function expects one argument",
          noii),
      empty) handle Fail msg => compErr r msg

(* Only dyadic version *)
fun compPrimFunD k r dya ii =
    k(Fs (fn [x1,x2] => dya r (x1,x2)
           | _ => compErr r "dyadic function expects two arguments",
          ii),
      empty) handle Fail msg => compErr r msg

(* Compile identifier expression *)
fun compId G (id,r) k =
  let 
      fun compIdOpt G (id,r) k =
          case lookup G id of
              SOME x => SOME(k(x,empty))
            | NONE => NONE
  in
    case compIdOpt G (id,r) k of
        SOME r => r
      | NONE => 
        let val id = AplAst.pr_id id
            fun consider id = if List.exists (fn x => id = x) ["$dot","$out","$slashbar","$log","$tally"] then 
                                "; consider including the prelude.apl file"
                              else ""
        in compErr r ("identifier " ^ id ^ " not in the environment" ^ consider id)
        end
  end

(* Compiles APL expressions to TAIL expressions *)
fun compileAst flags (G0 : env) (e : AplAst.exp) : (unit, Double Num) prog =
    let fun comp (G:env) (e : AplAst.exp) (k: tagged_exp * env -> tagged_exp M) : tagged_exp M =
            case e of
              IntE (s,r)    => (case StoI s of
                                   SOME 0 => k (Bs(B false),empty)
                                 | SOME 1 => k (Bs(B true),empty)
                                 | SOME i => k (Is(I i),empty) 
                                 | NONE => compErr r ("expects an integer, got " ^ s))
            | StrE ([w],r)  => k (Cs(C w),empty)
            | StrE (ws,r)   => k (Acs(vec(fromChars ws)),empty)
            | DoubleE (s,r) => (case StoD s of
                                    SOME d => k (Ds(D d),empty)
                                  | NONE => compErr r ("expects a double, got " ^ s))
            | ComplexE (s,r) =>
              (case String.tokens (fn c => c = #"j" orelse c = #"J") s of
                   [re,im] => (case (StoD re,StoD im) of
                                 (SOME re,SOME im) => k (Xs(X(re,im)),empty)
                                | _ => compErr r ("expects a complex number, got " ^ s))
                 | _ => compErr r ("expects a complex number, got " ^ s))
            | IndexE(e,opts,r) =>
              (*  
                    I ::= S | V | S;I | V;I | ;I

                    none(S)   = ;
                    none(V)   = ;
                    none(S;I) = ;none(I)
                    none(V;I) = ;none(I)
                    none(;I)  = ;none(I)

                       A[S]   = ...
                       A[V]   = ...
                       A[S;I] = A[S;none(I)][I]
                       A[V;I] = A[S;none(I)][;I]
              *)
              let fun toI (SOME(Bs b)) = SOME(Is(b2i b))
                    | toI (SOME(Abs bs)) = SOME(Ais(each (ret o b2i) bs)) 
                    | toI x = x
                  fun toi (Bs b) = b2i b
                    | toi (Is i) = i
                    | toi s = compErrS r s "expects a simple index (toi) "
                  fun findOpt n (NONE::xs) = findOpt (n+1) xs
                    | findOpt n (SOME(Is e)::xs) = SOME(n,e,xs)
                    | findOpt n (SOME s::xs) = compErrS r s ("expects a simple index")
                    | findOpt n nil = NONE
                  fun genericIndexing scalar array a =
                      compOpts G opts (fn (opts,_) =>
                                          case findOpt 1 (List.map toI opts) of
                                              SOME (x,i,xs) => 
                                              (case findOpt 1 xs of
                                                   NONE => (k(idxS x i a scalar array, empty)
                                                             handle Fail s => compErr r s)
                                                 | SOME _ => compErr r "only simple indexing supported")
                                            | NONE => k(array a,empty)
                                      )
              in comp G e (fn (Ais a,_) => genericIndexing Is Ais a
                            | (Ads a,_) => genericIndexing Ds Ads a
                            | (Axs a,_) => genericIndexing Xs Axs a
                            | (Abs a,_) => genericIndexing Bs Abs a
                            | (Acs a,_) => genericIndexing Cs Acs a
                            | (Ts ss, _) =>
                              compOpts G opts (fn ([SOME x],_) =>
                                                  (case Exp.T.unS (typeOf (toi x)) of
                                                     NONE => compErr r "static indexing required"
                                                   | SOME (_,rnk) =>
                                                     case Exp.T.unRnk rnk of
                                                         NONE => compErr r "static indexing is required"
                                                       | SOME i => 
                                                         let val s = List.nth (ss,i-1)
                                                                     handle _ => 
                                                                            compErr r ("index " ^ Int.toString i ^ " out of bounds; tuple has " 
                                                                                       ^ Int.toString (length ss) ^ " elements")
                                                         in k(s,empty)
                                                         end)
                                              | _ => compErr r "malformed indexing")
                            | (s, _) => compErrS r s "index function supports tuples, boolean arrays, double arrays, and integer arrays only")
              end
            | AssignE(v,nil,e,_) =>
              let fun cont f x = 
                      let val t = f x
                      in k(t,[(Var v,t)])
                      end
                  fun pp p a = if v = "$Quad" then p a else a
                  fun contA p f a = letm (pp p a) >>= cont f
                  fun contS p f a = lett (pp p a) >>= cont f
                  fun contT p f a = lett (pp p a) >>= cont f

              (* we should really modify the prety-printing functions
              so that they return 0 instead of returning the argument; also, here letm and lett should be applied to the argument before prety-printing...
               *)

              in comp G e (fn (Ais a,_) => contA prArrI Ais a
                          | (Ads a,_) => contA prArrD Ads a
                          | (Axs a,_) => contA prArrX Axs a
                          | (Abs a,_) => contA prArrB Abs a
                          | (Acs a,_) => contA prArrC Acs a
                          | (Is a,_) => contS prSclI Is a
                          | (Ds a,_) => contS prSclD Ds a
                          | (Xs a,_) => contS prSclX Xs a
                          | (Bs a,_) => contS prSclB Bs a
                          | (Cs a,_) => contS prSclC Cs a
                          | (s,_) => k(s,[(Var v,s)]))
              end
            | AssignE(id,opts,e,r) =>
              let fun toI (SOME(Bs b)) = b2i b
                    | toI (SOME(Is b)) = b
                    | toI (SOME s) = compErrS r s "expecting integer arguments for index assignments" 
                    | toI NONE = compErr r "non-supported indexing for index assignment - all indexes must be present"
                  fun genericIndexAssign unCon a =
                      comp G e (fn (e,G1) =>
                      compOpts (G++G1) opts (fn (opts,_) =>
                      let val is = List.map toI opts                      
                          val v = vec(fromList Int is)
                      in (lett(idxassign v a (unCon e))) >>= (fn x => k(Bs x,empty))
                      end))
                  fun unDs (Ds d) = d
                    | unDs (Is i) = i2d i
                    | unDs (Bs b) = i2d(b2i b)
                    | unDs s = compErrS r s "expecting rhs double, integer, or boolean in index assignment"
                  fun unXs (Xs x) = x
                    | unXs (Ds d) = d2x d
                    | unXs (Is i) = d2x(i2d i)
                    | unXs (Bs b) = d2x(i2d(b2i b))
                    | unXs s = compErrS r s "expecting rhs complex, double, integer, or boolean in index assignment"
                  fun unIs (Is i) = i
                    | unIs (Bs b) = b2i b
                    | unIs s = compErrS r s "expecting rhs integer or boolean in index assignment"
                  fun unBs (Bs b) = b
                    | unBs s = compErrS r s "expecting rhs boolean in index assignment"
                  fun unCs (Cs c) = c
                    | unCs s = compErrS r s "expecting rhs character in index assignment"
              in case lookup G (Var id) of
                     SOME (Abs a) => genericIndexAssign unBs a
                   | SOME (Ais a) => genericIndexAssign unIs a
                   | SOME (Ads a) => genericIndexAssign unDs a
                   | SOME (Axs a) => genericIndexAssign unXs a
                   | SOME (Acs a) => genericIndexAssign unCs a
                   | SOME s => compErrS r s "index assignment supported for arrays only"
                   | NONE => compErr r ("identifier " ^ id ^ " not in scope for index assignment")
              end
            | SeqE ([],r) => compErr r "empty sequences not supported"
            | SeqE ([e],_) => comp G e k
            | SeqE (e1::es,r) =>
              comp G e1 (fn (s1,G1) =>
              comp (G++G1) (SeqE(es,r)) (fn (s2,G2) =>
              k(s2,G1++G2)))
            | LambE((2,2),e,r) => (* dyadic operator => dyadic function *)
              k(Fs (fn [f,g] => compLam22 G e (f,g)
                     | _ => compErr r "dyadic operator (of class (2,2)) expects 2 operator arguments",
                    noii),
                empty)
            | LambE((2,1),e,r) => (* dyadic operator => monadic function *)
              k(Fs (fn [f,g] => compLam21 G e (f,g)
                     | _ => compErr r "dyadic operator (of class (2,1)) expects 2 operator arguments",
                    noii),
                empty)
            | LambE((1,1),e,r) => (* monadic operator => monadic function *)
              k(Fs (fn [f] => compLam11 G e f
                     | _ => compErr r "monadic operator (of class (1,1)) expects 1 operator argument",
                    noii),
                empty)
            | LambE((1,2),e,r) => (* monadic operator => dyadic function *)
              k(Fs (fn [f] => compLam12 G e f
                     | _ => compErr r "monadic operator (of class (1,2)) expects 1 operator argument",
                    noii),
                empty)
            | LambE((0,1),e,r) =>
              k(Fs (fn [x] => lets x >>= compLam01 G e
                     | l => compErr r ("monadic function expects one argument; received " ^ Int.toString(List.length l) ^ " arguments"),
                    noii),
                empty)
            | LambE((0,2),e,r) =>
              k(Fs (fn [x,y] => lets x >>= (fn x => lets y >>= (fn y => compLam02 G e (x,y)))
                     | l => compErr r ("dyadic function expects two arguments; received " ^ Int.toString(List.length l) ^ " arguments"),
                    noii),
                empty)
            | LambE((0,0),e,r)         => comp G (LambE((0,1),e,r)) k   (* support for constant functions *)
            | LambE((x,y),e,r)         => compErr r ("function or operator of class ("
                                                     ^ Int.toString x ^ ","
                                                     ^ Int.toString y ^ ") not supported")
            | IdE(Var v,r)             => compId G (Var v,r) k
            | IdE(Symb L.Omega,r)      => compId G (Symb L.Omega,r) k
            | IdE(Symb L.Omegaomega,r) => compId G (Symb L.Omegaomega,r) k
            | IdE(Symb L.Alpha,r)      => compId G (Symb L.Alpha,r) k
            | IdE(Symb L.Alphaalpha,r) => compId G (Symb L.Alphaalpha,r) k
            | IdE(Symb L.Zilde,_)      => k (Ais (zilde ()),empty)
            | VecE(es,r) =>
              comps G (rev es) (fn (nil,G1) => k (Ais (zilde ()),G1)
                                 | ([s],_) => compErr r "singleton vectors should not appear" 
                                 | (ss,G1) => 
                                   let val ss = rev ss
                                       fun vec' t ss = vec (fromList t ss)
                                       exception TRYTUPLE
                                       fun tryTuple () = k(Ts ss, G1)
                                   in (if List.exists (fn Xs _ => true | _ => false) ss then
                                         k(Axs(vec' Complex (List.map (fn Xs x => x
                                                                        | Ds d => d2x d
                                                                        | Is i => d2x(i2d i)
                                                                        | Bs b => d2x(i2d(b2i b))
                                                                        | _ => raise TRYTUPLE) ss)),G1)
                                       else if List.exists (fn Ds _ => true | _ => false) ss then
                                         k(Ads(vec' Double (List.map (fn Is e => i2d e
                                                                     | Ds d => d
                                                                     | Bs b => i2d(b2i b)
                                                                     | _ => raise TRYTUPLE) ss)),G1)
                                       else if List.exists (fn Is _ => true | _ => false) ss then
                                         k(Ais(vec' Int (List.map (fn Is e => e
                                                                  | Ds d => compErr r ("vec compilation: impossible")
                                                                  | Bs b => b2i b
                                                                  | _ => raise TRYTUPLE) ss)),G1)
                                       else if List.all (fn Bs _ => true | _ => false) ss then
                                         k(Abs(vec' Bool (List.map (fn Bs b => b | _ => compErr r ("vec compilation: impossible (bool)")) ss)),G1)
                                       else tryTuple())
                                      handle TRYTUPLE => tryTuple()
                                   end)
            | App1E(GenericE(f,_),e1,r) => comp G (App1E(f MONADIC,e1,r)) k
            | App1E(e0,e1,r) =>
              comp G e1 (fn (s,G') =>
              comp (G++G') e0 (fn (f,G'') =>
                                  case f of
                                    Fs (f,_) => f [s] >>= (fn s' => k(s',G'++G''))
                                  | s => compErrS r s "expects monadic function"))
            | App2E(GenericE(f,_),e1,e2,r) => comp G (App2E(f DYADIC,e1,e2,r)) k
            | App2E(e0,e1,e2,r) =>
              comp G e2 (fn (s2,G2) =>
              comp (G++G2) e0 (fn (f,G0) =>
              comp (G++G2++G0) e1 (fn (s1,G1) =>
                                      case f of
                                        Fs (f,_) => f[s1,s2] >>= (fn s' => k(s',G2++G0++G1))
                                      | s => compErrS r s "expects dyadic function")))
            | AppOpr1E(_,e0,e1,r) =>
              comp G e1 (fn (s,G') =>
              comp (G++G') e0 (fn (f,G'') =>
                                  case f of
                                    Fs (f,_) => f [s] >>= (fn s' => k(s',G'++G''))
                                  | s => compErrS r s "expects monadic operator"))
            | AppOpr2E(v,IdE(Symb L.Dot,r1),IdE(Symb L.Ring,r2),e1,r) =>
              comp G (AppOpr1E(v,IdE(Var "$out",Region.plus "out" r2 r1),e1,r)) k
            | AppOpr2E(_,e0,e1,e2,r) =>
              comp G e2 (fn (s2,G2) =>
              comp (G++G2) e1 (fn (s1,G1) =>
              comp (G++G2++G1) e0 (fn (f,G0) =>
                                      case f of
                                        Fs (f,_) => f [s1,s2] >>= (fn s => k(s,G2++G1++G0))
                                      | s => compErrS r s "expects dyadic operator")))
            | IdE(Symb L.StarDia,r) => 
              k(Fs (fn [Fs (f,ii),n] => ret(Fs (compPower {bench=false} r f n, noii))
                     | _ => compErr r "power operation expects a function as its first argument",
                    noii), 
                empty)
            | IdE(Symb L.Slash,r) => k (Fs (compSlash r, noii), empty)
            | IdE(Symb L.Backslash,r) => k (Fs (compBackslash r, noii), empty)
            | IdE(Symb L.Slashbar,r) => compId G (Var "$slashbar",r) k
            | IdE(Symb L.Dot,r) => compId G (Var "$dot",r) k
            | IdE(Symb L.Nmatch,r) => compId G (Var "$tally",r) k
            | IdE(Symb L.Each,r) => 
              k(Fs (fn [Fs (f,_)] =>
                       let fun errScl r t = compErr r ("problem with each - expecting scalar " ^ t ^ " as function result")
                           fun retI (Is v) = ret v
                             | retI _ = errScl r "int"
                           fun retB (Bs v) = ret v
                             | retB _ = errScl r "bool"
                           fun retD (Ds v) = ret v
                             | retD _ = errScl r "double"
                           fun retX (Xs v) = ret v
                             | retX _ = errScl r "complex"
                           fun retC (Cs v) = ret v
                             | retC _ = errScl r "char"
                           fun classifyM dummy g x =
                               let fun doM g ret = each (fn x => (f[g x] >>= ret))
                               in case classifyEachMonadic dummy f of
                                      INT_C => ret(Ais(doM g retI x))
                                    | BOOL_C => ret(Abs(doM g retB x))
                                    | DOUBLE_C => ret(Ads(doM g retD x))
                                    | COMPLEX_C => ret(Axs(doM g retX x))
                                    | CHAR_C => ret(Acs(doM g retC x))
                                    | TUPLE_C _ => compErr r "failed to classify monadic each operation - tuple not allowed"
                                    | UNKNOWN_C => compErr r "failed to classify monadic each operation"
                               end
                           fun classifyD (dummy1,dummy2) g1 g2 (xs,ys) =
                               let fun doD g1 g2 ret (xs,ys) = zipWith (fn (x,y) => (f[g1 x,g2 y] >>= ret)) xs ys
                               in case classifyEachDyadic (dummy1,dummy2) f of
                                      INT_C => ret(Ais(doD g1 g2 retI (xs,ys)))
                                    | BOOL_C => ret(Abs(doD g1 g2 retB (xs,ys)))
                                    | DOUBLE_C => ret(Ads(doD g1 g2 retD (xs,ys)))
                                    | COMPLEX_C => ret(Axs(doD g1 g2 retX (xs,ys)))
                                    | CHAR_C => ret(Acs(doD g1 g2 retC (xs,ys)))
                                    | TUPLE_C _ => compErr r "failed to classify dyadic each operation - tuple not allowed"
                                    | UNKNOWN_C => compErr r "failed to classify dyadic each operation"
                               end
                       in ret(Fs (fn [Abs x] => classifyM dummyBoolS    Bs x
                                   | [Ais x] => classifyM dummyIntS     Is x
                                   | [Ads x] => classifyM dummyDoubleS  Ds x
                                   | [Axs x] => classifyM dummyComplexS Xs x
                                   | [Acs x] => classifyM dummyCharS    Cs x

                                   | [Abs x, Abs y] => classifyD (dummyBoolS,dummyBoolS)    Bs Bs (x,y)
                                   | [Abs x, Ais y] => classifyD (dummyBoolS,dummyIntS)     Bs Is (x,y)
                                   | [Abs x, Ads y] => classifyD (dummyBoolS,dummyDoubleS)  Bs Ds (x,y)
                                   | [Abs x, Axs y] => classifyD (dummyBoolS,dummyComplexS) Bs Xs (x,y)
                                   | [Abs x, Acs y] => classifyD (dummyBoolS,dummyCharS)    Bs Cs (x,y)

                                   | [Ais x, Abs y] => classifyD (dummyIntS,dummyBoolS)    Is Bs (x,y)
                                   | [Ais x, Ais y] => classifyD (dummyIntS,dummyIntS)     Is Is (x,y)
                                   | [Ais x, Ads y] => classifyD (dummyIntS,dummyDoubleS)  Is Ds (x,y)
                                   | [Ais x, Axs y] => classifyD (dummyIntS,dummyComplexS) Is Xs (x,y)
                                   | [Ais x, Acs y] => classifyD (dummyIntS,dummyCharS)    Is Cs (x,y)

                                   | [Ads x, Abs y] => classifyD (dummyDoubleS,dummyBoolS)    Ds Bs (x,y)
                                   | [Ads x, Ais y] => classifyD (dummyDoubleS,dummyIntS)     Ds Is (x,y)
                                   | [Ads x, Ads y] => classifyD (dummyDoubleS,dummyDoubleS)  Ds Ds (x,y)
                                   | [Ads x, Axs y] => classifyD (dummyDoubleS,dummyComplexS) Ds Xs (x,y)
                                   | [Ads x, Acs y] => classifyD (dummyDoubleS,dummyCharS)    Ds Cs (x,y)

                                   | [Axs x, Abs y] => classifyD (dummyComplexS,dummyBoolS)    Xs Bs (x,y)
                                   | [Axs x, Ais y] => classifyD (dummyComplexS,dummyIntS)     Xs Is (x,y)
                                   | [Axs x, Ads y] => classifyD (dummyComplexS,dummyDoubleS)  Xs Ds (x,y)
                                   | [Axs x, Axs y] => classifyD (dummyComplexS,dummyComplexS) Xs Xs (x,y)
                                   | [Axs x, Acs y] => classifyD (dummyComplexS,dummyCharS)    Xs Cs (x,y)
                                                                 
                                   | [Acs x, Abs y] => classifyD (dummyCharS,dummyBoolS)    Cs Bs (x,y)
                                   | [Acs x, Ais y] => classifyD (dummyCharS,dummyIntS)     Cs Is (x,y)
                                   | [Acs x, Ads y] => classifyD (dummyCharS,dummyDoubleS)  Cs Ds (x,y)
                                   | [Acs x, Axs y] => classifyD (dummyCharS,dummyComplexS) Cs Xs (x,y)
                                   | [Acs x, Acs y] => classifyD (dummyCharS,dummyCharS)    Cs Cs (x,y)

                                   | _ => compErr r "expecting array as right argument to each operation",
                                  noii))
                       end
                     | _ => compErr r "expecting function as left argument to each operation",
                    noii), 
                empty)
            | IdE(Symb L.Iota,r) => compPrimFunM k r (fn r =>
                                                      fn Is i => (ret(Ais(iota i)) handle Fail msg => compErr r msg)
                                                       | Ais a => (ret(Ais(iota' a)) handle Fail msg => compErr r msg)
                                                       | s => compErrS r s "expects an integer argument to iota")
            | IdE(Symb L.Rtack,r) => compPrimFunMD k r (fn _ => ret, fn r => ret o #2) noii
            | IdE(Symb L.Ltack,r) => compPrimFunD k r (fn r => ret o #1) noii

            | IdE(Symb L.Ring,r) =>
              k(Fs (fn [Fs (f,_), Fs(g,_)] => ret(Fs (fn a => g a >>= (fn y => f [y]),
                                                      noii))
                     | [Fs (f,_), b] => ret (Fs (fn [a] => f [a,b]
                                                  | _ => compErr r "expecting only one argument to function", noii))
                     | [a, Fs (f,_)] => ret (Fs (fn [b] => f [a,b]
                                                  | _ => compErr r "expecting only one argument to function", noii))
                     | _ => compErr r "expecting function as (at least) one of the arguments to the compose operation",
                    noii),
                empty)
              
(*
            | IdE(Symb L.Ring,r) => compPrimFunMD k r (fn r => fn Fs f => ret(Fs(fn [x] => f [x,x]
                                                                                  | _ => compErr r "result function of monadic compose expects a single argument"))
                                                                | s => compErrS r s "expects function as argument to monadic compose", 
                                                       fn r => ret o #2) noii
*)

            | IdE(Symb L.Trans,r) => compPrimFunMD k r (fn r =>
                                                        fn Ais a => ret(Ais(transpose a))
                                                         | Ads a => ret(Ads(transpose a))
                                                         | Axs a => ret(Axs(transpose a))
                                                         | Abs a => ret(Abs(transpose a))
                                                         | Acs a => ret(Acs(transpose a))
                                                         | Is a => ret(Is a)
                                                         | Bs a => ret(Bs a)
                                                         | Ds a => ret(Ds a)
                                                         | Xs a => ret(Xs a)
                                                         | Cs a => ret(Cs a)
                                                         | s => compErrS r s "expects array as right argument to transpose",
                                                        fn r =>
                                                        fn (Ais a1, Ais a2) => ret(Ais(transpose2 (rav0 a1) a2))
                                                         | (Ais a1, Ads a2) => ret(Ads(transpose2 (rav0 a1) a2))
                                                         | (Ais a1, Axs a2) => ret(Axs(transpose2 (rav0 a1) a2))
                                                         | (Ais a1, Abs a2) => ret(Abs(transpose2 (rav0 a1) a2))
                                                         | (Ais a1, Acs a2) => ret(Acs(transpose2 (rav0 a1) a2))
                                                         | (_, Is a) => ret(Is a)
                                                         | (_, Bs a) => ret(Bs a)
                                                         | (_, Ds a) => ret(Ds a)
                                                         | (_, Xs a) => ret(Xs a)
                                                         | (_, Cs a) => ret(Cs a)
                                                         | _ => compErr r "expecting arrays as arguments to dyadic transpose") noii
            | IdE(Symb L.Rho,r) => 
              let val rec compDya =
                   fn r =>
                   fn (Ais a1, Ais a2) => ret(Ais(reshape (rav0 a1) a2))
                    | (Ais a1, Ads a2) => ret(Ads(reshape (rav0 a1) a2))
                    | (Ais a1, Axs a2) => ret(Axs(reshape (rav0 a1) a2))
                    | (Ais a1, Abs a2) => ret(Abs(reshape (rav0 a1) a2))
                    | (Ais a1, Acs a2) => ret(Acs(reshape (rav0 a1) a2))
                    | (Ais a1, Is a2) => ret(Ais(reshape (rav0 a1) (scalar a2)))
                    | (Ais a1, Ds a2) => ret(Ads(reshape (rav0 a1) (scalar a2)))
                    | (Ais a1, Xs a2) => ret(Axs(reshape (rav0 a1) (scalar a2)))
                    | (Ais a1, Bs a2) => ret(Abs(reshape (rav0 a1) (scalar a2)))
                    | (Ais a1, Cs a2) => ret(Acs(reshape (rav0 a1) (scalar a2)))
                    | (Abs a1, e2) => compDya r (Ais(each (ret o b2i) a1),e2)
                    | (Is i1, e2) => compDya r (Ais(scalar i1),e2)
                    | (Bs b1, e2) => compDya r (Is(b2i b1),e2)
                    | (Ds _,_) => compErr r "left argument to reshape operation cannot be a double"
                    | (Xs _,_) => compErr r "left argument to reshape operation cannot be a complex number"
                    | (Ads _,_) => compErr r "left argument to reshape operation cannot be an array of type double"
                    | (Axs _,_) => compErr r "left argument to reshape operation cannot be an array of complex numbers"
                    | (Cs _,_) => compErr r "left argument to reshape operation cannot be a char"
                    | (Acs _,_) => compErr r "left argument to reshape operation cannot be an array of type char"
                    | _ => compErr r "expecting arrays as left and right arguments to reshape operation"
              in compPrimFunMD k r (fn r =>
                                    fn Ais a => ret(Ais(vec(shape a)))
                                     | Ads a => ret(Ais(vec(shape a)))
                                     | Axs a => ret(Ais(vec(shape a)))
                                     | Abs a => ret(Ais(vec(shape a)))
                                     | Acs a => ret(Ais(vec(shape a)))
                                     | Is _ => ret(Ais(zilde()))
                                     | Ds _ => ret(Ais(zilde()))
                                     | Bs _ => ret(Ais(zilde()))
                                     | Cs _ => ret(Ais(zilde()))
                                     | s => compErrS r s "expects array as right argument to shape operation",
                                    compDya) noii
              end
            | IdE(Symb L.Cat,r) => compPrimFunMD k r (fn r =>
                                                      fn Ais a => ret(Ais(rav a))
                                                       | Ads a => ret(Ads(rav a))
                                                       | Axs a => ret(Axs(rav a))
                                                       | Abs a => ret(Abs(rav a))
                                                       | Acs a => ret(Acs(rav a))
                                                       | s => compErrS r s "expects array as right argument to ravel operation",
                                                      compCat catenate catenate catenate catenate catenate) noii
            | IdE(Symb L.Vcat,r) => compPrimFunMD k r (fn r => 
                                                       fn Ais a => ret(Ais(rav a))
                                                        | Ads a => ret(Ads(rav a))
                                                        | Axs a => ret(Axs(rav a))
                                                        | Abs a => ret(Abs(rav a))
                                                        | Acs a => ret(Acs(rav a))
                                                        | s => compErrS r s "expects array as right argument to ravel operation",
                                                       compCat catenate_first catenate_first catenate_first catenate_first catenate_first) noii
            | IdE(Symb L.Disclose,r) =>
              let fun comp_first r s =
                      case s of
                          Ais a => ret(Is(first a))
                        | Ads a => ret(Ds(first a))
                        | Axs a => ret(Xs(first a))
                        | Abs a => ret(Bs(first a))
                        | Acs a => ret(Cs(first a))
                        | Is a => ret s
                        | Ds a => ret s
                        | Xs a => ret s
                        | Bs a => ret s
                        | Cs a => ret s
                        | s => compErrS r s "expects an array or a scalar as right argument to disclose operation"
              in compPrimFunM k r comp_first (* (compOpr_a2a e first first first first) *)
              end
            | IdE(Symb L.Thorn,r) =>
              let fun comp_format r s =
                      case s of
                          Is a => ret(Acs(formatI a))
                        | Ds a => ret(Acs(formatD a))
                        | Xs a => ret(Acs(formatX a))
                        | Bs a => ret(Acs(formatI (b2i a)))
                        | s => compErrS r s "expects a numeric scalar as right argument to format operation"
              in compPrimFunM k r comp_format
              end
            | IdE(Symb L.Squad,r)    => compPrimFunM k r (compOpr_a2a' e mem mem mem mem mem memScl memScl memScl memScl memScl)
            | IdE(Symb L.Take,r)     => compPrimFunD k r (compOpr2_i8a2a_td e take take take take take) noii
            | IdE(Symb L.Drop,r)     => compPrimFunD k r (compOpr2_i8a2a_td e drop drop drop drop drop) noii
            | IdE(Symb L.Rot,r)      => compPrimFunMD k r (compOpr_a2a e reverse reverse reverse reverse reverse,
                                                           compOpr2_i8a2a e rotate rotate rotate rotate rotate) noii
            | IdE(Symb L.Vrot,r)     => compPrimFunMD k r (compOpr_a2a e vreverse vreverse vreverse vreverse vreverse,
                                                           compOpr2_i8a2a e vrotate vrotate vrotate vrotate vrotate) noii
            | IdE(Symb L.Add,r)      => compPrimFunMD k r (compOpr1 (fn x => x) (fn x => x) conjx,
                                                           compOpr2 addi addd addx) (LRii 0, LRii 0.0, NOii, LRii (0.0,0.0))
            | IdE(Symb L.Sub,r)      => compPrimFunMD k r (compOpr1 negi negd negx,
                                                           compOpr2 subi subd subx) (Rii 0, Rii 0.0, NOii, Rii (0.0,0.0))
            | IdE(Symb L.Times,r)    => compPrimFunMD k r (compOpr1i signi signd,
                                                           compOpr2 muli muld mulx) (LRii 1,LRii 1.0,NOii,LRii (1.0,0.0))
            | IdE(Symb L.Div,r)      => compPrimFunMD k r (compOpr1d (fn x => divd(D 1.0,x)) (fn _ => compErr r "expecting non-complex number to operation"),
                                                           compOpr2d divd) (Rii 1,Rii 1.0,NOii,NOii)
            | IdE(Symb L.Pow,r)      => compPrimFunMD k r (compOpr1d (fn x => expd x) expx,
                                                           compOpr2d powd) (Rii 1, Rii 1.0, NOii, Rii (1.0,0.0))
            | IdE(Symb L.Pipe,r)     => compPrimFunMD k r (compOpr1xd absi absd magnx,
                                                           compOpr2 resi resd (fn _ => compErr r "expecting non-complex number to operation")) (Lii 0,Lii 0.0,NOii,Lii(0.0,0.0))
            | IdE(Symb L.Max,r)      => compPrimFunMD k r (compOpr1i (fn x => x) ceil,
                                                           compOpr2 (Util.uncurry maxi) (Util.uncurry maxd) (fn _ => compErr r "expecting non-complex number to operation"))
                                                               (LRii(Util.minInt), LRii(Real.negInf),NOii,NOii)
            | IdE(Symb L.Min,r)      => compPrimFunMD k r (compOpr1i (fn x => x) floor,
                                                           compOpr2 (Util.uncurry mini) (Util.uncurry mind) (fn _ => compErr r "expecting non-complex number to operation"))
                                                               (LRii(Util.maxInt), LRii(Real.posInf),NOii,NOii)
            | IdE(Symb L.Lt,r)       => compPrimFunD k r (compCompare lti ltd ltc) (LRii 0,LRii 0.0,LRii false,NOii)
            | IdE(Symb L.Lteq,r)     => compPrimFunD k r (compCompare ltei lted ltec) (LRii 1,LRii 1.0,LRii true,NOii)
            | IdE(Symb L.Gt,r)       => compPrimFunD k r (compCompare gti gtd gtc) (LRii 0,LRii 0.0,LRii false,NOii)
            | IdE(Symb L.Gteq,r)     => compPrimFunD k r (compCompare gtei gted gtec) (LRii 1,LRii 1.0,LRii true,NOii)
            | IdE(Symb L.Eq,r)       => compPrimFunD k r (compCompare' eqi eqd eqb eqc) (LRii 1,LRii 1.0,LRii true,LRii(1.0,0.0))
            | IdE(Symb L.Neq,r)      => compPrimFunD k r (compCompare' neqi neqd xorb neqc) (LRii 0,LRii 0.0,LRii false,LRii(0.0,0.0))
            | IdE(Symb L.Circstar,r) => compPrimFunM k r (compOpr1d ln (fn _ => compErr r "expecting non-complex number to operation"))
            | IdE(Symb L.Circ,r)     => compPrimFunMD k r (compOpr1d (fn x => muld (x, pi ())) (fn _ => compErr r "expecting non-complex number to operation"),
                                                           compOpr2_i8d2d e (circularOp r)) (NOii,NOii,NOii,NOii)
            | IdE(Symb L.Qmark,r)    => compPrimFunM k r (fn r =>
                                                          fn Is i => ret(Ds(roll i))
                                                           | Bs b => ret(Ds(roll (b2i b)))
                                                           | Ais a => ret(Ads(each (ret o roll) a))
                                                           | Abs a => ret(Ads(each (ret o roll o b2i) a))
                                                           | s => compErrS r s "expects integer as right argumet to roll function")
            | IdE(Symb L.And,r)      => compPrimFunD k r (compBoolOp andb) (NOii,NOii,LRii true,NOii)
            | IdE(Symb L.Or,r)       => compPrimFunD k r (compBoolOp orb) (NOii,NOii,LRii false,NOii)
            | IdE(Symb L.Nand,r)     => compPrimFunD k r (compBoolOp nandb) (NOii,NOii,NOii,NOii)
            | IdE(Symb L.Nor,r)      => compPrimFunD k r (compBoolOp norb) (NOii,NOii,NOii,NOii)
            | IdE(Symb L.Tilde,r)    => compPrimFunM k r (compOpr1b notb)
            | e                      => compError (pr_exp e ^ " not implemented")
        and comps G nil k = k(nil,empty)
          | comps G (e::es) k = comp G e (fn (s,G1) => comps (G++G1) es (fn (ss,G2) => k(s::ss,G1++G2)))
        and compOpts G nil k = k(nil,empty)
          | compOpts G (NONE::es) k = compOpts G es (fn (ss,_) => k(NONE::ss,empty))
          | compOpts G (SOME e::es) k = comp G e (fn (s,_) => compOpts G es (fn (ss,_) => k(SOME s::ss,empty)))
        and compLam11 G e f =
            ret(Fs(fn [x] =>
                       lets x >>= (fn x => 
                       let val G' = [(Symb L.Alphaalpha, f),(Symb L.Omega, x)]
                       in comp (G++G') e (fn (s,_) => ret s)
                       end)
                     | _ => raise Fail "compLam11: expecting 1 argument",
                    noii))
        and compLam12 G e f =
            ret(Fs(fn [x,y] =>
                       lets x >>= (fn x => (lets y) >>= (fn y =>
                       let val G' = [(Symb L.Alphaalpha, f),(Symb L.Omega, y),(Symb L.Alpha, x)]
                       in comp (G++G') e (fn (s,_) => ret s)
                       end))
                     | _ => raise Fail "compLam12: expecting 2 arguments",
                    noii))
        and compLam22 G e (f,g) =
            ret(Fs(fn [x,y] =>
                       lets x >>= (fn x => (lets y) >>= (fn y =>
                       let val G' = [(Symb L.Alphaalpha, f),(Symb L.Omegaomega, g),(Symb L.Omega, y),(Symb L.Alpha, x)]
                       in comp (G++G') e (fn (s,_) => ret s)
                       end))
                     | _ => raise Fail "compLam22: expecting 2 arguments",
                    noii))
        and compLam21 G e (f,g) =
            ret(Fs(fn [x] =>
                       lets x >>= (fn x => 
                       let val G' = [(Symb L.Alphaalpha, f),(Symb L.Omegaomega, g),(Symb L.Omega, x)]
                       in comp (G++G') e (fn (s,_) => ret s)
                       end)
                     | _ => raise Fail "compLam21: expecting 2 arguments",
                    noii))
        and compLam01 G e x     = comp (G++[(Symb L.Omega,x)])                  e (fn (s,_) => ret s)
        and compLam02 G e (x,y) = comp (G++[(Symb L.Omega,y),(Symb L.Alpha,x)]) e (fn (s,_) => ret s)
        val c : tagged_exp M = comp G0 e (fn (s,_) => ret s)
        val c' : DOUBLE M = c >>= (fn s =>
                                case s of
                                  Is i => ret (i2d i)
                                | Bs b => ret (i2d(b2i b))
                                | Ds d => ret d
                                | _ => raise Fail ("Compile error: expects scalar double value as the result of a program - got " ^ pp_tagged_exp s))
    in runM flags Double c'
    end

val initial : (string * AplParse.class * tagged_exp) list =
    let fun liftBinOpIII s f =
            let fun err () = compError ("dyadic function " ^ s ^ " expects two integer arrays as arguments")
            in (s, AplParse.fun2, Fs(fn [a1,a2] => compOpr2III f err (a1,a2)
                             | _ => err(),noii))
            end
        fun liftUnOpII s f =
            let fun err () = compError ("monadic function " ^ s ^ " expects an integer array as argument")
            in (s, AplParse.fun1, Fs(fn [a] => compOpr1II f err a
                             | _ => err(),noii))
            end
        fun Fun1Acs2 g s f =
            (s, AplParse.fun1, Fs (fn [Acs x] => ret(g (f x))
                         | l => compError ("monadic function " ^ s ^ " expects character vector as argument"),
                          noii))
        fun Fun1Is2 g s f =
            (s, AplParse.fun1, Fs (fn [Is x] => ret(g (f x))
                         | [Bs x] => ret(g (f(b2i x)))
                         | l => compError ("monadic function " ^ s ^ " expects integer argument"),
                          noii))
        fun Opr2Fun1bench s =
            let val r = (Region.botloc,Region.botloc)
            in (s, AplParse.opr2fun1, Fs(fn [Fs(f,ii),n] => ret (Fs (compPower {bench=true} r f n, noii))
                                          | _ => compError "bench operation expects a function as its first argument",
                                         noii))
            end
     in
            [Fun1Acs2 Acs "Quad$ReadFile" readFile,
             Fun1Acs2 Ais "Quad$ReadIntVecFile" readIntVecFile,
             Fun1Acs2 Ads "Quad$ReadDoubleVecFile" readDoubleVecFile,
             Fun1Is2 Is "Quad$NOW" nowi,
             Opr2Fun1bench "Quad$BENCH",
             liftUnOpII "Quad$INT32NOT" noti,
             liftBinOpIII "Quad$INT32AND" andi,
             liftBinOpIII "Quad$INT32OR" ori,
             liftBinOpIII "Quad$INT32SHL" shli,
             liftBinOpIII "Quad$INT32SHR" shri,
             liftBinOpIII "Quad$INT32SHAR" shari,
             liftBinOpIII "Quad$INT32XOR" xori]
    end

val initialCompileEnv : env = List.map (fn (id,_,x) => (AplAst.Var id,x)) initial

end

val initialParseEnv : AplParse.env = List.foldl (fn ((id,c,_),e) => AplParse.add (id, [c]) e) AplParse.env0 initial
fun compile flags parsetree = compileAst flags initialCompileEnv parsetree

end
