functor Apl2Tail(X : TAIL) :
sig
  type flag = string * string option  (* supported flags: [-o f, -tl, -c, -v, -noopt, -p_types] *)
  val compileAndRun      : flag list -> string -> unit
  val compileAndRunFiles : flag list -> string list -> unit
end = 
struct

type flag =  string * string option

fun prln s = print(s ^ "\n")

fun minInt() = case Int.minInt of
                 SOME i => i
               | NONE => raise Fail "no minInt"
fun maxInt() = case Int.maxInt of
                 SOME i => i
               | NONE => raise Fail "no maxInt"

local
  open X

  datatype 'a identity_item = Lii of 'a
                            | Rii of 'a
                            | LRii of 'a
                            | NOii

  type id_item = int identity_item * real identity_item * bool identity_item
                 
  fun id_item ii v =
      case ii of
        Lii v => v
      | Rii v => v
      | LRii v => v
      | NOii => v
  val noii : id_item = (NOii,NOii,NOii)
  fun id_item_int (ii:id_item) = id_item (#1 ii) 0
  fun id_item_double (ii:id_item) = id_item (#2 ii) 0.0
  fun id_item_bool (ii:id_item) = id_item (#3 ii) false

  type mb = Bool m          (* Multidimensional bool array *)
  type mi = Int Num m       (* Multidimensional integer array *)
  type md = Double Num m    (* Multidimensional double array *)
  type mc = Char m          (* Multidimensional char array *)

  infix >>=

  datatype 'a N = S of 'a
                | M of 'a M
  infix >>>=
  val rett : 'a -> 'a N = fn a => S a
  fun (a: 'a N) >>>= (b: 'a -> 'b N) : 'b N =
      case a of
        M a => M(a >>= (fn a => case b a of S x => ret x
                                          | M m => m))
      | S a => b a
  fun subM (M c) = c
    | subM (S e) = ret e

  datatype ty = Ity | Dty | Bty | Cty | Aty of ty | FUNty of ty list -> ty | APPty of ty list * ty 

  datatype s =                         (* Terms *)
      Bs of BOOL                       (*   boolean *)
    | Is of INT                        (*   integer *)
    | Ds of DOUBLE                     (*   double *)
    | Cs of CHAR                       (*   char *)
    | Abs of mb                        (*   boolean array *)
    | Ais of mi                        (*   integer array *)
    | Ads of md                        (*   double array *)
    | Acs of mc                        (*   char array *)
    | Fs of (s list -> s N) * id_item  (*   function in-lining *)

  fun pp_s s =
      case s of
          Bs _ => "Bs"
        | Is _ => "Is"
        | Ds _ => "Ds"
        | Cs _ => "Cs"
        | Abs _ => "Abs"
        | Ais _ => "Ais"
        | Ads _ => "Ads"
        | Acs _ => "Acs"
        | Fs _ => "Fs"

  fun lets s = case s of
                   Bs _ => ret s
                 | Is _ => ret s
                 | Ds _ => ret s
                 | Cs _ => ret s
                 | Abs mb => letm mb >>= (fn x => ret(Abs x))
                 | Ais mi => letm mi >>= (fn x => ret(Ais x))
                 | Ads md => letm md >>= (fn x => ret(Ads x))
                 | Acs md => letm md >>= (fn x => ret(Acs x))
                 | Fs _ => ret s

  open AplAst
  type env = (id * s) list
  fun lookup (E:env) id =
      case List.find (fn (id',_) => id = id') E of
        SOME(_,r) => SOME r
      | NONE => NONE
  val emp : env = []
  fun plus (e1,e2) = e2@e1
  infix ++ 
  val op ++ = plus
  fun uncurry f (x,y) = f x y
  fun repair s = String.translate (fn #"-" => "~"
                                    | c => String.str c) s
  fun StoI s = Int.fromString(repair s)
  fun StoD s = Real.fromString(repair s)
in

fun compOpr2_i8a2a e opr1 opr2 opr3 opr4 =
 fn (Is i1, Ais a2) => S(Ais(opr1 i1 a2))
  | (Is i1, Ads a2) => S(Ads(opr2 i1 a2))
  | (Is i1, Abs a2) => S(Abs(opr3 i1 a2))
  | (Is i1, Acs a2) => S(Acs(opr4 i1 a2))
  | (Is i1, Is i2) => S(Is i2)
  | (Is i1, Ds d2) => S(Ds d2)
  | (Is i1, Bs b2) => S(Bs b2)
  | (Is i1, Cs c2) => S(Cs c2)
  | (Bs b1, e2) => compOpr2_i8a2a e opr1 opr2 opr3 opr4 (Is(b2i b1),e2)
  | _ => raise Fail ("compOpr2_i8a2a: expecting integer and array arguments in " ^ pr_exp e)

fun compOpr2_i8a2a_td e opr1 opr2 opr3 opr4 =
 fn (Is i1, Ais a2) => S(Ais(opr1 i1 a2))
  | (Is i1, Ads a2) => S(Ads(opr2 i1 a2))
  | (Is i1, Abs a2) => S(Abs(opr3 i1 a2))
  | (Is i1, Acs a2) => S(Acs(opr4 i1 a2))
  | (Is i1, Is i2) => S(Ais(opr1 i1 (vec(fromList Int [i2]))))
  | (Is i1, Ds d2) => S(Ads(opr2 i1 (vec(fromList Double [d2]))))
  | (Is i1, Bs b2) => S(Abs(opr3 i1 (vec(fromList Bool [b2]))))
  | (Is i1, Cs c2) => S(Acs(opr4 i1 (vec(fromList Char [c2]))))
  | (Bs b1, e2) => compOpr2_i8a2a_td e opr1 opr2 opr3 opr4 (Is(b2i b1),e2)
  | _ => raise Fail ("compOpr2_i8a2a_td: expecting integer and array arguments in " ^ pr_exp e)

fun compOpr2_i8d2d e opr =
 fn (Is i1,Is i2) => S(Ds (opr i1 (i2d i2)))
  | (Is i1,Ds d2) => S(Ds (opr i1 d2))
  | (Is i1,Bs b2) => S(Ds (opr i1 (i2d (b2i b2))))
  | (Bs b1, e2) => compOpr2_i8d2d e opr (Is(b2i b1),e2)
  | _ => raise Fail ("compOpr2_i8d2d: expecting integer and double arguments in " ^ pr_exp e)

fun compCat opr1 opr2 opr3 opr4 =
 fn (Ais a1, Ais a2) => S(Ais(opr1 a1 a2))
  | (Ads a1, Ads a2) => S(Ads(opr2 a1 a2))
  | (Abs a1, Abs a2) => S(Abs(opr3 a1 a2))
  | (Acs a1, Acs a2) => S(Acs(opr4 a1 a2))

  | (Is v1,Is v2) => S(Ais(opr1 (scl v1) (scl v2)))
  | (Ds v1,Ds v2) => S(Ads(opr2 (scl v1) (scl v2)))
  | (Bs v1,Bs v2) => S(Abs(opr3 (scl v1) (scl v2)))
  | (Cs v1,Cs v2) => S(Acs(opr4 (scl v1) (scl v2)))

  | (Is v1,Ais a2) => S(Ais(opr1 (scl v1) a2))
  | (Ds v1,Ads a2) => S(Ads(opr2 (scl v1) a2))
  | (Bs v1,Abs a2) => S(Abs(opr3 (scl v1) a2))
  | (Cs v1,Acs a2) => S(Acs(opr4 (scl v1) a2))
  | (Ais a1,Is v2) => S(Ais(opr1 a1 (scl v2)))
  | (Ads a1,Ds v2) => S(Ads(opr2 a1 (scl v2)))
  | (Abs a1,Bs v2) => S(Abs(opr3 a1 (scl v2)))
  | (Acs a1,Cs v2) => S(Acs(opr4 a1 (scl v2)))
(*
  | (Is i1,e2) => compCat opr1 opr2 opr3 (Ais(scl i1),e2)
  | (e1, Is i2) => compCat opr1 opr2 opr3 (e1,Ais(scl i2))
  | (Ds d1,e2) => compCat opr1 opr2 opr3 (Ads(scl d1),e2)
  | (e1, Ds d2) => compCat opr1 opr2 opr3 (e1,Ads(scl d2))
  | (Bs b1,e2) => compCat opr1 opr2 opr3 (Abs(scl b1),e2)
  | (e1, Bs b2) => compCat opr1 opr2 opr3 (e1,Abs(scl b2))
*)
  | (Abs a1, e2) => compCat opr1 opr2 opr3 opr4 (Ais(each (ret o b2i) a1),e2)
  | (e1, Abs a2) => compCat opr1 opr2 opr3 opr4 (e1,Ais(each (ret o b2i) a2))

  | (Bs v1, e2) => compCat opr1 opr2 opr3 opr4 (Is(b2i v1),e2)
  | (e1,Bs v2) => compCat opr1 opr2 opr3 opr4 (e1,Is(b2i v2))

  | (Is v1, e2) => compCat opr1 opr2 opr3 opr4 (Ds(i2d v1),e2)
  | (e1,Is v2) => compCat opr1 opr2 opr3 opr4 (e1,Ds(i2d v2))

  | (Ais a1, e2) => compCat opr1 opr2 opr3 opr4 (Ads(each (ret o i2d) a1),e2)
  | (e1, Ais a2) => compCat opr1 opr2 opr3 opr4 (e1,Ads(each (ret o i2d) a2))

  | _ => raise Fail "compCat: expecting two similar arrays as arguments"

fun compOpr2 opi opd =
 fn (Is i1, Is i2) => S(Is(opi(i1,i2)))
  | (Ds d1, Ds d2) => S(Ds(opd(d1,d2)))
  | (Ais a1, Ais a2) => S(Ais(zipWith (ret o opi) a1 a2))
  | (Ads a1, Ads a2) => S(Ads(zipWith (ret o opd) a1 a2))
  | (Ais a1, Is i2) => S(Ais(each (fn x => ret(opi(x,i2)))a1))
  | (Ads a1, Ds d2) => S(Ads(each (fn x => ret(opd(x,d2)))a1))
  | (Is i1, Ais a2) => S(Ais(each (fn x => ret(opi(i1,x)))a2))
  | (Ds d1, Ads a2) => S(Ads(each (fn x => ret(opd(d1,x)))a2))
  | (Bs b1, e2) => compOpr2 opi opd (Is(b2i b1),e2)
  | (e1, Bs b2) => compOpr2 opi opd (e1,Is(b2i b2))
  | (Is i1, e2) => compOpr2 opi opd (Ds(i2d i1),e2)
  | (e1, Is i2) => compOpr2 opi opd (e1,Ds(i2d i2))
  | (Abs a1, e2) => compOpr2 opi opd (Ais(each (ret o b2i) a1),e2)
  | (e1, Abs a2) => compOpr2 opi opd (e1,Ais(each (ret o b2i) a2))
  | (Ais a1, e2) => compOpr2 opi opd (Ads(each (ret o i2d) a1),e2)
  | (e1, Ais a2) => compOpr2 opi opd (e1,Ads(each (ret o i2d) a2))
  | _ => raise Fail "compOpr2.function"

fun compBoolOp opb =
 fn (Bs b1, Bs b2) => S(Bs(opb(b1,b2)))
  | (Abs bs1, Abs bs2) => S(Abs(zipWith (ret o opb) bs1 bs2))
  | (Abs bs1, Bs b2) => S(Abs(each (fn x => ret(opb(x,b2)))bs1))
  | (Bs b1, Abs bs2) => S(Abs(each (fn x => ret(opb(b1,x)))bs2))
  | _ => raise Fail "compBoolOp.function expecting boolean argument"

fun compCmp opi opd opc =
 fn (Is i1, Is i2) => S(Bs(opi(i1,i2)))
  | (Ds d1, Ds d2) => S(Bs(opd(d1,d2)))
  | (Cs c1, Cs c2) => S(Bs(opc(c1,c2)))
  | (Ais a1, Ais a2) => S(Abs(zipWith (ret o opi) a1 a2))
  | (Ads a1, Ads a2) => S(Abs(zipWith (ret o opd) a1 a2))
  | (Acs a1, Acs a2) => S(Abs(zipWith (ret o opc) a1 a2))
  | (Ais a1, Is i2) => S(Abs(each (fn x => ret(opi(x,i2)))a1))
  | (Ads a1, Ds d2) => S(Abs(each (fn x => ret(opd(x,d2)))a1))
  | (Acs a1, Cs c2) => S(Abs(each (fn x => ret(opc(x,c2)))a1))
  | (Is i1, Ais a2) => S(Abs(each (fn x => ret(opi(i1,x)))a2))
  | (Ds d1, Ads a2) => S(Abs(each (fn x => ret(opd(d1,x)))a2))
  | (Cs c1, Acs a2) => S(Abs(each (fn x => ret(opc(c1,x)))a2))
  | (Bs b1, e2) => compCmp opi opd opc (Is(b2i b1),e2)
  | (e1, Bs b2) => compCmp opi opd opc (e1,Is(b2i b2))
  | (Is i1, e2) => compCmp opi opd opc (Ds(i2d i1),e2)
  | (e1, Is i2) => compCmp opi opd opc (e1,Ds(i2d i2))
  | (Ais a1, e2) => compCmp opi opd opc (Ads(each (ret o i2d) a1),e2)
  | (e1, Ais a2) => compCmp opi opd opc (e1,Ads(each (ret o i2d) a2))
  | _ => raise Fail "compCmp.function"

fun compCmp' opi opd opb opc =
 fn (Bs b1, Bs b2) => S(Bs(opb(b1,b2)))
  | (Abs bs1, Abs bs2) => S(Abs(zipWith (ret o opb) bs1 bs2))
  | (Abs bs1, Bs b2) => S(Abs(each (fn x => ret(opb(x,b2)))bs1))
  | (Bs b1, Abs bs2) => S(Abs(each (fn x => ret(opb(b1,x)))bs2))
  | p => compCmp opi opd opc p

fun compOpr1 opi opd =
 fn Is i => S(Is(opi i))
  | Ds d => S(Ds(opd d))
  | Bs b => compOpr1 opi opd (Is(b2i b))
  | Ais a => S(Ais(each (ret o opi) a))
  | Ads a => S(Ads(each (ret o opd) a))
  | Abs a => compOpr1 opi opd (Ais(each (ret o b2i) a))
  | _ => raise Fail "compOpr1.function"

fun compOpr1d opd =
 fn Is i => S(Ds(opd(i2d i)))
  | Ds d => S(Ds(opd d))
  | Bs b => compOpr1d opd (Is(b2i b))
  | Ais a => S(Ads(each (ret o opd o i2d) a))
  | Ads a => S(Ads(each (ret o opd) a))
  | Abs a => compOpr1d opd (Ais(each (ret o b2i) a))
  | _ => raise Fail "compOpr1d.function"

fun compOpr2d opd =
 fn (Ds d1,Ds d2) => S(Ds(opd(d1,d2)))
  | (Ads a1,Ads a2) => S(Ads(zipWith (ret o opd) a1 a2))
  | (Ads a1,Ds d2) => S(Ads(each (ret o (fn x => opd(x,d2))) a1))
  | (Ds d1,Ads a2) => S(Ads(each (ret o (fn x => opd(d1,x))) a2))
  | (Is i1,a2) => compOpr2d opd (Ds(i2d i1),a2)
  | (a1,Is i2) => compOpr2d opd (a1,Ds(i2d i2))
  | (Bs b1,a2) => compOpr2d opd (Is(b2i b1),a2)
  | (a1,Bs b2) => compOpr2d opd (a1,Is(b2i b2))
  | (Ais a1,a2) => compOpr2d opd (Ads(each (ret o i2d) a1),a2)
  | (a1,Ais a2) => compOpr2d opd (a1,Ads(each (ret o i2d) a2))
  | (Abs a1,a2) => compOpr2d opd (Ads(each (ret o i2d o b2i) a1),a2)
  | (a1,Abs a2) => compOpr2d opd (a1,Ads(each (ret o i2d o b2i) a2))
  | _ => raise Fail "compOpr2d.function"

fun compOpr1i opi opd =
 fn Is i => S(Is(opi i))
  | Ds d => S(Is(opd d))
  | Bs b => compOpr1i opi opd (Is(b2i b))
  | Ais a => S(Ais(each (ret o opi) a))
  | Ads a => S(Ais(each (ret o opd) a))
  | Abs a => compOpr1i opi opd (Ais(each (ret o b2i) a))
  | _ => raise Fail "compOpr1i.function"

fun compOpr1b opb =
 fn Bs b => S(Bs(opb b))
  | Abs a => S(Abs(each (ret o opb) a))
  | _ => raise Fail "compOpr1b.function"

fun signi x = If(lti(x,I 0),I ~1, I 1)
fun signd x = If(ltd(x,D 0.0),I ~1, I 1)


fun compErr r msg =
    raise Fail ("Compile Error: " ^ Region.pp r ^ ".\n  " ^ msg ^ ".")

fun circularOp r x y = 
    case Exp.T.unS (Exp.typeOf x) of
        NONE => compErr r ("Incorrect type (" ^ Exp.T.prType (Exp.typeOf x) ^
                           ") of left-argument to circular-operator.")
      | SOME (_,rnk) => 
          (case Exp.T.unRnk rnk of
              NONE => compErr r "c"
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
            | SOME xi => compErr r ("Unsupported left-argument (" ^ Int.toString xi ^
                                    ") to circular-operator."))

fun failWrap r f x =
    f x handle Fail s => raise Fail (s ^ " at " ^ Region.pp r)

datatype classifier = BOOL_C | INT_C | DOUBLE_C | UNKNOWN_C
local
  fun class v =
      case v of
          Is _ => INT_C
        | Bs _ => BOOL_C
        | Ds _ => DOUBLE_C
        | Abs _ => BOOL_C
        | Ais _ => INT_C
        | Ads _ => DOUBLE_C
        | _ => UNKNOWN_C
  fun classify (l: s list) (f: s list -> s N) : classifier =
      case f l of
          S v => class v
        | M m => case runHack m of
                     SOME v => class v
                   | NONE => UNKNOWN_C
in
val dummyIntS = Is (I 0)
val dummyBoolS = Bs (B false)
val dummyDoubleS = Ds (D 0.0)
val classifyReduce : (s list -> s N) -> classifier = classify [dummyBoolS,dummyBoolS]
val classifyEach : s -> (s list -> s N) -> classifier = fn x => classify [x]
val classifyPower : (s list -> s N) -> classifier = classify [Abs(zilde())]
end

fun compSlash r =
    fn [Fs (f,ii)] =>
       rett(Fs (fn [Ads x] => S(reduce (fn (x,y) =>
                                           subM(f[Ds x,Ds y] >>>= (fn Ds z => rett z
                                                                  | _ => compErr r "expecting double as result of reduce")))
                                       (D(id_item_double ii)) x Ds Ads)
                 | [Ais x] => S(reduce (fn (x,y) =>
                                           subM(f[Is x,Is y] >>>= (fn Is z => rett z
                                                                  | _ => compErr r "expecting integer as result of reduce")))
                                       (I(id_item_int ii)) x Is Ais) 
                 | [Abs x] =>
                   (case classifyReduce f of
                        INT_C =>
                        S(let val x = each (ret o b2i) x
                          in reduce (fn (x,y) =>
                                        subM(f[Is x,Is y] >>>= (fn Is z => rett z
                                                               | _ => compErr r "expecting int as result of reduce")))
                                    (I(id_item_int ii)) x Is Ais
                          end)
                      | BOOL_C => 
                        S(reduce (fn (x,y) =>
                                     subM(f[Bs x,Bs y] >>>= (fn Bs z => rett z
                                                            | _ => compErr r "expecting boolean as result of reduce")))
                                 (B(id_item_bool ii)) x Bs Abs)
                      | _ => compErr r "expecting boolean or integer as result of reduce")
                 | [Ds x] => S(Ds x)
                 | [Is x] => S(Is x)
                 | [Cs x] => compErr r "char not supported as right argument to reduce"
                 | [Acs x] => compErr r "char array not supported as right argument to reduce"
                 | _ => compErr r "expecting array as right argument to reduce",
                noii))
  | [Abs x] => rett(Fs (fn [Ais y] => S(Ais(compress x y))
                         | [Is y] => S(Ais(compress x (scalar y)))
                         | [Abs y] => S(Abs(compress x y))
                         | [Bs y] => S(Abs(compress x (scalar y))) 
                         | [Ads y] => S(Ads(compress x y))
                         | [Ds y] => S(Ads(compress x (scalar y))) 
                         | [Acs y] => S(Acs(compress x y))
                         | [Cs y] => S(Acs(compress x (scalar y))) 
                         | _ => compErr r "compress does not support function arguments as right argument", 
                        noii))
  | [Ais x] => rett(Fs (fn [Ais y] => S(Ais(replicate (I 0) x y))
                         | [Is y] => S(Ais(replicate (I 0) x (scalar y)))
                         | [Abs y] => S(Abs(replicate (B false) x y))
                         | [Bs y] => S(Abs(replicate (B false) x (scalar y)))
                         | [Ads y] => S(Ads(replicate (D 0.0) x y))
                         | [Ds y] => S(Ads(replicate (D 0.0) x (scalar y)))
                         | [Acs y] => S(Acs(replicate (C 0w32) x y))
                         | [Cs y] => S(Acs(replicate (C 0w32) x (scalar y)))
                         | _ => compErr r "replicate does not support function arguments as right argument", 
                        noii))
  | [Ads _] => compErr r "replicate does not support double arrays as left argument"
  | [Ds _] => compErr r "replicate does not support double scalars as left argument"
  | [Is x] => compSlash r ([Ais(scalar x)])
  | [Bs x] => compSlash r ([Abs(scalar x)])
  | _ => compErr r "operator slash (reduce/replicate/compress) takes only one argument (monadic)"

fun compileAst flags G0 e =
    let fun comp (G:env) e (k: s*env -> s N) : s N =
            case e of
              IntE (s,r) =>
              (case StoI s of
                   SOME 0 => k (Bs(B false),emp)
                 | SOME 1 => k (Bs(B true),emp)
                 | SOME i => k (Is(I i),emp) 
                 | NONE => compErr r ("Expecting integer, got " ^ s))
            | StrE ([w],r) => k (Cs(C w),emp)
            | StrE (ws,r) => k (Acs(vec(fromChars ws)),emp)
            | DoubleE (s,r) =>
              (case StoD s of
                 SOME d => k (Ds(D d),emp)
               | NONE => compErr r ("Expecting double, got " ^ s))
            | AssignE(v,e,_) => 
              let fun cont f x = 
                      let val t = f x
                      in k(t,[(Var v,t)])
                      end
                  fun pp p a = if v = "$Quad" then p a else a
                  fun contA p f a = M(letm (pp p a)) >>>= cont f
                  fun contS p f a = M(lett (pp p a)) >>>= cont f
              in comp G e (fn (Ais a,_) => contA prArrI Ais a
                          | (Ads a,_) => contA prArrD Ads a
                          | (Abs a,_) => contA prArrB Abs a
                          | (Acs a,_) => contA prArrC Acs a
                          | (Is a,_) => contS prSclI Is a
                          | (Ds a,_) => contS prSclD Ds a
                          | (Bs a,_) => contS prSclB Bs a
                          | (Cs a,_) => contS prSclC Cs a
                          | (s,_) => k(s,[(Var v,s)]))
              end
            | SeqE ([],r) => compErr r "empty sequence"
            | SeqE ([e],_) => comp G e k
            | SeqE (e1::es,r) =>
              comp G e1 (fn (s1,G1) =>
              comp (G++G1) (SeqE(es,r)) (fn (s2,G2) =>
              k(s2,G1++G2)))
            | LambE((2,2),e,r) => (* dyadic operator => dyadic function *)
              k(Fs (fn [f,g] => compLam22 G e (f,g)
                     | _ => compErr r "dyadic operator (of class (2,2)) expecting 2 operator arguments",
                    noii),
                emp)
            | LambE((2,1),e,r) => (* dyadic operator => monadic function *)
              k(Fs (fn [f,g] => compLam21 G e (f,g)
                     | _ => compErr r "dyadic operator (of class (2,1)) expecting 2 operator arguments",
                    noii),
                emp)
            | LambE((1,1),e,r) => (* monadic operator => monadic function *)
              k(Fs (fn [f] => compLam11 G e f
                     | _ => compErr r "monadic operator (of class (1,1)) expecting 1 operator argument",
                    noii),
                emp)
            | LambE((1,2),e,r) => (* monadic operator => dyadic function *)
              k(Fs (fn [f] => compLam12 G e f
                     | _ => compErr r "monadic operator (of class (1,2)) expecting 1 operator argument",
                    noii),
                emp)
            | LambE((0,1),e,r) =>
              k(Fs (fn [x] => M(lets x) >>>= compLam01 G e
                     | l => compErr r ("monadic function expecting one argument; received " ^ Int.toString(List.length l) ^ " arguments"),
                    noii),
                emp)
            | LambE((0,2),e,r) =>
              k(Fs (fn [x,y] => compLam02 G e (x,y)
                     | l => compErr r ("dyadic function expecting two arguments; received " ^ Int.toString(List.length l) ^ " arguments"),
                    noii),
                emp)
            | LambE((0,0),e,r) => comp G (LambE((0,1),e,r)) k   (* support for constant functions *)
            | LambE((x,y),e,r) =>
              compErr r ("function or operator of class (" ^ Int.toString x ^ "," ^ Int.toString y ^ ") not supported")
            | IdE(Var v,r) => compId G (Var v,r) k
            | IdE(Symb L.Omega,r) => compId G (Symb L.Omega,r) k
            | IdE(Symb L.Omegaomega,r) => compId G (Symb L.Omegaomega,r) k
            | IdE(Symb L.Alpha,r) => compId G (Symb L.Alpha,r) k
            | IdE(Symb L.Alphaalpha,r) => compId G (Symb L.Alphaalpha,r) k
            | IdE(Symb L.Zilde,_) => k (Ais (zilde ()),emp)
            | VecE(es,r) =>
              comps G (rev es) (fn (nil,G1) => k (Ais (zilde ()),G1)
                                 | ([s],_) => compErr r "singleton vectors should not appear" 
                                 | (ss,G1) => 
                                   let val ss = rev ss
                                       fun vec' t ss = vec (fromList t ss)
                                   in if List.exists (fn Ds _ => true | _ => false) ss then
                                        k(Ads(vec' Double (List.map (fn Is e => i2d e
                                                                      | Ds d => d
                                                                      | Bs b => i2d(b2i b)
                                                                      | _ => compErr r ("nested vectors not supported")) ss)),G1)
                                      else if List.exists (fn Is _ => true | _ => false) ss then
                                        k(Ais(vec' Int (List.map (fn Is e => e
                                                                 | Ds d => compErr r ("vec compilation: impossible")
                                                                 | Bs b => b2i b
                                                                 | _ => compErr r ("nested vectors not supported")) ss)),G1)
                                      else if List.all (fn Bs _ => true | _ => false) ss then
                                        k(Abs(vec' Bool (List.map (fn Bs b => b | _ => compErr r ("vec compilation: impossible (bool)")) ss)),G1)
                                      else compErr r ("nested vectors not supported")
                                   end)
            | App1E(e0,e1,r) =>
              comp G e1 (fn (s,G') =>
              comp (G++G') e0 (fn (f,G'') =>
                                  case f of
                                    Fs (f,_) => f [s] >>>= (fn s' => k(s',G'++G''))
                                  | _ => compErr r "expecting monadic function"))
            | App2E(e0,e1,e2,r) =>
              comp G e2 (fn (s2,G2) =>
              comp (G++G2) e0 (fn (f,G0) =>
              comp (G++G2++G0) e1 (fn (s1,G1) =>
                                      case f of
                                        Fs (f,_) => f[s1,s2] >>>= (fn s' => k(s',G2++G0++G1))
                                      | _ => compErr r "expecting dyadic function")))
            | AppOpr1E(_,e0,e1,r) =>
              comp G e1 (fn (s,G') =>
              comp (G++G') e0 (fn (f,G'') =>
                                  case f of
                                    Fs (f,_) => f [s] >>>= (fn s' => k(s',G'++G''))
                                  | _ => compErr r "expecting monadic operator"))
            | AppOpr2E(v,IdE(Symb L.Dot,r1),IdE(Symb L.Ring,r2),e1,r) =>
              comp G (AppOpr1E(v,IdE(Var "$out",Region.plus "out" r2 r1),e1,r)) k
            | AppOpr2E(_,e0,e1,e2,r) =>
              comp G e2 (fn (s2,G2) =>
              comp (G++G2) e1 (fn (s1,G1) =>
              comp (G++G2++G1) e0 (fn (f,G0) =>
                                      case f of
                                        Fs (f,_) => f [s1,s2] >>>= (fn s => k(s,G2++G1++G0))
                                      | _ => compErr r "expecting dyadic operator")))
            | IdE(Symb L.StarDia,r) => 
              k(Fs (fn [Fs (f,ii),n] =>
                       let val n = case n of Is n => n
                                           | Bs b => b2i b
                                           | _ => compErr r "power operation expects an integer or a boolean as its second argument"
                       in rett(Fs (compPower r f n, noii))
                       end
                     | _ => compErr r "power operation expects a function as its first argument",
                    noii), 
                emp)
            | IdE(Symb L.Slash,r) => k (Fs (compSlash r, noii), emp)
            | IdE(Symb L.Slashbar,r) => compId G (Var "$slashbar",r) k
            | IdE(Symb L.Dot,r) => compId G (Var "$dot",r) k
            | IdE(Symb L.Each,r) => 
              k(Fs (fn [Fs (f,_)] =>
                       let exception No
                           fun tryInt g x =
                               each (fn x => subM(f[g x] >>>= (fn Is v => rett v
                                                                | _ => raise No))) x
                           fun tryDouble g x =
                               each (fn x => subM(f[g x] >>>= (fn Ds v => rett v
                                                                | _ => raise compErr r "problem with each operator - function is perhaps not returning a scalar"))) x
                       in rett(Fs (fn [Ais x] => (S(Ais(tryInt Is x)) handle No => S(Ads(tryDouble Is x)))
                                    | [Ads x] => (S(Ais(tryInt Ds x)) handle No => S(Ads(tryDouble Ds x)))
                                    | _ => compErr r "expecting array as right argument to each operation",
                                   noii))
                       end
                     | _ => compErr r "expecting function as left argument to each operation",
                    noii), 
                emp)
            | IdE(Symb L.Iota,r) => compPrimFunM k r (fn Is i => S(Ais(iota i))
                                                       | Ais a => S(Ais(iota' a))
                                                       | _ => compErr r "expecting integer argument to iota")
            | IdE(Symb L.Trans,r) => compPrimFunMD k r (fn Ais a => S(Ais(transpose a))
                                                         | Ads a => S(Ads(transpose a))
                                                         | Abs a => S(Abs(transpose a))
                                                         | Acs a => S(Acs(transpose a))
                                                         | Is a => S(Is a)
                                                         | Bs a => S(Bs a)
                                                         | Ds a => S(Ds a)
                                                         | Cs a => S(Cs a)
                                                         | _ => compErr r "expecting array as right argument to transpose",
                                                        fn (Ais a1, Ais a2) => S(Ais(transpose2 (rav0 a1) a2))
                                                         | (Ais a1, Ads a2) => S(Ads(transpose2 (rav0 a1) a2))
                                                         | (Ais a1, Abs a2) => S(Abs(transpose2 (rav0 a1) a2))
                                                         | (Ais a1, Acs a2) => S(Acs(transpose2 (rav0 a1) a2))
                                                         | (_, Is a) => S(Is a)
                                                         | (_, Bs a) => S(Bs a)
                                                         | (_, Ds a) => S(Ds a)
                                                         | (_, Cs a) => S(Cs a)
                                                         | _ => compErr r "expecting arrays as arguments to dyadic transpose") noii
            | IdE(Symb L.Rho,r) => 
              let val rec compDyn = 
                   fn (Ais a1, Ais a2) => S(Ais(reshape (rav0 a1) a2))
                    | (Ais a1, Ads a2) => S(Ads(reshape (rav0 a1) a2))
                    | (Ais a1, Abs a2) => S(Abs(reshape (rav0 a1) a2))
                    | (Ais a1, Acs a2) => S(Acs(reshape (rav0 a1) a2))
                    | (Ais a1, Is a2) => S(Ais(reshape (rav0 a1) (scalar a2)))
                    | (Ais a1, Ds a2) => S(Ads(reshape (rav0 a1) (scalar a2)))
                    | (Ais a1, Bs a2) => S(Abs(reshape (rav0 a1) (scalar a2)))
                    | (Ais a1, Cs a2) => S(Acs(reshape (rav0 a1) (scalar a2)))
                    | (Abs a1, e2) => compDyn(Ais(each (ret o b2i) a1),e2)
                    | (Is i1, e2) => compDyn(Ais(scalar i1),e2)
                    | (Bs b1, e2) => compDyn(Is(b2i b1),e2)
                    | (Ds _,_) => compErr r "left argument to reshape operation cannot be a double"
                    | (Ads _,_) => compErr r "left argument to reshape operation cannot be an array of type double"
                    | (Cs _,_) => compErr r "left argument to reshape operation cannot be a char"
                    | (Acs _,_) => compErr r "left argument to reshape operation cannot be an array of type char"
                    | _ => compErr r "expecting arrays as left and right arguments to reshape operation"
              in compPrimFunMD k r (fn Ais a => S(Ais(vec(shape a)))
                                     | Ads a => S(Ais(vec(shape a)))
                                     | Abs a => S(Ais(vec(shape a)))
                                     | Acs a => S(Ais(vec(shape a)))
                                     | Is _ => S(Ais(zilde()))
                                     | Ds _ => S(Ais(zilde()))
                                     | Bs _ => S(Ais(zilde()))
                                     | Cs _ => S(Ais(zilde()))
                                     | _ => compErr r "expecting array as right argument to shape operation",
                                    compDyn) noii
              end
            | IdE(Symb L.Cat,r) => compPrimFunMD k r (fn Ais a => S(Ais(rav a))
                                                       | Ads a => S(Ads(rav a))
                                                       | Abs a => S(Abs(rav a))
                                                       | Acs a => S(Acs(rav a))
                                                       | _ => compErr r "expecting array as right argument to ravel operation",
                                                      compCat catenate catenate catenate catenate) noii
            | IdE(Symb L.Vcat,r) => compPrimFunMD k r (fn Ais a => S(Ais(rav a))
                                                        | Ads a => S(Ads(rav a))
                                                        | Abs a => S(Abs(rav a))
                                                        | Acs a => S(Acs(rav a))
                                                        | _ => compErr r "expecting array as right argument to ravel operation",
                                                       compCat catenate_first catenate_first catenate_first catenate_first) noii
            | IdE(Symb L.Disclose,r) => compPrimFunM k r (fn Ais a => S(Is(first a))
                                                           | Ads a => S(Ds(first a))
                                                           | Abs a => S(Bs(first a))
                                                           | Acs a => S(Cs(first a))
                                                           | Is a => S(Is a)
                                                           | Ds a => S(Ds a)
                                                           | Bs a => S(Bs a)
                                                           | Cs a => S(Cs a)
                                                           | _ => compErr r "expecting an array or a scalar as right argument to disclose operation")
            | IdE(Symb L.Take,r) => compPrimFunD k r (compOpr2_i8a2a_td e take take take take) noii
            | IdE(Symb L.Drop,r) => compPrimFunD k r (compOpr2_i8a2a_td e drop drop drop drop) noii
            | IdE(Symb L.Rot,r) => compPrimFunMD k r (fn Ais a => S(Ais(reverse a))
                                                       | Ads a => S(Ads(reverse a))
                                                       | Abs a => S(Abs(reverse a))
                                                       | Acs a => S(Acs(reverse a))
                                                       | Is a => S(Is a)
                                                       | Ds a => S(Ds a)
                                                       | Bs a => S(Bs a)
                                                       | Cs a => S(Cs a)
                                                       | _ => compErr r "expecting array as right argument to reverse operation",
                                                      compOpr2_i8a2a e rotate rotate rotate rotate) noii
            | IdE(Symb L.Vrot,r) => compPrimFunMD k r (fn Ais a => S(Ais(vreverse a))
                                                        | Ads a => S(Ads(vreverse a))
                                                        | Abs a => S(Abs(vreverse a))
                                                        | Acs a => S(Acs(vreverse a))
                                                        | Is a => S(Is a)
                                                        | Ds a => S(Ds a)
                                                        | Bs a => S(Bs a)
                                                        | Cs a => S(Cs a)
                                                        | _ => compErr r "expecting array as right argument to reverse-last operation",
                                                      compOpr2_i8a2a e vrotate vrotate vrotate vrotate) noii
            | IdE(Symb L.Add,r) => compPrimFunMD k r (S,
                                                      compOpr2 addi addd) (LRii 0, LRii 0.0, NOii)
            | IdE(Symb L.Sub,r) => compPrimFunMD k r (compOpr1 negi negd,
                                                      compOpr2 subi subd) (Rii 0, Rii 0.0, NOii)
            | IdE(Symb L.Times,r) => compPrimFunMD k r (compOpr1i signi signd,
                                                        compOpr2 muli muld) (LRii 1,LRii 1.0,NOii)
            | IdE(Symb L.Div,r) => compPrimFunMD k r (compOpr1d (fn x => divd(D 1.0,x)),
                                                      compOpr2d divd) (Rii 1,Rii 1.0,NOii)
            | IdE(Symb L.Pow,r) => compPrimFunMD k r (compOpr1d (fn x => powd(D Math.e,x)),
                                                      compOpr2d powd) (Rii 1, Rii 1.0, NOii)
            | IdE(Symb L.Pipe,r) => compPrimFunMD k r (compOpr1 absi absd,
                                                       compOpr2 resi resd) (Lii 0,Lii 0.0,NOii)
            | IdE(Symb L.Max,r) => compPrimFunMD k r (compOpr1i (fn x => x) ceil,
                                                      compOpr2 (uncurry maxi) (uncurry maxd)) (LRii(minInt()), LRii(Real.negInf),NOii)
            | IdE(Symb L.Min,r) => compPrimFunMD k r (compOpr1i (fn x => x) floor,
                                                      compOpr2 (uncurry mini) (uncurry mind)) (LRii(maxInt()), LRii(Real.posInf),NOii)
            | IdE(Symb L.Lt,r) => compPrimFunD k r (compCmp lti ltd ltc) (LRii 0,LRii 0.0,LRii false)
            | IdE(Symb L.Lteq,r) => compPrimFunD k r (compCmp ltei lted ltec) (LRii 1,LRii 1.0,LRii true)
            | IdE(Symb L.Gt,r) => compPrimFunD k r (compCmp gti gtd gtc) (LRii 0,LRii 0.0,LRii false)
            | IdE(Symb L.Gteq,r) => compPrimFunD k r (compCmp gtei gted gtec) (LRii 1,LRii 1.0,LRii true)
            | IdE(Symb L.Eq,r) => compPrimFunD k r (compCmp' eqi eqd eqb eqc) (LRii 1,LRii 1.0,LRii true)
            | IdE(Symb L.Neq,r) => compPrimFunD k r (compCmp' neqi neqd xorb neqc) (LRii 0,LRii 0.0,LRii false)
            | IdE(Symb L.Circstar,r) => compPrimFunM k r (compOpr1d ln)
            | IdE(Symb L.Circ,r) => compPrimFunMD k r (compOpr1d (fn x => muld (x, pi ())),
                                                       compOpr2_i8d2d e (circularOp r)) (NOii,NOii,NOii)
            | IdE(Symb L.Qmark,r) => compPrimFunM k r (fn Is i => S(Ds(roll i))
                                                        | Bs b => S(Ds(roll (b2i b)))
                                                        | Ais a => S(Ads(each (ret o roll) a))
                                                        | Abs a => S(Ads(each (ret o roll o b2i) a))
                                                        | _ => raise Fail "roll.function")
            | IdE(Symb L.And,r) => compPrimFunD k r (compBoolOp andb) (NOii,NOii,LRii true)
            | IdE(Symb L.Or,r) => compPrimFunD k r (compBoolOp orb) (NOii,NOii,LRii false)
            | IdE(Symb L.Nand,r) => compPrimFunD k r (compBoolOp nandb) (NOii,NOii,NOii)
            | IdE(Symb L.Nor,r) => compPrimFunD k r (compBoolOp norb) (NOii,NOii,NOii)
            | IdE(Symb L.Tilde,r) => compPrimFunM k r (compOpr1b notb)
            | e => raise Fail ("compile.expression " ^ pr_exp e ^ " not implemented")
        and comps G nil k = k(nil,emp)
          | comps G (e::es) k = comp G e (fn (s,G1) => comps (G++G1) es (fn (ss,G2) => k(s::ss,G1++G2)))
        and compPrimFunMD k r (mon,dya) ii =
            k(Fs (fn [x1,x2] => failWrap r dya (x1,x2)
                   | [x] => failWrap r mon x
                   | _ => compErr r "function expecting one or two arguments",
                  ii),
              emp)
        and compPrimFunM k r mon =
            k(Fs (fn [x] => failWrap r mon x 
                   | _ => compErr r "monadic function expecting one argument",
                  noii),
              emp) 
        and compPrimFunD k r dya ii =
            k(Fs (fn [x1,x2] => failWrap r dya (x1,x2)
                   | _ => compErr r "dyadic function expecting two arguments",
                  ii),
              emp)
        and compId G (id,r) k =
            case compIdOpt G (id,r) k of
                SOME r => r
              | NONE => 
                let val id = AplAst.pr_id id
                    fun consider id = if List.exists (fn x => id = x) ["$dot","$out","$slashbar"] then 
                                        ". Consider including the prelude.apl file"
                                      else ""
                in compErr r ("identifier " ^ id ^ " not in the environment" ^ consider id)
                end
        and compIdOpt G (id,r) k =
            case lookup G id of
                SOME x => SOME(k(x,emp))
              | NONE => NONE
        and compLam11 G e f =
            rett(Fs(fn [x] =>
                       let val G' = [(Symb L.Alphaalpha, f),(Symb L.Omega, x)]
                       in comp (G++G') e (fn (s,_) => rett s)
                       end
                     | _ => raise Fail "compLam11: expecting 1 argument",
                    noii))
        and compLam12 G e f =
            rett(Fs(fn [x,y] =>
                       let val G' = [(Symb L.Alphaalpha, f),(Symb L.Omega, y),(Symb L.Alpha, x)]
                       in comp (G++G') e (fn (s,_) => rett s)
                       end
                     | _ => raise Fail "compLam12: expecting 2 arguments",
                    noii))
        and compLam22 G e (f,g) =
            rett(Fs(fn [x,y] =>
                       let val G' = [(Symb L.Alphaalpha, f),(Symb L.Omegaomega, g),(Symb L.Omega, y),(Symb L.Alpha, x)]
                       in comp (G++G') e (fn (s,_) => rett s)
                       end
                     | _ => raise Fail "compLam22: expecting 2 arguments",
                    noii))
        and compLam21 G e (f,g) =
            rett(Fs(fn [x] =>
                       let val G' = [(Symb L.Alphaalpha, f),(Symb L.Omegaomega, g),(Symb L.Omega, x)]
                       in comp (G++G') e (fn (s,_) => rett s)
                       end
                     | _ => raise Fail "compLam21: expecting 2 arguments",
                    noii))
        and compLam01 G e x =
            let val G' = [(Symb L.Omega,x)]
            in comp (G++G') e (fn (s,_) => rett s)
            end
        and compLam02 G e (x,y) =
            let val G' = [(Symb L.Omega,y),(Symb L.Alpha,x)]
            in comp (G++G') e (fn (s,_) => rett s)
            end
        and compPower r f n =
            fn [Ais m] => S(Ais(power (fn x =>
                                        subM(f[Ais x] >>>= (fn Ais z => rett z
                                                           | _ => compErr r "expecting integer array as result of power")))
                                    n m))
             | [Is m] => S(Is(powerScl (fn x =>
                                        subM(f[Is x] >>>= (fn Is z => rett z
                                                          | _ => compErr r "expecting integer scalar as result of power")))
                                    n m))
             | [Abs m] =>
               (case classifyPower f of
                    INT_C => compPower r f n [Ais(each (ret o b2i) m)]
                  | BOOL_C => S(Abs(power (fn x =>
                                            subM(f[Abs x] >>>= (fn Abs z => rett z
                                                               | _ => compErr r "expecting boolean array as result of power")))
                                    n m))
                  | _ => compErr r "expecting boolean or integer array as result of power")
             | _ => compErr r "expecting boolean or integer array as argument to power"
        val c = comp G0 e (fn (s,_) => rett s)
        val c' = subM c >>= (fn s =>
                                case s of
                                  Is i => ret (i2d i)
                                | Bs b => ret (i2d(b2i b))
                                | Ds d => ret d
                                | _ => raise Fail "expecting scalar double value as the result of a program")
    in runM flags Double c'
    end

fun Fun1Acs2 g s f =
    (s, Fs (fn [Acs x] => rett(g (f x))
           | l => raise Fail ("Compile Error: monadic function " ^ s ^ " expects character vector as argument"),
            noii))

val initialB =
    let val initial =
            [Fun1Acs2 Acs "Quad$ReadFile" readFile,
             Fun1Acs2 Ais "Quad$ReadIntVecFile" readIntVecFile]
        open AplParse
        val initialPE = List.foldl (fn ((id,_),e) => add (id, [fun1]) e) env0 initial
        val initialCE = List.map (fn (id,x) => (Var id,x)) initial
    in (initialPE, initialCE)
    end

end

fun flag_p flags s =
    List.exists (fn p => p = (s,NONE)) flags

fun flag flags s =
    case List.find (fn (s',_) => s' = s) flags of
        SOME (_,SOME v) => SOME v
      | _ => NONE

fun readFile f =
    let val is = TextIO.openIn f
    in let val s = TextIO.inputAll is
       in TextIO.closeIn is;
          s
       end handle ? => (TextIO.closeIn is; raise ?)
    end

fun parseFile flags pe f =
    let val verbose_p = flag_p flags "-v"
        val () = prln ("[Reading file: " ^ f ^ "]")
        val s = readFile f
        val ts = AplLex.lex f s
        fun pr f = if verbose_p then prln(f()) else ()
        val () = pr (fn () => "File lexed:")
        val () = pr (fn () => " " ^ AplLex.pr_tokens (map #1 ts))
        val () = pr (fn () => "Parsing tokens...")
        val (e,pe') = AplParse.parse pe ts
    in pr(fn () => "Parse success:\n " ^ AplAst.pr_exp e);
       (e,pe')
    end

fun parseFiles flags (pe0 : AplParse.env) (fs: string list) : AplAst.exp =
    let val verbose_p = flag_p flags "-v"
        fun mergeExps (NONE,e) = SOME e
          | mergeExps (SOME e0,e) = SOME(AplParse.seq(e0,e))
        fun parseFs pe = 
            fn (nil, NONE) => raise Fail "Expecting at least one file"
             | (nil, SOME e) => e
             | (f::fs, acc) =>
                let val (e,pe') = parseFile flags pe f
                in parseFs (AplParse.plus(pe,pe')) (fs,mergeExps (acc,e))
                end 
    in parseFs pe0 (fs,NONE)
    end

fun compileExp flags G e =
    let val compile_only_p = flag_p flags "-c"
        val verbose_p = flag_p flags "-v"
        val p_tail = flag_p flags "-p_tail"
        val p_types = flag_p flags "-p_types"
        val optlevel = if flag_p flags "-noopt" then 0 else 1
        val outfile = flag flags "-o"
        val p = compileAst {verbose=verbose_p, optlevel=optlevel, prtype=p_types} G e
        val () =
            case outfile of
                SOME ofile => X.outprog p_types ofile p
              | NONE =>
                if p_tail andalso not verbose_p then
                  (print "Resulting program:\n";
                   print (X.pp_prog p_types p);
                   print "\n")
                else ()  (* program already printed! *)
        val () = if compile_only_p then ()
                 else let val () = prln("Evaluating")
                          val v = X.eval p X.Uv
                      in prln("Result is " ^ X.ppV v)
                      end
    in ()
    end

fun errHandler e =
    case e of
        AplParse.ParseErr (l,msg) => prln ("Parse Error at " ^ 
                                           Region.ppLoc l ^ ": \n  " ^ 
                                           msg)
      | Fail s => prln s
      | _ => raise e

fun compileAndRun flags s =
    let val verbose_p = flag_p flags "-v"
        val ts = AplLex.lex "stream" s
        fun pr f = if verbose_p then prln(f()) else ()
        val () = pr (fn () => "Program lexed:")
        val () = pr (fn () => " " ^ AplLex.pr_tokens (map #1 ts))
        val () = pr (fn () => "Parsing tokens...")
        val (e,_) = AplParse.parse (#1 initialB) ts
    in pr(fn () => "Parse success:\n " ^ AplAst.pr_exp e);
       compileExp flags (#2 initialB) e
    end handle ? => errHandler ?


fun compileAndRunFiles flags fs =
    let val (PE,CE) = initialB
        val e = parseFiles flags PE fs
    in compileExp flags CE e
    end handle ? => errHandler ?
end
