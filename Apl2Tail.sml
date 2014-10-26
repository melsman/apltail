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

  datatype ty = Ity | Dty | Bty | Aty of ty | FUNty of ty list -> ty | APPty of ty list * ty 

  datatype s =                         (* Terms *)
      Bs of BOOL                       (*   boolean *)
    | Is of INT                        (*   integer *)
    | Ds of DOUBLE                     (*   double *)
    | Abs of mb                        (*   boolean array *)
    | Ais of mi                        (*   integer array *)
    | Ads of md                        (*   double array *)
    | Fs of (s list -> s N) * id_item  (*   function in-lining *)

  fun pp_s s =
      case s of
          Bs _ => "Bs"
        | Is _ => "Is"
        | Ds _ => "Ds"
        | Abs _ => "Abs"
        | Ais _ => "Ais"
        | Ads _ => "Ads"
        | Fs _ => "Fs"

  fun lets s = case s of
                   Bs _ => ret s
                 | Is _ => ret s
                 | Ds _ => ret s
                 | Abs mb => letm Bool mb >>= (fn x => ret(Abs x))
                 | Ais mi => letm Int mi >>= (fn x => ret(Ais x))
                 | Ads md => letm Double md >>= (fn x => ret(Ads x))
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

fun b2i b = If(b, I 1, I 0)

fun compOpr2_i8a2a e opr1 opr2 opr3 =
 fn (Is i1, Ais a2) => S(Ais(opr1 i1 a2))
  | (Is i1, Ads a2) => S(Ads(opr2 i1 a2))
  | (Is i1, Abs a2) => S(Abs(opr3 i1 a2))
(*
  | (Ais a1, Ais a2) => S(Ais(opr1 (pick a1) a2))
  | (Ais a1, Ads a2) => S(Ads(opr2 (pick a1) a2))
*)
  | (Bs b1, e2) => compOpr2_i8a2a e opr1 opr2 opr3 (Is(b2i b1),e2)
  | _ => raise Fail ("compOpr2_i8a2a: expecting integer and array arguments in " ^ pr_exp e)

fun compOpr2_a8a2aM opr1 opr2 opr3 =
 fn (Ais a1, Ais a2) => M(opr1 a1 a2 >>= (fn a => ret(Ais a)))
  | (Ads a1, Ads a2) => M(opr2 a1 a2 >>= (fn a => ret(Ads a)))
  | (Abs a1, Abs a2) => M(opr3 a1 a2 >>= (fn a => ret(Abs a)))
  | (Is i1,e2) => compOpr2_a8a2aM opr1 opr2 opr3 (Ais(scl Int i1),e2)
  | (e1, Is i2) => compOpr2_a8a2aM opr1 opr2 opr3 (e1,Ais(scl Int i2))
  | (Ds d1,e2) => compOpr2_a8a2aM opr1 opr2 opr3 (Ads(scl Double d1),e2)
  | (e1, Ds d2) => compOpr2_a8a2aM opr1 opr2 opr3 (e1,Ads(scl Double d2))
  | (Bs b1,e2) => compOpr2_a8a2aM opr1 opr2 opr3 (Abs(scl Bool b1),e2)
  | (e1, Bs b2) => compOpr2_a8a2aM opr1 opr2 opr3 (e1,Abs(scl Bool b2))
  | (Abs a1, e2) => compOpr2_a8a2aM opr1 opr2 opr3 (Ais(each Bool Int (ret o b2i) a1),e2)
  | (e1, Abs a2) => compOpr2_a8a2aM opr1 opr2 opr3 (e1,Ais(each Bool Int (ret o b2i) a2))
  | (Ais a1, e2) => compOpr2_a8a2aM opr1 opr2 opr3 (Ads(each Int Double (ret o i2d) a1),e2)
  | (e1, Ais a2) => compOpr2_a8a2aM opr1 opr2 opr3 (e1,Ads(each Int Double (ret o i2d) a2))
  | _ => raise Fail "compOpr2_a8a2aM: expecting two similar arrays as arguments"

fun compOpr2 opi opd =
 fn (Is i1, Is i2) => S(Is(opi(i1,i2)))
  | (Ds d1, Ds d2) => S(Ds(opd(d1,d2)))
  | (Ais a1, Ais a2) => M(zipWith Int Int Int (ret o opi) a1 a2 >>= (fn x => ret(Ais x)))
  | (Ads a1, Ads a2) => M(zipWith Double Double Double (ret o opd) a1 a2 >>= (fn x => ret(Ads x)))
  | (Ais a1, Is i2) => S(Ais(each Int Int (fn x => ret(opi(x,i2)))a1))
  | (Ads a1, Ds d2) => S(Ads(each Double Double (fn x => ret(opd(x,d2)))a1))
  | (Is i1, Ais a2) => S(Ais(each Int Int (fn x => ret(opi(i1,x)))a2))
  | (Ds d1, Ads a2) => S(Ads(each Double Double (fn x => ret(opd(d1,x)))a2))
  | (Bs b1, e2) => compOpr2 opi opd (Is(b2i b1),e2)
  | (e1, Bs b2) => compOpr2 opi opd (e1,Is(b2i b2))
  | (Is i1, e2) => compOpr2 opi opd (Ds(i2d i1),e2)
  | (e1, Is i2) => compOpr2 opi opd (e1,Ds(i2d i2))
  | (Abs a1, e2) => compOpr2 opi opd (Ais(each Bool Int (ret o b2i) a1),e2)
  | (e1, Abs a2) => compOpr2 opi opd (e1,Ais(each Bool Int (ret o b2i) a2))
  | (Ais a1, e2) => compOpr2 opi opd (Ads(each Int Double (ret o i2d) a1),e2)
  | (e1, Ais a2) => compOpr2 opi opd (e1,Ads(each Int Double (ret o i2d) a2))
  | _ => raise Fail "compOpr2.function"

fun compCmp opi opd =
 fn (Is i1, Is i2) => S(Bs(opi(i1,i2)))
  | (Ds d1, Ds d2) => S(Bs(opd(d1,d2)))
  | (Ais a1, Ais a2) => M(zipWith Int Int Bool (ret o opi) a1 a2 >>= (fn x => ret(Abs x)))
  | (Ads a1, Ads a2) => M(zipWith Double Double Bool (ret o opd) a1 a2 >>= (fn x => ret(Abs x)))
  | (Ais a1, Is i2) => S(Abs(each Int Bool (fn x => ret(opi(x,i2)))a1))
  | (Ads a1, Ds d2) => S(Abs(each Double Bool (fn x => ret(opd(x,d2)))a1))
  | (Is i1, Ais a2) => S(Abs(each Int Bool (fn x => ret(opi(i1,x)))a2))
  | (Ds d1, Ads a2) => S(Abs(each Double Bool (fn x => ret(opd(d1,x)))a2))
  | (Is i1, e2) => compCmp opi opd (Ds(i2d i1),e2)
  | (e1, Is i2) => compCmp opi opd (e1,Ds(i2d i2))
  | (Ais a1, e2) => compCmp opi opd (Ads(each Int Double (ret o i2d) a1),e2)
  | (e1, Ais a2) => compCmp opi opd (e1,Ads(each Int Double (ret o i2d) a2))
  | _ => raise Fail "compCmp.function"

fun compOpr1 opi opd =
 fn Is i => S(Is(opi i))
  | Ds d => S(Ds(opd d))
  | Bs b => compOpr1 opi opd (Is(b2i b))
  | Ais a => S(Ais(each Int Int (ret o opi) a))
  | Ads a => S(Ads(each Double Double (ret o opd) a))
  | Abs a => compOpr1 opi opd (Ais(each Bool Int (ret o b2i) a))
  | _ => raise Fail "compOpr1.function"

fun compOpr1d opd =
 fn Is i => S(Ds(opd(i2d i)))
  | Ds d => S(Ds(opd d))
  | Bs b => compOpr1d opd (Is(b2i b))
  | Ais a => S(Ads(each Int Double (ret o opd o i2d) a))
  | Ads a => S(Ads(each Double Double (ret o opd) a))
  | Abs a => compOpr1d opd (Ais(each Bool Int (ret o b2i) a))
  | _ => raise Fail "compOpr1d.function"

fun compOpr1i opi opd =
 fn Is i => S(Is(opi i))
  | Ds d => S(Is(opd d))
  | Bs b => compOpr1i opi opd (Is(b2i b))
  | Ais a => S(Ais(each Int Int (ret o opi) a))
  | Ads a => S(Ais(each Double Int (ret o opd) a))
  | Abs a => compOpr1i opi opd (Ais(each Bool Int (ret o b2i) a))
  | _ => raise Fail "compOpr1i.function"

fun signi x = If(lti(x,I 0),I ~1, I 1)
fun signd x = If(ltd(x,D 0.0),I ~1, I 1)

fun compErr r msg =
    raise Fail ("Compile Error: " ^ Region.pp r ^ ".\n  " ^ msg ^ ".")

fun failWrap r f x =
    f x handle Fail s => raise Fail (s ^ " at " ^ Region.pp r)

fun compileAst flags e =
    let fun comp (G:env) e (k: s*env -> s N) : s N =
            case e of
              IntE (s,r) =>
              (case StoI s of
                 SOME i => k (Is(I i),emp)
               | NONE => compErr r ("Expecting integer, got " ^ s))
            | DoubleE (s,r) =>
              (case StoD s of
                 SOME d => k (Ds(D d),emp)
               | NONE => compErr r ("Expecting double, got " ^ s))
            | AssignE(v,e,_) => 
              let fun cont f x = let val t = f x in k(t,[(Var v,t)]) end
              in comp G e (fn (Ais a,_) => M(letm_asgn Int a) >>>= cont Ais
                          | (Ads a,_) => M(letm_asgn Double a) >>>= cont Ads
                          | (Is a,_) => M(lett Int a) >>>= cont Is
                          | (Ds a,_) => M(lett Double a) >>>= cont Ds
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
            | IdE(Symb L.Zilde,_) => k (Ais (zilde Int),emp)
            | VecE(es,r) =>
              (case List.foldr (fn (IntE (s,_),SOME acc) =>
                                   (case StoI s of
                                      SOME i => SOME(I i::acc)
                                    | NONE => NONE)
                                 | _ => NONE) (SOME[]) es of
                 SOME is => (M(fromListM Int is)) >>>= (fn e => k(Ais(vec e),emp))
               | NONE =>
                 case List.foldr (fn (e,acc) =>
                                     let val s = case e of
                                                   IntE (s,_) => s
                                                 | DoubleE (s,_) => s
                                                 | _ => compErr r "expecting only integers or doubles in immediate vectors"
                                     in case StoD s of
                                          SOME d => D d :: acc
                                        | NONE => compErr r ("could not parse double value " ^ s)
                                     end) [] es of
                   [] => compErr r "expecting a non-empty sequence of integers or doubles in immediate vector"
                 | ds => (M(fromListM Double ds)) >>>= (fn e => k(Ads(vec e),emp)))   (*k(Ads(vec(fromList Double ds)),emp))*)
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
            | IdE(Symb L.Slash,r) => 
              k(Fs (fn [Fs (f,ii)] =>
                       rett(Fs (fn [Ais x] => M(reduce Int (fn (x,y) =>
                                                        subM(f[Is x,Is y] >>>= (fn Is z => rett z
                                                                                 | _ => compErr r "expecting integer as result of reduce")))
                                                    (I(id_item_int ii)) x Is Ais)
                                 | [Ads x] => M(reduce Double (fn (x,y) =>
                                                        subM(f[Ds x,Ds y] >>>= (fn Ds z => rett z
                                                                                 | _ => compErr r "expecting double as result of reduce")))
                                                    (D(id_item_double ii)) x Ds Ads)
                                 | [Ds x] => S(Ds x)
                                 | [Is x] => S(Is x)
                                 | _ => compErr r "expecting array as right argument to reduce",
                                noii))
                     | _ => compErr r "expecting function as left argument to reduce",
                    noii), 
                emp)
            | IdE(Symb L.Dot,r) =>
              (case compIdOpt G (Var "$dot",r) k of
                   SOME res => res
                 | NONE =>
                   k(Fs (fn [Fs(f1,ii),Fs(f2,_)] =>
                            rett(Fs (fn [Ais a1, Ais a2] => 
                                        M(prod Int (fn (x,y) =>  
                                                       subM(f1[Is x,Is y] >>>= (fn Is z => rett z
                                                                               | _ => compErr r "expecting int as result of dot operation")))
                                               (fn (x,y) =>  
                                                   subM(f2[Is x,Is y] >>>= (fn Is z => rett z
                                                                           | _ => compErr r "expecting int as result of dot operation")))
                                               (I(id_item_int noii)) a1 a2 Is Ais)
                                    | _ => compErr r "expecting two arrays as arguments to function-specialized dot operator",
                                     noii))
                        | _ => compErr r "expecting two functions as arguments to dot operator",
                         noii),
                     emp)
              )
            | IdE(Symb L.Each,r) => 
              k(Fs (fn [Fs (f,_)] =>
                       let exception No
                           fun tryInt t g x =
                               each t Int (fn x => case f[g x] of S(Is v) => ret v
                                                           | _ => raise No) x
                           fun tryDouble t g x =
                               each t Double (fn x => case f[g x] of S(Ds v) => ret v
                                                          | _ => raise Fail "Not Ds") x
                       in rett(Fs (fn [Ais x] => (S(Ais(tryInt Int Is x)) handle No => S(Ads(tryDouble Int Is x)))
                                    | [Ads x] => (S(Ais(tryInt Double Ds x)) handle No => S(Ads(tryDouble Double Ds x)))
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
                                                         | _ => compErr r "expecting array as right argument to transpose",
                                                        fn (Ais a1, Ais a2) => S(Ais(transpose2 (rav0 a1) a2))
                                                         | (Ais a1, Ads a2) => S(Ads(transpose2 (rav0 a1) a2))
                                                         | (Ais a1, Abs a2) => S(Abs(transpose2 (rav0 a1) a2))
                                                         | _ => compErr r "expecting arrays as arguments to dyadic transpose") noii
            | IdE(Symb L.Rho,r) => compPrimFunMD k r (fn Ais a => S(Ais(vec(shape a)))
                                                       | Ads a => S(Ais(vec(shape a)))
                                                       | Abs a => S(Ais(vec(shape a)))
                                                       | _ => compErr r "expecting array as right argument to shape operation",
                                                      fn (Ais a1, Ais a2) => M(reshape (rav0 a1) a2) >>>= (fn a => rett(Ais a))
                                                       | (Ais a1, Ads a2) => M(reshape (rav0 a1) a2) >>>= (fn a => rett(Ads a))
                                                       | (Ais a1, Abs a2) => M(reshape (rav0 a1) a2) >>>= (fn a => rett(Abs a))
                                                       | (Ais a1, Is a2) => M(reshape (rav0 a1) (scalar a2)) >>>= (fn a => rett(Ais a)) 
                                                       | (Ais a1, Ds a2) => M(reshape (rav0 a1) (scalar a2)) >>>= (fn a => rett(Ads a)) 
                                                       | (Ais a1, Bs a2) => M(reshape (rav0 a1) (scalar a2)) >>>= (fn a => rett(Abs a)) 
                                                       | _ => compErr r "expecting arrays as left and right arguments to reshape operation") noii
            | IdE(Symb L.Cat,r) => compPrimFunMD k r (fn Ais a => S(Ais(rav a))
                                                       | Ads a => S(Ads(rav a))
                                                       | Abs a => S(Abs(rav a))
                                                       | _ => compErr r "expecting array as right argument to ravel operation",
                                                      compOpr2_a8a2aM catenate catenate catenate) noii
            | IdE(Symb L.Vcat,r) => compPrimFunMD k r (fn Ais a => S(Ais(rav a))
                                                        | Ads a => S(Ads(rav a))
                                                        | Abs a => S(Abs(rav a))
                                                        | _ => compErr r "expecting array as right argument to ravel operation",
                                                       compOpr2_a8a2aM catenate_first catenate_first catenate_first) noii
            | IdE(Symb L.Disclose,r) => compPrimFunM k r (fn Ais a => S(Is(first a))
                                                           | Ads a => S(Ds(first a))
                                                           | Abs a => S(Bs(first a))
                                                           | Is a => S(Is a)
                                                           | Ds a => S(Ds a)
                                                           | Bs a => S(Bs a)
                                                           | _ => compErr r "expecting an array or a scalar as right argument to disclose operation")
            | IdE(Symb L.Take,r) => compPrimFunD k r (compOpr2_i8a2a e take take take) noii
            | IdE(Symb L.Drop,r) => compPrimFunD k r (compOpr2_i8a2a e drop drop drop) noii
            | IdE(Symb L.Rot,r) => compPrimFunMD k r (fn Ais a => S(Ais(reverse a))
                                                       | Ads a => S(Ads(reverse a))
                                                       | Abs a => S(Abs(reverse a))
                                                       | _ => compErr r "expecting array as right argument to reverse operation",
                                                      compOpr2_i8a2a e rotate rotate rotate) noii
            | IdE(Symb L.Add,r) => compPrimFunMD k r (S,
                                                      compOpr2 addi addd) noii
            | IdE(Symb L.Sub,r) => compPrimFunMD k r (compOpr1 negi negd,
                                                      compOpr2 subi subd) noii
            | IdE(Symb L.Times,r) => compPrimFunMD k r (compOpr1i signi signd,
                                                        compOpr2 muli muld) (LRii 1,LRii 1.0,NOii)
            | IdE(Symb L.Div,r) => compPrimFunMD k r (compOpr1d (fn x => divd(D 1.0,x)),
                                                      compOpr2 divi divd) (LRii 1,LRii 1.0,NOii)
            | IdE(Symb L.Pipe,r) => compPrimFunMD k r (compOpr1 absi absd,
                                                       compOpr2 resi resd) (Lii 0,Lii 0.0,Lii false)
            | IdE(Symb L.Max,r) => compPrimFunD k r (compOpr2 (uncurry maxi) (uncurry maxd)) (LRii(minInt()), LRii(Real.negInf),NOii)
            | IdE(Symb L.Min,r) => compPrimFunD k r (compOpr2 (uncurry mini) (uncurry mind)) (LRii(maxInt()), LRii(Real.posInf),NOii)
            | IdE(Symb L.Lt,r) => compPrimFunD k r (compCmp lti ltd) (NOii,NOii,NOii)
            | e => raise Fail ("compile.expression " ^ pr_exp e ^ " not implemented")
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
                    fun consider id = if id = "$out" then ". Consider including the prelude.apl file"
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
                       let val G' = [(Symb L.Alphaalpha, f),(Symb L.Omega, x),(Symb L.Alpha, y)]
                       in comp (G++G') e (fn (s,_) => rett s)
                       end
                     | _ => raise Fail "compLam12: expecting 2 arguments",
                    noii))
        and compLam22 G e (f,g) =
            rett(Fs(fn [x,y] =>
                       let val G' = [(Symb L.Alphaalpha, f),(Symb L.Omegaomega, g),(Symb L.Omega, y),(Symb L.Alpha, x)]
                       in comp (G++G') e (fn (s,_) => rett s)
                       end
                     | _ => raise Fail "compLam12: expecting 2 arguments",
                    noii))
        and compLam01 G e x =
            let val G' = [(Symb L.Omega,x)]
            in comp (G++G') e (fn (s,_) => rett s)
            end
        and compLam02 G e (x,y) =
            let val G' = [(Symb L.Omega,y),(Symb L.Alpha,x)]
            in comp (G++G') e (fn (s,_) => rett s)
            end
        val c = comp emp e (fn (s,_) => rett s)
        val c' = subM c >>= (fn s =>
                                case s of
                                  Ais im => red Int Int (ret o addi) (I 0) im >>= (fn x => ret (i2d x))
                                | Ads dm => red Double Double (ret o addd) (D 0.0) dm
                                | Is i => ret (i2d i)
                                | Ds d => ret d
                                | _ => raise Fail "expecting array")
    in runM flags Double c'
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

fun compileExp flags e =
    let val compile_only_p = flag_p flags "-c"
        val verbose_p = flag_p flags "-v"
        val p_tail = flag_p flags "-p_tail"
        val p_types = flag_p flags "-p_types"
        val optlevel = if flag_p flags "-noopt" then 0 else 1
        val outfile = flag flags "-o"
        val p = compileAst {verbose=verbose_p, optlevel=optlevel, prtype=p_types} e
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
        val (e,_) = AplParse.parse AplParse.env0 ts
    in pr(fn () => "Parse success:\n " ^ AplAst.pr_exp e);
       compileExp flags e
    end handle ? => errHandler ?

fun compileAndRunFiles flags fs =
    let val e = parseFiles flags AplParse.env0 fs
    in compileExp flags e
    end handle ? => errHandler ?
end
