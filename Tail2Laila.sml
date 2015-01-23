
(* Tail -> Laila compilation *)

structure Tail2Laila : 

  sig val compile : (string * string option) list -> (unit, Tail.Double Tail.Num) Tail.prog -> Laila.prog
  end = 

struct

fun die s = raise Fail ("Tail2Laila: " ^ s)
fun qq s = "'" ^ s ^ "'"

structure T = Tail
structure E = T.Exp
structure TY = E.T
type exp = E.exp

structure L = Laila


datatype lexp = S of L.t     (*scalars*)
              | A of L.m
              | FN of lexp list -> lexp L.M

infixr $
fun f $ x = f x 

datatype opOpt = SS_S of L.t * L.t -> L.t
               | S_S of L.t -> L.t
               | S_A of L.t -> L.m
               | SA_A of L.t * L.m -> L.m
               | A_A of L.m -> L.m
               | AA_A of L.m * L.m -> L.m
               | AA_AM of L.m * L.m -> L.m L.M
               | VA_AM of int list * L.m -> L.m L.M
               | A_AM of L.m -> L.m L.M
               | S_SM of L.t -> L.t L.M
               | A_SM of L.m -> L.t L.M
               | NOTOP

infix >>=
val op >>= = L.>>=
val ret = L.ret

val classifyOp : string -> opOpt = 
 fn "addi" => SS_S L.addi
  | "subi" => SS_S L.subi
  | "muli" => SS_S L.muli
  | "divi" => SS_S L.divi
  | "resi" => SS_S L.resi
  | "maxi" => SS_S L.maxi
  | "mini" => SS_S L.mini
  | "lti" => SS_S L.lti
  | "ltei" => SS_S L.ltei
  | "gti" => SS_S L.gti
  | "gtei" => SS_S L.gtei
  | "eqi" => SS_S L.eqi
  | "neqi" => SS_S L.neqi
  | "negi" => S_S L.negi 
  | "signi" => S_S L.signi 
  | "addd" => SS_S L.addd
  | "subd" => SS_S L.subd
  | "muld" => SS_S L.muld
  | "divd" => SS_S L.divd
  | "resd" => SS_S L.resd
  | "maxd" => SS_S L.maxd
  | "mind" => SS_S L.mind
  | "ltd" => SS_S L.ltd
  | "lted" => SS_S L.lted
  | "gtd" => SS_S L.gtd
  | "gted" => SS_S L.gted
  | "eqd" => SS_S L.eqd
  | "neqd" => SS_S L.neqd
  | "negd" => S_S L.negd 
  | "signd" => S_S L.signd
  | "eqb" => SS_S L.eqb
  | "neqb" => SS_S (L.notb o L.eqb)
  | "andb" => SS_S L.andb
  | "orb" => SS_S L.orb
  | "xorb" => SS_S L.xorb
  | "nandb" => SS_S (L.notb o L.andb)
  | "norb" => SS_S (L.notb o L.orb)
  | "notb" => S_S L.notb
  | "i2d" => S_S L.i2d
  | "b2i" => S_S L.b2i
  | "iotaV" => S_A L.iota
  | "iota" => S_A L.iota
  | "transp" => A_AM L.transpose
  | "transp2" => VA_AM (fn (v,a) => L.transpose2 v a)
  | "reverse" => A_A L.reverse
  | "vrotateV" => SA_A (fn (i,a) => L.rotate i a)
  | "drop" => SA_A (fn (i,a) => L.drop i a)
  | "dropV" => SA_A (fn (i,a) => L.drop i a)
  | "take" => SA_A (fn (i,a) => L.take i a)
  | "takeV" => SA_A (fn (i,a) => L.take i a)
  | "cat" => AA_AM (fn (a1,a2) => L.catenate a1 a2)
  | "catV" => AA_AM (fn (a1,a2) => L.catenate a1 a2)
  | "compress" => AA_AM L.compress
  | "snoc" => AA_AM (fn (a1,a2) => L.catenate a1 (L.dimincr a2))
  | "cons" => AA_AM (fn (a1,a2) => L.catenate (L.dimincr a1) a2)
  | "shape" => A_A (L.vec o L.shape)
  | "first" => A_SM L.first
  | "prSclI" => S_SM (fn t => L.printf("[](%d)\n",[t]) >>= (fn _ => ret t))
  | "prSclB" => S_SM (fn t => L.printf("[](%d)\n",[t]) >>= (fn _ => ret t))
  | "prSclD" => S_SM (fn t => L.printf("[](%12.g)\n",[t]) >>= (fn _ => ret t))
  | "prArrI" => A_AM (fn a => L.prArr a >>= (fn _ => ret a))
  | "prArrB" => A_AM (fn a => L.prArr a >>= (fn _ => ret a))
  | "prArrD" => A_AM (fn a => L.prArr a >>= (fn _ => ret a))
  | "rav" => A_A L.rav
  | _ => NOTOP

structure FM = StringFinMap
type env = lexp FM.map

fun compP c1 c2 E es k =
    case es of 
        [e1,e2] => c1 E e1 (fn x => c2 E e2 (fn y => k(x,y)))
      | _ => die "compP expects 2 expressions" 

fun compU c E es k =
    case es of 
        [e1] => c E e1 k
      | _ => die "compU expects one expression" 

fun fnExtract e =
    let fun loop e a =
            case e of
                E.Fn(v,_,e,_) => loop e (v::a)
              | _ => (rev a, e)
    in loop e []
    end

fun extendE (nil,nil,E) = E
  | extendE (v::vs,x::xs,E) = extendE (vs,xs,FM.add(v,x,E))
  | extendE _ = die "extendE"

fun mklet lt (A a) = L.letm a >>= (L.ret o A)
  | mklet lt (S s) = L.lett lt s >>= (L.ret o S)
  | mklet _ _ = die "mklet expects array or scalar"

fun ltypeOf t =
    case TY.unArr' t of
        SOME (bt,_) => if TY.isInt bt then L.Int
                       else if TY.isDouble bt then L.Double
                       else if TY.isBool bt then L.Bool
                       else die "ltypeOf.supports only arrays of type int, bool, or double"
      | NONE => die "ltypeOf.supports only arrays"

fun comp (E:env) (e : E.exp) (k: lexp -> lexp L.M) : lexp L.M =
    let val kS = k o S
        val kA = k o A
    in case e of
           E.I i => kS $ L.I i
         | E.D d => kS $ L.D d
         | E.B b => kS $ L.B b
         | E.Var(v,t) => 
           (case FM.lookup E v of
                SOME e => k e
              | NONE => die ("comp: identifier " ^ qq v ^ " not in environment"))
         | E.Vc(es,t) => comps compS E es (fn v => kA $ L.vec $ L.fromList (ltypeOf t) v)
         | E.Let(v,tv,e1,e2,t) => 
           comp E e1 (fn e1 => 
           mklet (ltypeOf tv) e1 >>= (fn x => 
           comp (FM.add(v,x,E)) e2 k))
         | E.Fn(v,tv,e,t) =>
           let val (vs,e) = fnExtract e
           in k $ FN (fn xs => comp (extendE(v::vs,xs,E)) e L.ret)
           end
         | E.Iff(e1,e2,e3,t) => die "comp: Iff not supported" 
         | E.Op("eachV", [f,a], t) => comp_each E f a t k
         | E.Op("each", [f,a], t) => comp_each E f a t k
         | E.Op("reduce", [f,n,a], t) =>
           (compFN E f (fn f =>
            compS E n (fn n' =>
            compA E a (fn a =>
            let val f = fn (x,y) => f [S x,S y] >>= (L.ret o unS "reduce")
            in L.reduce f n' a S A >>= k
            end))))
         | E.Op("zipWith", [f,a1,a2], t) =>
           (compFN E f (fn f =>
            compA E a1 (fn a1 =>
            compA E a2 (fn a2 =>
            let val f = fn (x,y) => f [S x,S y] >>= (L.ret o unS "zipWith")
            in L.zipWith (ltypeOf t) f a1 a2 >>= kA
            end))))
         | E.Op("reshape",[v,a], _) =>
           compA E v (fn v =>
           compA E a (fn a =>
           L.reshape (L.rav0 v) a >>= kA))
         | E.Op("snocV",[a,x], t) =>
           compA E a (fn a =>
           compS E x (fn x =>
           L.catenate a (L.vec(L.fromList(ltypeOf t)[x])) >>= kA))
         | E.Op("snoc",[a,x], t) =>
           compA E a (fn a =>
           compA E x (fn x =>
           L.catenate a (L.dimincr x) >>= kA))
         | E.Op("consV",[x,a], t) =>
           compS E x (fn x =>
           compA E a (fn a =>
           L.catenate (L.vec(L.fromList(ltypeOf t)[x])) a >>= kA))
         | E.Op("zilde",[], t) => kA(L.zilde(ltypeOf t))
         | E.Op(opr,es,_) =>
           (case classifyOp opr of
                SS_S opr => compP compS compS E es (kS o opr)
              | S_S opr => compU compS E es (kS o opr)
              | S_A opr => compU compS E es (kA o opr)
              | A_A opr => compU compA E es (kA o opr)
              | SA_A opr => compP compS compA E es (kA o opr)
              | AA_A opr => compP compA compA E es (kA o opr)
              | A_AM opr => compU compA E es (fn p => opr p >>= kA)
              | S_SM opr => compU compS E es (fn p => opr p >>= kS)
              | A_SM opr => compU compA E es (fn p => opr p >>= kS)
              | AA_AM opr => compP compA compA E es (fn p => opr p >>= kA)
              | VA_AM opr => compP compV compA E es (fn p => opr p >>= kA)
              | NOTOP => die ("comp: operator " ^ qq opr ^ " not supported"))
      | _ => die "comp: expression not supported"
    end
and comp_each E f a t k =
    compFN E f (fn f =>
    compA E a (fn a =>
    let val f = fn x => f [S x] >>= (L.ret o unS "each")
    in k $ A $ L.each (ltypeOf t) f a
    end))
and unS s = fn S s => s | _ => die ("unS: " ^ s)
and compS E e k = comp E e (k o unS "compS")
and compA E (e : E.exp) (k: L.m -> lexp L.M) : lexp L.M =
    comp E e (fn A a => k a | _ => die "compA")
and compV E (e : E.exp) (k: int list -> lexp L.M) : lexp L.M =
    case e of
        E.Vc(xs,_) => k(List.map (fn E.I x => x
                                 | _ => die "compV.expecting immediate integer") xs)
      | _ => die "compV.expecting immediate integer vector" 
and compFN E (e : E.exp) (k: (lexp list -> lexp L.M) -> lexp L.M) : lexp L.M =
    comp E e (fn FN f => k f | _ => die "compFN")
and comps c E nil k = k nil
  | comps c E (e::es) k = 
    comps c E es (fn xs => c E e (fn x => k (x::xs)))
fun compile flags p =
    let val verbose_p = Flags.flag_p flags "-v"
        val optlevel = 2
        val m = comp FM.empty (T.toExp p) L.ret
        val m = m >>= (fn S e => L.ret e
                        | _ => die "compile: result expected to be a scalar")
    in L.runM {verbose=verbose_p,optlevel=optlevel} L.Double m
    end

end
