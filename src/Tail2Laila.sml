
(* Tail -> Laila compilation *)

structure Tail2Laila : 

  sig val compile : Flags.flags -> (unit, Tail.Double Tail.Num) Tail.prog -> Laila.prog
  end = 

struct

fun die s = raise Fail ("Tail2Laila: " ^ s)
fun qq s = "'" ^ s ^ "'"

(* structure T = Tail *)
structure TailExp = Tail.Exp
structure TailType = Tail.Exp.T
type exp = Tail.Exp.uexp

(* structure L = Laila *)


datatype lexp = S of Laila.t     (*scalars*)
              | A of Laila.m
              | MA of Laila.mm
              | FN of lexp list -> lexp Laila.M

infixr $
val op $ = Util.$

datatype opOpt = SS_S of Laila.t * Laila.t -> Laila.t
               | S_S of Laila.t -> Laila.t
               | S_A of Laila.t -> Laila.m
               | SA_A of Laila.t * Laila.m -> Laila.m
               | A_A of Laila.m -> Laila.m
               | AA_A of Laila.m * Laila.m -> Laila.m
               | AA_AM of Laila.m * Laila.m -> Laila.m Laila.M
               | SA_AM of Laila.t * Laila.m -> Laila.m Laila.M
               | VA_AM of int list * Laila.m -> Laila.m Laila.M
               | A_AM of Laila.m -> Laila.m Laila.M
               | S_SM of Laila.t -> Laila.t Laila.M
               | S_AM of Laila.t -> Laila.m Laila.M
               | A_SM of Laila.m -> Laila.t Laila.M
               | NOTOP

infix >>=
val op >>= = Laila.>>=
val ret = Laila.ret

val classifyOp : string -> opOpt = 
 fn "addi" => SS_S Laila.addi
  | "subi" => SS_S Laila.subi
  | "muli" => SS_S Laila.muli
  | "divi" => SS_S Laila.divi
  | "resi" => SS_S Laila.resi
  | "maxi" => SS_S Laila.maxi
  | "mini" => SS_S Laila.mini
  | "lti" => SS_S Laila.lti
  | "ltei" => SS_S Laila.ltei
  | "gti" => SS_S Laila.gti
  | "gtei" => SS_S Laila.gtei
  | "eqi" => SS_S Laila.eqi
  | "neqi" => SS_S Laila.neqi
  | "negi" => S_S Laila.negi 
  | "nowi" => S_S Laila.nowi 
  | "signi" => S_S Laila.signi 
  | "absi" => S_S Laila.absi 
  | "ori" => SS_S Laila.ori
  | "andi" => SS_S Laila.andi
  | "xori" => SS_S Laila.xori
  | "shli" => SS_S Laila.shli
  | "shri" => SS_S Laila.shri
  | "shari" => SS_S Laila.shari
  | "addd" => SS_S Laila.addd
  | "subd" => SS_S Laila.subd
  | "muld" => SS_S Laila.muld
  | "divd" => SS_S Laila.divd
  | "resd" => SS_S Laila.resd
  | "maxd" => SS_S Laila.maxd
  | "mind" => SS_S Laila.mind
  | "powd" => SS_S Laila.powd
  | "ltd" => SS_S Laila.ltd
  | "lted" => SS_S Laila.lted
  | "gtd" => SS_S Laila.gtd
  | "gted" => SS_S Laila.gted
  | "eqd" => SS_S Laila.eqd
  | "neqd" => SS_S Laila.neqd
  | "negd" => S_S Laila.negd 
  | "signd" => S_S Laila.signd
  | "absd" => S_S Laila.absd
  | "ceil" => S_S Laila.ceil
  | "floor" => S_S Laila.floor
  | "ln" => S_S Laila.ln
  | "sin" => S_S Laila.sin
  | "cos" => S_S Laila.cos
  | "tan" => S_S Laila.tan
  | "expd" => S_S Laila.expd
  | "roll" => S_S Laila.roll
  | "eqc" => SS_S Laila.eqc
  | "eqb" => SS_S Laila.eqb
  | "neqb" => SS_S (Laila.notb o Laila.eqb)
  | "andb" => SS_S Laila.andb
  | "orb" => SS_S Laila.orb
  | "xorb" => SS_S Laila.xorb
  | "nandb" => SS_S (Laila.notb o Laila.andb)
  | "norb" => SS_S (Laila.notb o Laila.orb)
  | "notb" => S_S Laila.notb
  | "i2d" => S_S Laila.i2d
  | "b2i" => S_S Laila.b2i
  | "b2iV" => S_S Laila.b2i
  | "iotaV" => S_A Laila.iota
  | "iota" => S_A Laila.iota
  | "mem" => A_AM Laila.mem
  | "memScl" => S_S (fn x => x)
  | "transp" => A_AM Laila.transpose
  | "transp2" => VA_AM (fn (v,a) => Laila.transpose2 v a)
  | "vrotateV" => SA_A (fn (i,a) => Laila.rotate i a)
  | "vrotate" => SA_AM (fn (i,a) => Laila.vrotate i a)
  | "rotateV" => SA_A (fn (i,a) => Laila.rotate i a)
  | "rotate" => SA_AM (fn (i,a) => Laila.transpose a >>= (fn a => Laila.vrotate i a >>= Laila.transpose))
  | "drop" => SA_AM (fn (i,a) => Laila.drop i a)
  | "dropV" => SA_AM (fn (i,a) => Laila.drop i a)
  | "take" => SA_AM (fn (i,a) => Laila.take i a)
  | "takeV" => SA_AM (fn (i,a) => Laila.take i a)
  | "cat" => AA_AM (fn (a1,a2) => Laila.catenate a1 a2)
  | "catV" => AA_AM (fn (a1,a2) => Laila.catenate a1 a2)
  | "compress" => AA_AM Laila.compress
  | "shape" => A_A Laila.shape
  | "shapeV" => A_A Laila.shape
  | "first" => A_SM Laila.first
  | "firstV" => A_SM Laila.first
  | "vreverseV" => A_AM Laila.vreverse
  | "vreverse" => A_AM Laila.vreverse
  | "prSclI" => S_SM (fn t => Laila.printf("[](%d)\n",[t]) >>= (fn () => ret t))
  | "prSclB" => S_SM (fn t => Laila.printf("[](%d)\n",[t]) >>= (fn () => ret t))
  | "prSclD" => S_SM (fn t => Laila.printf("[](",[]) >>= (fn () =>
                              Laila.printf("%DOUBLE",[t]) >>= (fn () =>
                              Laila.printf(")\n",[]) >>= (fn () => ret t))))
  | "prSclC" => S_SM (fn t => Laila.printf("[](%c)\n",[t]) >>= (fn () => ret t))
  | "prArrI" => A_AM (fn a => Laila.prArr a >>= (fn () => ret a))
  | "prArrB" => A_AM (fn a => Laila.prArr a >>= (fn () => ret a))
  | "prArrD" => A_AM (fn a => Laila.prArr a >>= (fn () => ret a))
  | "prArrC" => A_AM (fn a => Laila.prArr a >>= (fn () => ret a))
  | "formatI" => S_AM (fn t => Laila.sprintf("%d",[t]))
  | "formatD" => S_AM (fn t => Laila.sprintf("%DOUBLE",[t]))
  | "rav" => A_AM Laila.rav
  | "readIntVecFile" => A_AM Laila.readIntVecFile
  | "readDoubleVecFile" => A_AM Laila.readDoubleVecFile
  | _ => NOTOP

structure FM = Tail.Exp.FM

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
                Tail.Exp.Fn(v,_,e,_) => loop e (v::a)
              | _ => (rev a, e)
    in loop e []
    end

fun extendE (nil,nil,E) = E
  | extendE (v::vs,x::xs,E) = extendE (vs,xs,FM.add(v,x,E))
  | extendE _ = die "extendE"

fun mklet mutable (A a) = 
    if mutable then Laila.mk_mm a >>= (Laila.ret o MA)
    else Laila.letm a >>= (Laila.ret o A)
  | mklet _ (S s) = Laila.lett s >>= (Laila.ret o S)
  | mklet _ _ = die "mklet expects array or scalar"

fun ltypeOf t =
    case TailType.unArr' t of
        SOME (bt,_) => if TailType.isInt bt then Laila.Int
                       else if TailType.isDouble bt then Laila.Double
                       else if TailType.isBool bt then Laila.Bool
                       else if TailType.isChar bt then Laila.Char
                       else die "ltypeOf.supports only arrays of type int, bool, char, or double"
      | NONE => die "ltypeOf.supports only arrays"

fun comp (E:env) (e : Tail.Exp.uexp) (k: lexp -> lexp Laila.M) : lexp Laila.M =
    let val kS = k o S
        val kA = k o A
    in case e of
           Tail.Exp.I i => kS $ Laila.I i
         | Tail.Exp.D d => kS $ Laila.D d
         | Tail.Exp.B b => kS $ Laila.B b
         | Tail.Exp.C c => kS $ Laila.C c
         | Tail.Exp.Var(v,t) => 
           (case FM.lookup E v of
                SOME e => k e
              | NONE => die ("comp: identifier " ^ qq (Tail.Exp.ppVar v) ^ " not in environment"))
         | Tail.Exp.Vc(es,t) => comps compS E es (fn vs => Laila.fromListM (ltypeOf t) vs >>= kA)
         | Tail.Exp.Let(v,tv,e1,e2,t) => 
           comp E e1 (fn e1 =>
           mklet (!(Tail.Exp.mutableVar v)) e1 >>= (fn x => 
           comp (FM.add(v,x,E)) e2 k)) 
        | Tail.Exp.Fn(v,tv,e,t) =>
           let val (vs,e) = fnExtract e
           in k $ FN (fn xs => comp (extendE(v::vs,xs,E)) e Laila.ret)
           end
         | Tail.Exp.Iff(e1,e2,e3,t) => die "comp: Iff not supported" 
         | Tail.Exp.Op("eachV", [f,a], t) => comp_each E f a t k
         | Tail.Exp.Op("each", [f,a], t) => comp_each E f a t k
         | Tail.Exp.Op("powerScl", [f,n,a], _) => 
           (compFN E f (fn f =>
            compS E n (fn n =>
            compS E a (fn a =>
            let val f = fn x => f [S x] >>= (Laila.ret o unS "powerScl")
            in Laila.powerScl f n a >>= kS
            end))))                                 
         | Tail.Exp.Op("bench", [f,n,a], t) => 
           (compFN E f (fn f =>
            compS E n (fn n =>
            compS E a (fn a =>
            let val f = fn x => f [S x] >>= (Laila.ret o unS "bench")
                val fmt = if ltypeOf t = Laila.Double then "%f" else "%d"
            in Laila.lett (Laila.nowi (Laila.I 0)) >>= (fn t0 =>
               Laila.powerScl f n a >>= (fn res =>
               Laila.lett (Laila.nowi (Laila.I 1)) >>= (fn t1 =>
               Laila.lett (Laila.subi(t1,t0)) >>= (fn time =>
               Laila.lett (Laila.divd(Laila.i2d time,Laila.i2d n)) >>= (fn avgtime =>
               Laila.printf("ITERATIONS: %d\n", [n]) >>= (fn () =>
               Laila.printf("TIMING: %d\n", [time]) >>= (fn () =>
               Laila.printf("AVGTIMING: %g\n", [avgtime]) >>= (fn () =>
               Laila.printf("RESULT: "^ fmt ^"\n", [res]) >>= (fn () =>
               kS res)))))))))
            end))))                            
         | Tail.Exp.Op("condScl", [f,n,a], _) => 
           (compFN E f (fn f =>
            compS E n (fn n =>
            compS E a (fn a =>
            let val f = fn x => f [S x] >>= (Laila.ret o unS "condScl")
            in Laila.condScl f n a >>= kS
            end))))                                 
         | Tail.Exp.Op("power", [f,n,a], _) => 
           (compFN E f (fn f =>
            compS E n (fn n =>
            compA E a (fn a =>
            let val f = fn x => f [A x] >>= (Laila.ret o unA "power")
            in Laila.power f n a >>= kA
            end))))                                 
         | Tail.Exp.Op("reduce", [f,n,a], t) =>
           (compFN E f (fn f =>
            compS E n (fn n =>
            compA E a (fn a =>
            let val f = fn (x,y) => f [S x,S y] >>= (Laila.ret o unS "reduce")
            in Laila.reduce f n a S A >>= k
            end))))
         | Tail.Exp.Op("replicate", [d,r,a], t) =>
           (compS E d (fn d =>
            compA E r (fn r =>
            compA E a (fn a => Laila.replicate (d, r, a) >>= kA))))
         | Tail.Exp.Op("scan", [f,a], t) =>
           (compFN E f (fn f =>
            compA E a (fn a =>
            let val f = fn (x,y) => f [S x,S y] >>= (Laila.ret o unS "scan")
            in Laila.scan f a >>= kA
            end)))
         | Tail.Exp.Op("zipWith", [f,a1,a2], t) =>
           (compFN E f (fn f =>
            compA E a1 (fn a1 =>
            compA E a2 (fn a2 =>
            let val f = fn (x,y) => f [S x,S y] >>= (Laila.ret o unS "zipWith")
            in Laila.zipWith (ltypeOf t) f a1 a2 >>= kA
            end))))
         | Tail.Exp.Op("reshape",[v,a], _) =>
           compA E v (fn v =>
           compA E a (fn a =>
           Laila.reshape v a >>= kA))
         | Tail.Exp.Op("snocV",[a,x], t) =>
           compA E a (fn a =>
           compS E x (fn x =>
           Laila.catenate a (Laila.enclose x) >>= kA))
         | Tail.Exp.Op("snoc",[a,x], t) =>
           compA E a (fn a =>
           comp E x (fn A x => Laila.catenate a (Laila.dimincr x) >>= kA
                      | S x => Laila.catenate a (Laila.enclose x) >>= kA
                      | _ => die "snoc"))
         | Tail.Exp.Op("consV",[x,a], t) =>
           compS E x (fn x =>
           compA E a (fn a =>
           Laila.catenate (Laila.enclose x) a >>= kA))
         | Tail.Exp.Op("cons",[x,a], t) =>
           compA E a (fn a =>
           comp E x (fn A x => Laila.catenate (Laila.dimincr x) a >>= kA
                      | S x => Laila.catenate (Laila.enclose x) a >>= kA
                      | _ => die "cons"))
         | Tail.Exp.Op("zilde",[], t) => kA(Laila.zilde(ltypeOf t))
         | Tail.Exp.Op("pi",[], t) => kS Laila.pi
         | Tail.Exp.Op("idxS", [d,n,a], t) =>
           (compS E d (fn d =>
            compS E n (fn n =>
            compA E a (fn a => Laila.idxS d n a S A >>= k))))
         | Tail.Exp.Op("idxassign", [is,ma,v], t) =>
           (compA E is (fn is =>
            compS E v (fn v =>
            compMA E ma (fn ma => Laila.idxassign is ma v >>= (fn () => kS(Laila.B true))))))
         | Tail.Exp.Op(opr,es,_) =>
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
              | S_AM opr => compU compS E es (fn p => opr p >>= kA)
              | SA_AM opr => compP compS compA E es (fn p => opr p >>= kA)
              | AA_AM opr => compP compA compA E es (fn p => opr p >>= kA)
              | VA_AM opr => compP compV compA E es (fn p => opr p >>= kA)
              | NOTOP => die ("comp: operator " ^ qq opr ^ " not supported"))
    end
and comp_each E f a t k =
    compFN E f (fn f =>
    compA E a (fn a =>
    let val f = fn x => f [S x] >>= (Laila.ret o unS "each")
    in k $ A $ Laila.each (ltypeOf t) f a
    end))
and unS s = fn S s => s | _ => die ("unS: " ^ s)
and unA s = fn A a => a 
             | MA a => Laila.mm2m a
             | _ => die ("unA: " ^ s)
and compS E e k = comp E e (k o unS "compS")
and compA E (e : Tail.Exp.uexp) (k: Laila.m -> lexp Laila.M) : lexp Laila.M =
    comp E e (fn A a => k a 
               | MA a => k (Laila.mm2m a)
               | _ => die ("compA; e = " ^ Tail.pp_exp true e))
and compMA E (e : Tail.Exp.uexp) (k: Laila.mm -> lexp Laila.M) : lexp Laila.M =
    comp E e (fn MA a => k a
               | _ => die ("compMA; e = " ^ Tail.pp_exp true e))
and compV E (e : Tail.Exp.uexp) (k: int list -> lexp Laila.M) : lexp Laila.M =
    case e of
        Tail.Exp.Vc(xs,_) => k(List.map (fn Tail.Exp.I x => x
                                 | _ => die "compV.expecting immediate integer") xs)
      | _ => die "compV.expecting immediate integer vector" 
and compFN E (e : Tail.Exp.uexp) (k: (lexp list -> lexp Laila.M) -> lexp Laila.M) : lexp Laila.M =
    comp E e (fn FN f => k f | _ => die "compFN")
and comps c E nil k = k nil
  | comps c E (e::es) k = 
    comps c E es (fn xs => c E e (fn x => k (x::xs)))
fun compile flags p =
    let val verbose_p = Flags.flag_p flags "-v"
        val optlevel = 2
        val m = comp FM.empty (Tail.toExp p) Laila.ret
        val m = m >>= (fn S e => Laila.ret e
                        | _ => die "compile: result expected to be a scalar")
    in Laila.runM {verbose=verbose_p,optlevel=optlevel} Laila.Double m
    end

end
