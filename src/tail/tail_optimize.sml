functor TailOptimize (Exp : TAIL_EXP) :
sig
  val optimize : int -> Exp.texp -> Exp.texp
  val optimisationLevel : int ref
end
 = struct

open Exp.T
open Exp

val optimisationLevel = ref 0
fun optlevel() = !optimisationLevel

type def = {shape: Exp.texp option, value: Exp.texp option}
type env = def FM.map

fun rot 0 es = es
  | rot n es = if n < 0 then rev(rot (~n) (rev es))
               else case es of
                        e::es => rot (n-1) (es@[e])
                      | [] => []

fun peep E e =
    case e of
        Op(opr,es,t) => peepOp E (opr,es,t)
      | e => e
and peepOp E (opr,es,t) =
    case (opr, es) of
        ("addi", [I 0,e]) => e
      | ("addi", [e,I 0]) => e
      | ("addi", [I i1,I i2]) => I(Int32.+(i1,i2))
      | ("subi", [I i1,I i2]) => I(Int32.-(i1,i2))
      | ("subi", [e,I 0]) => e
      | ("negi", [I i]) => I(Int32.~ i)
      | ("absi", [I i]) => I(Int32.abs i)
      | ("i2d", [I i]) => D(Real.fromLargeInt (Int32.toLarge i))
      | ("b2i", [B true]) => I 1
      | ("b2i", [B false]) => I 0
      | ("b2iV", [B true]) => I 1
      | ("b2iV", [B false]) => I 0
      | ("reduce", [f,n,Op("zilde",[],_)]) => n
      | ("vreverse", [Vc(es,t)]) => Vc(rev es, t)
      | ("vrotateV", [I 0,e]) => e
      | ("shape", [e]) => (case getShape E e of
                               SOME e => e
                             | NONE => Op (opr,[e],t))
      | ("shapeV", [e]) => (case getShape E e of
                                 SOME e => e
                               | NONE => Op (opr,[e],t))
      | ("dropV", [I n,Vc(es',_)]) =>
        let val n = Int32.toInt n
        in if n >= 0 andalso n <= length es' then Vc(List.drop(es',n),t)
           else if n < 0 andalso ~n <= length es' then Vc(List.take(es',length es' + n),t)
           else Op(opr,es,t)
        end
      | ("takeV", [I n,Vc(es',_)]) =>
        let val n = Int32.toInt n
        in if n >= 0 andalso n <= length es' then Vc(List.take(es',n),t)
           else if n < 0 andalso ~n <= length es' then Vc(List.drop(es',length es' + n),t)
           else Op(opr,es,t)
        end
      | ("firstV", [Vc(e::es,t)]) => e
      | ("first", [Vc(e::es,t)]) => e
      | ("transp2", [Vc([I 2,I 1],_),e]) => Op("transp", [e],t)
      | ("transp2", [Vc([_],_),e]) => e
      | ("catV", [Vc(es1,_),Vc(es2,_)]) => Vc(es1@es2,t)
      | ("cat", [Vc(es1,_),Vc(es2,_)]) => Vc(es1@es2,t)
      | ("snocV", [Vc(es,_),e]) => Vc(es@[e],t)
      | ("iotaV",[I n]) => 
        let val n = Int32.toInt n
        in if n <= 3 then Vc(List.map I (List.tabulate (n,fn x => x+1)),t)
           else Op(opr,es,t)
        end
      | ("eachV", [Fn(v,_,Op("b2i",[Var (v',_)],t'),_),Vc(es',_)]) =>
        if v=v' then Vc(List.map (fn e => peepOp E ("b2i",[e],t')) es',t)
        else Op(opr,es,t)
      | ("catV", [Vc([],_),e]) => e
      | ("vrotateV", [I n,Vc(es,_)]) => Vc(rot(Int32.toInt n) es,t)
      | ("transp", [Vc e]) => Vc e
      | ("reshape", [Vc([I n],_), Vc(es',t')]) =>
        if Int32.toInt n = length es' then Vc(es',t')
        else Op(opr,es,t)
      | ("idxS", [I 1, I i, Vc(xs,_)]) => List.nth(xs,i-1)
      | _ =>
        if optlevel() > 0 then 
          case (opr, es) of
              ("powd", [D a, D b]) => D(Math.pow(a,b))
            | ("muld", [D a, D b]) => D(Real.*(a,b))
            | ("addd", [D a, D b]) => D(Real.+(a,b))
            | ("subd", [D a, D b]) => D(Real.-(a,b))
            | _ => Op(opr,es,t)
        else Op(opr,es,t)
               
and getShape (E:env) (e : Exp.texp) : Exp.texp option =
    let fun tryType() =
            case unVcc (typeOf e) of
                NONE => NONE
              | SOME (bt,r) => case unRnk r of
                                   NONE => NONE
                                 | SOME i => SOME (Vc([I(Int32.fromInt i)],SV IntB r))
    in case tryType() of
           SOME e => SOME e
         | NONE =>
       case e of
           Op("reshape",[sh,e],_) => SOME sh
         | Var(v,_) => (case FM.lookup E v of
                            SOME{shape=SOME sh,...} => SOME sh
                          | _ => NONE)
         | Vc(es,_) => SOME(Vc([I(Int32.fromInt(length es))],SV IntB (rnk(length es))))
         | Op("transp", [e], _) =>
           (case getShape E e of
                SOME sh => SOME(peepOp E ("vreverse",[sh],typeOf sh))
              | NONE => NONE)
         | _ => NONE
    end

fun simple e =
    case e of
        I _ => true
      | D _ => true
      | B _ => true
      | C _ => true
      | Var _ => true
      | Vc(es,_) => (*length es <= 20 andalso*) List.all simple es
      | _ => false

fun optimize optlevel e =
    if Int.<= (optlevel, 0) then e
    else
    let fun add E k v = E
        fun opt E e =
            case e of
                Var (v,_) => (case FM.lookup E v of
                                  SOME{value=SOME e,...} => e
                                | _ => e)
              | I i => e
              | D r => e
              | B b => e
              | C w => e
              | Iff (c,e1,e2,t) => Iff(opt E c,opt E e1,opt E e2,t)
              | Vc(es,t) => Vc (opts E es,t)
              | Op(opr,es,t) => peepOp E (opr,opts E es,t)
              | Let (v,ty,e1,e2,t) => 
                let val e1 = opt E e1
                in if simple e1 then
                     let val E' = FM.add(v,{shape=NONE,value=SOME e1},E)
                     in opt E' e2
                     end
                   else 
                     let val sh = getShape E e1
                         val E' = FM.add(v,{shape=sh,value=NONE},E)
                         val e2 = opt E' e2
                     in Let(v,ty,e1,e2,t)
                     end
                end
              | Fn (v,t,e,t') => 
                let val E' = FM.add(v,{shape=NONE,value=NONE},E)
                in Fn(v,t,opt E' e,t')
                end
        and opts E es = List.map (opt E) es
        val initE = FM.empty
    in opt initE e
    end

(* Inliner : The inliner inlines simple (i.e., non-side-effecting
expressions not containing lambdas are considered simple) variable
definitions for which the bound variables are used at most once. The
inliner works in two steps. The first step computes a map (buttom-up)
of the number of uses of a variable (ZERO, ONE, MANY). Variables that
are used under a lambda are considered to be used MANY times (such
variables are therefore not inlined). The second step is a top-down
transformation of the source, which inlines simple variable
definitions that are inferred in the first step to be used ONE
time. Simple variable definitions that are never used are eliminated.
*)
                         
datatype mul = ONE | MANY

type IE = mul FM.map
fun bumpIE (ie : IE) : IE = FM.composemap (fn _ => MANY) ie
fun plusIE (ie1,ie2) : IE = FM.mergeMap (fn _ => MANY) ie1 ie2
fun oneIE v : IE = FM.singleton(v,ONE)
val emptyIE : IE = FM.empty

fun uses e =
    case e of
        Var (v,_) => oneIE v
      | I _ => emptyIE
      | D _ => emptyIE
      | B _ => emptyIE
      | C _ => emptyIE
      | Iff (c,e1,e2,_) => usess [c,e1,e2]
      | Vc(es,_) => usess es
      | Op(opr,es,_) => usess es
      | Let (_,_,e1,e2,_) => usess [e1,e2]
      | Fn (_,_,e,_) => bumpIE(uses e)
and usess nil = emptyIE
  | usess (e::es) = plusIE(uses e,usess es)

(* The inliner needs to be implemented fully one day - for now, we are
instead more aggressive in inline expressions (even bindings that are
referenced multiple times.) See definition of "simple" above. *)


end (* end of Optimize structure *)
