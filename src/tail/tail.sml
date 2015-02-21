structure Tail : TAIL = struct

structure Exp = TailExp(TailType)

fun die s = raise Fail ("Tail." ^ s)

open TailType
open Exp
type Int = unit
type Double = unit
type 'a Num = unit
type Bool = unit
type Char = unit
type 'a Vec = unit
type 'a T = typ

fun vecLength e =
    case e of
        Vc(es,_) => SOME(List.length es)
      | _ => NONE

(* Expressions *)
fun binOp opr (x,y) = Op_e(opr,[x,y])
fun binOp' opr x y = Op_e(opr,[x,y])
val addi = binOp "addi"
val subi = binOp "subi"
val muli = binOp "muli"
val divi = binOp "divi"
val resi = binOp "resi"
val lti  = binOp "lti"
val ltei = binOp "ltei"
val gti  = binOp "gti"
val gtei = binOp "gtei"
val eqi  = binOp "eqi"
val maxi = binOp' "maxi"
val mini = binOp' "mini"
fun negi x = Op_e("negi",[x])
fun absi x = Op_e("absi",[x])
fun signi x = Op_e("signi",[x])

val andi = binOp "andi"
val ori = binOp "ori"
val shri = binOp "shri"
val shari = binOp "shari"
val shli = binOp "shli"
val xori = binOp "xori"
fun noti x = Op_e("noti",[x])

fun floor x= Op_e("floor",[x])
fun ceil x = Op_e("ceil",[x])
fun ln x = Op_e("ln", [x])
fun cos x = Op_e("cos", [x])
fun sin x = Op_e("sin", [x])
fun tan x = Op_e("tan", [x])
fun acos x = Op_e("acos", [x])
fun asin x = Op_e("asin", [x])
fun atan x = Op_e("atan", [x])
fun cosh x = Op_e("cosh", [x])
fun sinh x = Op_e("sinh", [x])
fun tanh x = Op_e("tanh", [x])
fun pi () = Op_e ("pi", nil)
val addd = binOp "addd"
val subd = binOp "subd"
val muld = binOp "muld"
val divd = binOp "divd"
val resd = binOp "resd"
val powd = binOp "powd"
val ltd  = binOp "ltd"
val lted = binOp "lted"
val gtd  = binOp "gtd"
val gted = binOp "gted"
val eqd  = binOp "eqd"
val maxd = binOp' "maxd"
val mind = binOp' "mind"
fun negd x = Op_e("negd",[x])
fun absd x = Op_e("absd",[x])
fun signd x = Op_e("signd",[x])

val andb  = binOp "andb"
val orb   = binOp "orb"
val eqb  = binOp "eqb"
val xorb  = binOp "xorb"
val nandb = binOp "nandb"
val norb  = binOp "norb"
fun notb x = Op_e("notb",[x])

fun roll x = Op_e("roll",[x])

val ltc  = binOp "ltc"
val ltec = binOp "ltec"
val gtc  = binOp "gtc"
val gtec = binOp "gtec"
val eqc  = binOp "eqc"

val neqd = notb o eqd
val neqi = notb o eqi
val neqc = notb o eqc

val i2d = fn x => Op_e ("i2d", [x])
val b2i = fn x => Op_e ("b2i", [x])
val op % = binOp "mod"

fun If (b,e1,e2) = Iff_e(b,e1,e2)

val fromList = fn _ => Vc_e
val fromChars = Vc_e o (List.map C)

type 'a t = exp
type 'a v = exp
type 'a NUM = exp
type INT = exp
type DOUBLE = exp
type BOOL = exp
type CHAR = exp
                
type 'a M = ('a -> exp) -> exp
         
fun runHack (m : 'a M) : 'a option =
    let exception RunHack of 'a
    in (m (fn x => raise RunHack x); NONE)
       handle RunHack x => SOME x
    end

fun ret (v:'a) : 'a M = fn k => k v

infix >>=
fun (f : 'a M) >>= (g : 'a -> 'b M) : 'b M =
   fn k => f (fn x => g x k)

(* Compiled Programs *)
type ('a,'b) prog = exp
fun toExp x = x
val main_arg_var = newVar()
fun runF _ f = f (Var(main_arg_var,TyVar())) (fn x => x)
 
(* Values and Evaluation *)
type 'a V = Exp.value
fun Iv _ = die "Iv"
fun unIv _ = die "unIv"
val Dv = Exp.Dvalue
val unDv = Exp.unDvalue
fun Bv _ = die "Bv"
fun unBv _ = die "unBv"
fun Vv _ = die "Vv"
fun unVv _ = die "unVv"
val Uv = Exp.Uvalue

type 'a MVec = unit
type 'a m = exp
fun zilde () = Op_e("zilde",nil)
fun scl t = t
fun scalar t = Vc_e[t]
fun vec t = t
fun iota t = Op_e("iota",[t])
fun iota' t = Op_e("iota",[t])
fun first t = Op_e("first",[t])
fun siz t = Op_e("siz",[t])
fun dim t = Op_e("dim",[t])   (* or I(#2 t) *)
fun rav t = Op_e("rav",[t])
fun rav0 t = t
fun mkFn1 f =
    let val v = newVar()
        val t = TyVar()
        val e0 = f (Var(v,t))
    in Fn_e(v,t,e0)
    end
fun mkFn2 f =
    let val (v1, v2) = (newVar(), newVar())
        val (t1, t2) = (TyVar(), TyVar())
        val e0 = f (Var(v1,t1), Var(v2,t2))
    in Fn_e(v1,t1,Fn_e(v2,t2,e0))
    end
fun mkFn2m f = mkFn2 (fn a => f a (fn x=>x))
fun mkFn1m f = mkFn1 (fn a => f a (fn x=>x))

fun each f e = Op_e("each",[mkFn1m f,e])
fun red f n e = Op_e("red",[mkFn2 f,n,e])
fun mif (b,e1,e2) = If(b,e1,e2)
fun zipWith f e1 e2 = Op_e("zipWith",[mkFn2m f,e1,e2])
fun scan f e2 = Op_e("scan",[mkFn2m f,e2])
fun getStaticRank e =
    let val t = typeOf e
    in case unS t of
           SOME _ => SOME 0
         | NONE =>
       case unSV t of
           SOME _ => SOME 1
         | NONE =>
       case unVcc t of
           SOME _ => SOME 1 
         | NONE => 
       case unArr t of
           SOME (_, r) => (case unRnk r of SOME i => SOME i
                                         | NONE => NONE)
         | NONE => NONE
    end
fun catenate e1 e2 =
    let fun cat () = Op_e("cat", [e1,e2])
        fun cons () = Op_e("cons",[e1,e2])
        fun snoc () = Op_e("snoc",[e1,e2])
        open Int
    in case (getStaticRank e1, getStaticRank e2) of
           (SOME 0, SOME 0) => Vc_e[e1,e2]
         | (SOME i1, SOME i2) => if i2=i1+1 then cons()
                       else if i1=i2+1 then snoc()
                       else if i1=i2 then cat()
                       else die ("rank error: incompatible argument ranks for catenate: " 
                                 ^ Int.toString i1 ^ " and " ^ Int.toString i2)
         | _ => (*die "rank error: catenate not supported for arguments of unknown rank"*) cat()
    end
fun take e1 e2 = Op_e("take", [e1,e2])
fun drop e1 e2 = Op_e("drop", [e1,e2])
fun mem e = Op_e("mem",[e])
fun reshape e1 e2 = Op_e("reshape", [e1,e2])
fun shape e = Op_e("shape",[e])
fun reduce f e1 e2 s a =
    case getStaticRank e2 of
        SOME 0 => s e2
      | SOME r => let val e = Op_e("reduce",[mkFn2m f,e1,e2])
                  in if r=1 then s e else a e
                  end
      | NONE => die "rank error: reduce not supported for arguments of unknown rank"

fun idxS x ei ea s a =
    let val e = Op_e("idxS",[I x,ei,ea])
    in case getStaticRank ea of
           SOME 0 => die "rank error: idxS applied to array argument of rank 0"
         | SOME 1 => s e
         | SOME _ => a e
         | NONE => die "rank error: idxS not supported for arguments of unknown rank"
    end

fun idx x eis ea = Op_e("idx",[I x,eis,ea])
fun idxassign is a v = 
    case a of
        Var (var,_) => Op_e("idxassign",[is,a,v]) before mutableVar var := true
      | _ => die "idxassign.expects Var"

fun compress b a = Op_e("compress",[b,a])
fun replicate v b a = Op_e("replicate",[v,b,a])
fun power f e1 e2 = Op_e("power",[mkFn1m f,e1,e2])
fun powerScl f e1 e2 = Op_e("powerScl",[mkFn1m f,e1,e2])

fun transpose e = Op_e("transp", [e])
fun transpose2 e1 e2 = Op_e("transp2", [e1,e2])
fun vreverse e = Op_e("vreverse", [e])
fun reverse e = transpose(vreverse(transpose e))
fun vrotate e1 e2 = Op_e("vrotate", [e1,e2])
fun rotate e1 e2 = transpose(vrotate e1 (transpose e2))
fun catenate_first e1 e2 =
    transpose(catenate (transpose e1) (transpose e2))
fun lett' e f =
    let val v = newVar()
        val t = typeOf e
    in Let_e(v,t,e,f(Var(v,t)))
    end
fun lett e = 
    let val v = newVar()
        val t = typeOf e
    in fn f => Let_e(v,t,e,f(Var(v,t)))
    end

fun letm e =
    let val v = newVar()
        val t = typeOf e
    in fn f => Let_e(v,t,e,f(Var(v,t)))
    end

local fun pr s x = Op_e(s,[x])
in
val prArrI = pr "prArrI"
val prArrB = pr "prArrB"
val prArrD = pr "prArrD"
val prArrC = pr "prArrC"
val prSclI = pr "prSclI"
val prSclB = pr "prSclB"
val prSclD = pr "prSclD"
val prSclC = pr "prSclC"
end

(* Optimization *)

structure Optimize = struct

type def = {shape: Exp.exp option, value: Exp.exp option}
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
      | _ => Op(opr,es,t)
               
and getShape (E:env) (e : Exp.exp) : Exp.exp option =
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
      | Var _ => true
      | Vc(es,_) => length es <= 3 andalso List.all simple es
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
end

(* Pretty printing *)

fun prInstanceLists opr es t =
    let 
        fun unArr'' at = case unArr' at of
                             SOME p => p
                           | NONE => die ("unArr'': " ^ opr)
        fun len at =
            case unVcc at of
                SOME(_,r) => r
              | NONE => case unSV at of
                            SOME _ => T.rnk 1
                          | NONE => die ("prInstanceLists.len: " ^ opr)
        fun value at =
            case unS at of
                SOME (_,r) => r
              | NONE => die ("prInstanceLists.value: " ^ opr)
        val rnk = #2 o unArr''  (* return the rank of an array *)
        val bt  = #1 o unArr''  (* return the base type of an array type *)
        val ts = List.map typeOf es
        fun wrap il1 il2 = "{[" ^ String.concatWith "," (List.map prBty il1) ^ "],[" ^
                           String.concatWith "," (List.map prRnk il2) ^"]}"
        val none = ""
    in case (opr, ts) of
           ("each", [tf,ta]) => wrap [bt ta,bt t] [rnk t]
         | ("eachV", [tf,ta]) => wrap [bt ta,bt t] [len t] 
         | ("reduce", [tf,te,ta]) => wrap [bt te] [rnk t]
         | ("idxS", [_,_,ta]) => wrap [bt ta] [rnk t]
         | ("idx", [_,_,ta]) => wrap [bt ta] [rnk t]
         | ("compress", [_,ta]) => wrap [bt ta] [rnk t]
         | ("replicate", [_,ta]) => wrap [bt ta] [rnk t]
         | ("shape", [ta]) => wrap [bt ta] [rnk ta] 
         | ("shapeV", [ta]) => wrap [bt ta] [len ta] 
         | ("take", [_,ta]) => wrap [bt ta] [rnk ta]
         | ("drop", [_,ta]) => wrap [bt ta] [rnk ta] 
         | ("takeV", [_,ta]) => wrap [bt ta] [len t]
         | ("dropV", [_,ta]) => wrap [bt ta] [len t]
         | ("cat", [t1,t2]) => wrap [bt t1] [rnk t1]
         | ("catV", [t1,t2]) => wrap [bt t1] [len t1,len t2]
         | ("cons", [t1,t2]) => wrap [bt t1] [rnk t1]
         | ("snoc", [t1,t2]) => wrap [bt t1] [rnk t2]
         | ("snocV", [t1,t2]) => wrap [bt t1] [len t1]
         | ("consV", [t1,t2]) => wrap [bt t1] [len t2]
         | ("vrotate", [_,t2]) => wrap [bt t2] [rnk t2]
         | ("vreverse", [t1]) => wrap [bt t1] [rnk t1]
         | ("vreverseV", [t1]) => wrap [bt t1] [len t1]
         | ("first", [ta]) => wrap [bt ta] [rnk ta]
         | ("firstV", [ta]) => wrap [bt ta] [value ta]
         | ("transp", [ta]) => wrap [bt ta] [rnk ta]
         | ("transp2", [_,ta]) => wrap [bt ta] [rnk ta]
         | ("reshape", [_,ta]) => wrap [bt t] [rnk ta,rnk t]
         | ("zipWith", [ft,t1,t2]) => wrap [bt t1,bt t2,bt t] [rnk t1]
         | _ => none
    end

fun pp_exp (prtype:bool) e =
    let infix @@
        fun isVars nil nil = true
          | isVars (v::vs) (Var(v',_)::es) = v=v' andalso isVars vs es
          | isVars _ _ = false
        fun lookForOp vs e =
            case e of
                Fn(v,_,e',_) => lookForOp (v::vs) e'
              | Op(opr,es,t) => if isVars (rev vs) es then SOME (opr,t)
                                else NONE
              | _ => NONE
        val op + = Int.+
        val op - = Int.-
        datatype t = @@ of t * t | $ of string
        fun flatten t =
            let fun f t a =
                    case t of $ s => s::a
                            | t1 @@ t2 => f t1 (f t2 a)
            in String.concat (f t nil)
            end
        fun space 0 = ""
          | space n = " " ^ space (n-1)
        fun indent i = $("\n" ^ space i)
        fun maybePrType opr es t =
            $(if prtype then prInstanceLists opr es t
              else "")
        fun pp i e : t =
            case e of
                Var (v,_) => $(ppVar v)
              | I i => $(Int32.toString i)
              | D r => $(Real.fmt (StringCvt.FIX (SOME 2)) r)
              | B true => $"tt"
              | B false => $"ff"
              | C w => $("'" ^ pr_char w ^ "'")
              | Iff (c,e1,e2,_) => 
                let val i' = i + 2
                in $"if " @@ pp (i+3) c @@ $" then" @@
                    indent i' @@ pp i' e1 @@
                    indent i @@ $"else " @@
                    indent i' @@ pp i' e2
                end
                | Vc(es,_) => $"[" @@ pps (i+1) es @@ $"]"
                | Op (opr,nil,t) => $opr
                | Op (opr,es,t) => $opr @@ maybePrType opr es t 
                                    @@ $"(" @@ pps (i+1+size opr) es @@ $")"
                | Let (v,ty,e1,e2,_) => $"let " @@ $(ppVar v) @@ $":" @@ $(prType ty) @@ $" = " @@ pp (i+2) e1 @@ $" in" @@ 
                                         indent i @@ pp i e2
                | Fn (v,t,e,_) =>
                  (case lookForOp [v] e of
                       SOME (opr,t) => $opr
                     | NONE => $("fn " ^ ppVar v ^ ":" ^ prType t ^ " => ") @@ pp (i+2) e)
        and pps i nil = $""
          | pps i [e] = pp i e
          | pps i (e::es) = pp i e @@ $"," @@ pps i es
    in flatten(pp 0 e)
    end

val pp_prog = pp_exp
val ppV = Exp.pr_value

fun outprog prtype ofile p =
    let val body = pp_prog prtype p
        val os = TextIO.openOut ofile
        fun outln s = TextIO.output (os, s^"\n")
    in outln body
     ; TextIO.closeOut os
     ; print ("Wrote file " ^ ofile ^ "\n")
    end

fun runM {verbose,optlevel,prtype} tt m =
    let val p = m (fn x => x)
        fun prln f =
            if verbose then (print (f()); print "\n")
            else ()
        val () = prln (fn() => "Untyped program:\n" ^ pp_prog false p)
        val () = prln (fn() => "Typing the program...")
        fun typeit h p =
            case typeExp empEnv p of
                ERR s => 
                let val msg = "***Type error - " ^ h ^ ": " ^ s
                in prln (fn() => msg);
                   prln (fn() => "Program:");
                   prln (fn() => pp_prog prtype p);
                   die msg
                end
              | OK t => (prln (fn() => "  Program has type: " ^ prType t);   (* perhaps unify tt with t!! *)
                         let val p = Exp.resolveShOpr p
                         in prln (fn() => "Typed program - " ^ h ^ ":\n" ^ pp_prog prtype p);
                            p
                         end)
        val p = typeit "before optimization" p
        val p = Optimize.optimize optlevel p
        val p = typeit "after optimization" p
        val () = prln (fn() => "Optimised program:\n" ^ pp_prog prtype p)
    in p
    end

fun eval p v =
    let val de = Exp.addDE Exp.empDEnv main_arg_var v
        val v' = Exp.eval de p
    in v'
    end

fun readFile e = Op_e("readFile", [e])
fun readIntVecFile e = Op_e("readIntVecFile", [e])
fun readDoubleVecFile e = Op_e("readDoubleVecFile", [e])
    
end
