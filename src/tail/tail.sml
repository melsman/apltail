structure Tail : TAIL = struct

structure Exp = TailExp(TailType)
structure Optimize = TailOptimize(Exp)

val optimisationLevel = Optimize.optimisationLevel

fun die s = raise Fail ("Tail." ^ s)

open TailType
open Exp
type Int = unit
type Double = unit
type Complex = unit
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
fun nowi x = Op_e("nowi",[x])

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
fun expd x = Op_e("expd",[x])
fun signd x = Op_e("signd",[x])

val andb  = binOp "andb"
val orb   = binOp "orb"
val eqb  = binOp "eqb"
val xorb  = binOp "xorb"
val nandb = binOp "nandb"
val norb  = binOp "norb"
fun notb x = Op_e("notb",[x])

val addx = binOp "addx"
val subx = binOp "subx"
val mulx = binOp "mulx"
fun negx x = Op_e("negx",[x])
fun conjx x = Op_e("conjx",[x])
fun magnx x = Op_e("magnx",[x])
val injx = binOp "injx"
fun expx x = Op_e("expx",[x])
fun rex x = Op_e("rex",[x])
fun imx x = Op_e("imx",[x])
fun d2x x = Op_e("d2x",[x])
                 
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

type 'a exp = uexp
type 'a tvector = uexp
type 'a NUM = uexp
type INT = uexp
type DOUBLE = uexp
type COMPLEX = uexp
type BOOL = uexp
type CHAR = uexp

val typeOf = Exp.typeOf
                
type 'a M = ('a -> uexp) -> uexp
         
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
type ('a,'b) prog = uexp
fun toExp x = x
val main_arg_var = newVar()
fun runF _ f = f (Var(main_arg_var,TyVar())) (fn x => x)
 
(* Values and Evaluation *)
type 'a value = Exp.value
fun Iv _ = die "Iv"
fun unIv _ = die "unIv"
val Dv = Exp.Dvalue
val unDv = Exp.unDvalue
val Xv = Exp.Xvalue
val unXv = Exp.unXvalue
fun Bv _ = die "Bv"
fun unBv _ = die "unBv"
fun Vv _ = die "Vv"
fun unVv _ = die "unVv"
val Uv = Exp.Uvalue

type 'a MVec = unit
type 'a ndarray = uexp
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
fun scan f en e2 = Op_e("scan",[mkFn2m f,en,e2])
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
fun memScl e = Op_e("memScl",[e])
fun reshape e1 e2 = Op_e("reshape", [e1,e2])
fun shape e = Op_e("shape",[e])
fun gradeUp e = Op_e("gradeUp",[e])
fun gradeDown e = Op_e("gradeDown",[e])
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

fun idx x eis ea = each (fn ei => ret(idxS x ei ea (fn x => x) (fn _ => raise Fail "tail.idx expecting scalar"))) eis

(*Op_e("idx",[I x,eis,ea])*)
fun idxassign is a v = 
    case a of
        Var (var,_) => Op_e("idxassign",[is,a,v]) before mutableVar var := true
      | _ => die "idxassign.expects Var"

fun compress b a = Op_e("compress",[b,a])
fun replicate v b a = Op_e("replicate",[v,b,a])
fun power f e1 e2 = Op_e("power",[mkFn1m f,e1,e2])
fun powerScl f e1 e2 = Op_e("powerScl",[mkFn1m f,e1,e2])
fun bench f e1 e2 = Op_e("bench",[mkFn1m f,e1,e2])
fun condScl f e1 e2 = Op_e("condScl",[mkFn1m f,e1,e2])

type 'a tuple = uexp
type nil = unit
type ('a,'b) tupleIdx = int
val empTuple = Tuple_e nil
fun consTuple v t =
    case unTuple t of
        SOME l => Tuple_e(v::l)
      | NONE => raise Fail "consTuple: cannot cons expression to non-tuple"
fun Zero () = 0
fun Succ i = i + 1
fun prjTuple i e = Prj_e(i,e)
fun powerN f e1 e2 = Op_e("power",[mkFn1m f,e1,e2])

structure Unsafe = struct
type utuple = uexp
type uexp = uexp
fun letUtuple e = 
    let val v = newVar()
        val t = typeOf e
    in fn f => Let_e(v,t,e,f(Var(v,t)))
    end
val upowerN = powerN
val empUtuple = Tuple_e nil
fun consUtuple v t =
    case unTuple t of
        SOME l => Tuple_e(v::l)
      | NONE => raise Fail "consTuple: cannot cons expression to non-tuple"
fun prjUtuple i e = Prj_e(i,e)
val toUexp = fn x => x
val toUexpA = fn x => x
val fromUexp = fn x => x
val fromUexpA = fn x => x
end
                         
fun transpose e = Op_e("transp", [e])
fun transpose2 e1 e2 = Op_e("transp2", [e1,e2])
fun vreverse e = Op_e("vreverse", [e])
fun reverse e = transpose(vreverse(transpose e))
fun vrotate e1 e2 = Op_e("vrotate", [e1,e2])
fun rotate e1 e2 = Op_e("rotate", [e1,e2])
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

(* Pretty printing *)
local fun pr s x = Op_e(s,[x])
in
val prArrI = pr "prArrI"
val prArrB = pr "prArrB"
val prArrD = pr "prArrD"
val prArrC = pr "prArrC"
val prArrX = pr "prArrX"
val prSclI = pr "prSclI"
val prSclB = pr "prSclB"
val prSclD = pr "prSclD"
val prSclC = pr "prSclC"
val prSclX = pr "prSclX"
val formatI = pr "formatI"
val formatD = pr "formatD"
val formatX = pr "formatX"
end

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
         | ("zilde", nil) => wrap [bt t] [rnk t]
         | ("reduce", [tf,te,ta]) => wrap [bt te] [rnk t]
         | ("scan", [tf,te,ta]) => wrap [bt te] [rnk t]
         | ("idxS", [_,_,ta]) => wrap [bt ta] [rnk t]
         | ("idx", [_,_,ta]) => wrap [bt ta] [rnk t]
         | ("compress", [_,ta]) => wrap [bt ta] [rnk t]
         | ("replicate", [_,ta]) => wrap [bt ta] [rnk t]
         | ("rav", [ta]) => wrap [bt ta] [rnk ta] 
         | ("gradeUp", [ta]) => wrap [bt ta] [] 
         | ("gradeDown", [ta]) => wrap [bt ta] [] 
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
         | ("rotate", [_,t2]) => wrap [bt t2] [rnk t2]
         | ("vrotateV", [_,t2]) => wrap [bt t2] [len t2]
         | ("rotateV", [_,t2]) => wrap [bt t2] [len t2]
         | ("vreverse", [t1]) => wrap [bt t1] [rnk t1]
         | ("vreverseV", [t1]) => wrap [bt t1] [len t1]
         | ("first", [ta]) => wrap [bt ta] [rnk ta]
         | ("firstV", [ta]) => wrap [bt ta] [value ta]
         | ("transp", [ta]) => wrap [bt ta] [rnk ta]
         | ("transp2", [_,ta]) => wrap [bt ta] [rnk ta]
         | ("reshape", [_,ta]) => wrap [bt t] [rnk ta,rnk t]
         | ("zipWith", [ft,t1,t2]) => wrap [bt t1,bt t2,bt t] [rnk t1]
         | ("power", [ft,_,t2]) =>
           let val btrnk_s =
                   case unTup t2 of
                       SOME ts => List.map (fn t => (bt t,rnk t)) ts
                     | NONE => [(bt t2,rnk t2)]
               val (bts,rnks) = ListPair.unzip btrnk_s
           in wrap bts rnks
           end
         | ("powerScl", [ft,_,t2]) => wrap [bt t2] []
         | ("condScl", [ft,_,t2]) => wrap [bt t2] []
         | ("bench", [ft,_,t2]) => wrap [bt t2] []
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
              | I i => $(Util.intToCString i)
              | D r => $(Util.realToTailString r)
              | X (re,im) => $(Util.realToTailString re ^ "j" ^ Util.realToTailString im)
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
              | Vc(nil,t) => pp i (Op("zilde",[],t))
              | Vc(es,_) => $"[" @@ pps (i+1) es @@ $"]"
(*              | Op (opr,nil,t) => $opr @@ maybePrType opr nil t *)
              | Op (opr,es,t) => $opr @@ maybePrType opr es t 
                                  @@ $"(" @@ pps (i+1+size opr) es @@ $")"
              | Let (v,ty,e1,e2,_) => $"let " @@ $(ppVar v) @@ $":" @@ $(prType ty) @@ $" = " @@ pp (i+2) e1 @@ $" in" @@ 
                                       indent i @@ pp i e2
              | Fn (v,t,e,_) =>
                (case lookForOp [v] e of
                     SOME (opr,t) => $opr
                   | NONE => $("fn " ^ ppVar v ^ ":" ^ prType t ^ " => ") @@ pp (i+2) e)
              | Tuple (es,_) => $"(" @@ pps i es @@ $")"
              | Prj(i,e,_) =>
                let val t = if (*prtype*) false then
                              let val ta = typeOf e
                                  val sz = case unTup ta of
                                               SOME ts => Int.toString(length ts)
                                             | NONE => "arg_size_unknown"
                              in "{" ^ sz ^ "}"
                              end
                            else ""
                in $("Prj" ^ t ^ "(" ^ Int.toString i ^ ",") @@ pp i e @@ $")"
                end
        and pps i nil = $""
          | pps i [e] = pp i e
          | pps i (e::es) = pp i e @@ $"," @@ pps i es
    in flatten(pp 0 e)
    end

val pp_prog = pp_exp
val ppValue = Exp.pr_value

fun outprog prtype ofile p =
    let val body = pp_prog prtype p
        val os = TextIO.openOut ofile
        fun outln s = TextIO.output (os, s^"\n")
    in outln body
     ; TextIO.closeOut os
     ; print ("Wrote file " ^ ofile ^ "\n")
    end

fun runM {verbose,optlevel,materialize,prtype} tt m =
    let val p = m (fn x => x)
        val _ = Util.log verbose (fn _ => "Untyped program:\n" ^ pp_prog false p)
        val _ = Util.log verbose (fn _ => "Typing the program...")
        fun typeit h p =
            case typeExp emptyEnv p of
                ERR s => 
                let val msg = "***Type error - " ^ h ^ ": " ^ s
                in Util.log verbose (fn _ => msg);
                   Util.log verbose (fn _ => "Program:");
                   Util.log verbose (fn _ => pp_prog prtype p);
                   die msg
                end
              | OK t => (Util.log verbose (fn _ => "  Program has type: " ^ prType t);   (* perhaps unify tt with t!! *)
                         let val p = Exp.resolveShOpr p
                         in Util.log verbose (fn _ => "Typed program - " ^ h ^ ":\n" ^ pp_prog prtype p);
                            p
                         end)
        val p = typeit "before optimization" p
        val p = Optimize.optimize optlevel p
        val p = typeit "after optimization" p
        val p = if materialize then
                  let val p = Optimize.materialize p
                      val p = typeit "after materialization" p
                  in p
                  end
                else p
        val _ = Util.log verbose (fn _ => "Optimised program:\n" ^ pp_prog prtype p)
    in p
    end

fun eval p v =
    let val de = Exp.addDE Exp.emptyDEnv main_arg_var v
        val v' = Exp.eval de p
    in v'
    end

fun readFile e = Op_e("readFile", [e])
fun readIntVecFile e = Op_e("readIntVecFile", [e])
fun readDoubleVecFile e = Op_e("readDoubleVecFile", [e])
    
end
