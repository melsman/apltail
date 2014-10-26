structure Tail : TAIL = struct

structure Exp = TailExp(TailType)

open TailType
open Exp
type Int = unit
type Double = unit
type 'a Num = unit
type Bool = unit
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
val leqi = binOp "leqi"
val eqi  = binOp "eqi"
val maxi = binOp' "maxi"
val mini = binOp' "mini"
fun negi x = Op_e("negi",[x])
fun absi x = Op_e("absi",[x])
val addd = binOp "addd"
val subd = binOp "subd"
val muld = binOp "muld"
val divd = binOp "divd"
val resd = binOp "resd"
val ltd  = binOp "ltd"
val leqd = binOp "leqd"
val eqd  = binOp "eqd"
val maxd = binOp' "maxd"
val mind = binOp' "mind"
fun negd x = Op_e("negd",[x])
fun absd x = Op_e("absd",[x])

val i2d = fn x => Op_e ("i2d", [x])
val op % = binOp "mod"

fun If (b,e1,e2) = Iff_e(b,e1,e2)

val fromList = fn _ => Vc_e

type 'a t = exp
type 'a v = exp
type 'a NUM = exp
type INT = exp
type DOUBLE = exp
type BOOL = exp
                
type 'a M = ('a -> exp) -> exp
                               
fun ret (v:'a) : 'a M = fn k => k v

infix >>=
fun (f : 'a M) >>= (g : 'a -> 'b M) : 'b M =
   fn k => f (fn x => g x k)

fun fromListM t x = ret(fromList t x)

(* Compiled Programs *)
type ('a,'b) prog = exp
fun runF _ f = f (Var("arg",TyVar())) (fn x => x)
 
(* Values and Evaluation *)
type 'a V = Exp.value
fun Iv _ = raise Fail "Tail.Iv"
fun unIv _ = raise Fail "Tail.unIv"
val Dv = Exp.Dvalue
val unDv = Exp.unDvalue
fun Bv _ = raise Fail "Tail.Bv"
fun unBv _ = raise Fail "Tail.unBv"
fun Vv _ = raise Fail "Tail.Vv"
fun unVv _ = raise Fail "Tail.unVv"
val Uv = Exp.Uvalue

local val c = ref 0
in fun newVar() = "v" ^ Int.toString (!c) before c := Int.+(!c,1)
end

type 'a MVec = unit
type 'a m = exp
fun zilde _ = Op_e("zilde",nil)
fun scl _ t = t
fun scalar t = Vc_e[t]
fun vec t = t
fun iota t = Op_e("iota",[t])
fun iota' t = Op_e("iota",[t])
fun first t = Op_e("first",[t])
fun siz t = Op_e("siz",[t])
fun dim t = Op_e("dim",[t])   (* or I(#2 t) *)
fun rav t = Op_e("rav",[t])
fun rav0 t = t
fun each t _ f e =
    let val v = newVar()
        val e0 = f (Var(v,t)) (fn x => x)
    in Op_e("each",[Fn_e(v,t,e0),e])
    end
fun mkFn2 t1 t2 f =
    let val (v1, v2) = (newVar(), newVar())
        val e0 = f (Var(v1,t1), Var(v2,t2))
    in Fn_e(v1,t1,Fn_e(v2,t2,e0))
    end
fun mkFn2m t1 t2 f = mkFn2 t1 t2 (fn a => f a (fn x=>x))
fun bin t1 t2 s f e1 e2 =
    ret(Op_e(s,[mkFn2 t1 t2 f,e1,e2]))
fun binm t1 t2 s f e1 e2 = 
    bin t1 t2 s (fn a => f a (fn x => x)) e1 e2
fun red t1 t2 f n e = binm t1 t2 "red" f n e
fun meq t f e1 e2 = bin t t "meq" f e1 e2
fun mif (b,e1,e2) = If(b,e1,e2)
fun zipWith t1 t2 _ f e1 e2 = binm t1 t2 "zipWith" f e1 e2
fun scan t1 t2 f e1 e2 = bin t1 t2 "scan" f e1 e2
fun getRank s e =
    let fun fail s = raise Fail ("rank error: " ^ s ^ 
                                 " not supported for arguments of unknown rank")
        val t = typeOf e
    in case unSi t of
           SOME _ => 0
         | NONE =>
       case unVi t of
           SOME _ => 1
         | NONE =>
       case unSh t of
           SOME _ => 1 
         | NONE => 
       case unArr t of
           SOME (_, r) => (case unRnk r of SOME i => i
                                         | NONE => fail s)
         | NONE => fail s
    end
fun catenate e1 e2 =
    let fun cat () = ret(Op_e("cat", [e1,e2]))
        fun cons () = ret(Op_e("cons",[e1,e2]))
        fun snoc () = ret(Op_e("snoc",[e1,e2]))
        open Int
    in case (getRank "cat" e1, getRank "cat" e2) of
           (0, 0) => ret(Vc_e[e1,e2])
         | (r1, r2) => if r2=r1+1 then cons()
                       else if r1=r2+1 then snoc()
                       else if r1=r2 then cat()
                       else raise Fail ("rank error: incompatible argument ranks for catenate: " 
                                        ^ Int.toString r1 ^ " and " ^ Int.toString r2)
    end
fun take e1 e2 = Op_e("take", [e1,e2])
fun drop e1 e2 = Op_e("drop", [e1,e2])
fun mem e = ret(Op_e("mem",[e]))
fun rotate e1 e2 = Op_e("rotate", [e1,e2])
fun reshape e1 e2 = ret(Op_e("reshape", [e1,e2]))
fun shape e = Op_e("shape",[e])
fun prod t f g e m1 m2 s a =
    let open Int
        val r = case (getRank "prod" m1, getRank "prod" m2) of
                    (0,_) => raise Fail "rank error: prod1"
                  | (_,0) => raise Fail "rank error: prod2"
                  | (r1,r2) => r1+r2-2
        val res = Op_e("prod",[mkFn2m t t f,mkFn2m t t g,e,m1,m2])
    in if r < 0 then raise Fail "rank error: prod3"
       else if r = 0 then ret(s res)
       else ret(a res)
    end
fun outer t1 t2 f e1 e2 = binm t1 t2 "outer" f e1 e2
fun reduce t f e1 e2 s a =
    let open Int
    in case getRank "reduce" e2 of
           0 => ret(s e2)
         | r => binm t t "reduce" f e1 e2 >>= 
                     (fn e => ret(if r=1 then s e else a e))
    end
fun transpose e = Op_e("transp", [e])
fun transpose2 e1 e2 = Op_e("transp2", [e1,e2])
fun reverse e = Op_e("reverse", [e])
fun catenate_first e1 e2 =
    catenate (transpose e1) (transpose e2) >>= (ret o transpose)
fun lett _ e = 
    let val v = newVar()
        val t = typeOf e
    in fn f => Let_e(v,t,e,f(Var(v,t)))
    end

fun letm _ e =
    let val v = newVar()
        val t = typeOf e
    in fn f => Let_e(v,t,e,f(Var(v,t)))
    end

val letm_asgn = letm

(* Optimization *)

structure M = StringFinMap
structure Optimize = struct

type def = {shape: Exp.exp option, value: Exp.exp option}
type env = def M.map

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
      | ("addi", [I i1,I i2]) => I(Int.+(i1,i2))
      | ("subi", [I i1,I i2]) => I(Int.-(i1,i2))
      | ("subi", [e,I 0]) => e
      | ("negi", [I i]) => I(Int.~ i)
      | ("absi", [I i]) => I(Int.abs i)
      | ("i2d", [I i]) => D(real i)
      | ("reduce", [f,n,Op("zilde",[],_)]) => n
      | ("reverse", [Vc(es,t)]) => Vc(rev es, t)
      | ("shape", [e]) => (case getShape E e of
                               SOME e => e
                             | NONE => Op (opr,[e],t))
      | ("shapeSh", [e]) => (case getShape E e of
                                 SOME e => e
                               | NONE => Op (opr,[e],t))
      | ("dropSh", [I n,Vc(es,_)]) =>
        if n >= 0 andalso n < length es then Vc(List.drop(es,n),t)
        else if n < 0 andalso ~n < length es then Vc(List.take(es,length es + n),t)
        else Op(opr,es,t)
      | ("takeSh", [I n,Vc(es,_)]) =>
        if n >= 0 andalso n < length es then Vc(List.take(es,n),t)
        else if n < 0 andalso ~n < length es then Vc(List.drop(es,length es + n),t)
        else Op(opr,es,t)
      | ("firstSh", [Vc(e::es,t)]) => e
      | ("first", [Vc(e::es,t)]) => e
      | ("transp2", [Vc([I 2,I 1],_),e]) => Op("transp", [e],t)
      | ("catSh", [Vc(es1,_),Vc(es2,_)]) => Vc(es1@es2,t)
      | ("snocSh", [Vc(es,_),e]) => Vc(es@[e],t)
      | ("iotaSh",[I n]) => if n <= 3 then Vc(List.map I (List.tabulate (n,fn x => x+1)),t)
                            else Op(opr,es,t)
      | ("rotateSh", [I n,Vc(es,_)]) => Vc(rot n es,t)
      | _ => Op(opr,es,t)
               
and getShape (E:env) (e : Exp.exp) : Exp.exp option =
    let fun tryType() =
            case unSh (typeOf e) of
                NONE => NONE
              | SOME r => case unRnk r of
                              NONE => NONE
                            | SOME i => SOME (Vc([I i],Vi r))
    in case tryType() of
           SOME e => SOME e
         | NONE =>
       case e of
           Op("reshape",[sh,e],_) => SOME sh
         | Var(v,_) => (case M.lookup E v of
                            SOME{shape=SOME sh,...} => SOME sh
                          | _ => NONE)
         | Vc(es,_) => SOME(Vc([I(length es)],Vi(rnk(length es))))
         | Op("transp", [e], _) =>
           (case getShape E e of
                SOME sh => SOME(peepOp E ("reverse",[sh],typeOf sh))
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
                Var (v,_) => (case M.lookup E v of
                                  SOME{value=SOME e,...} => e
                                | _ => e)
              | I i => e
              | D r => e
              | B b => e
              | Iff (c,e1,e2,t) => Iff(opt E c,opt E e1,opt E e2,t)
              | Vc(es,t) => Vc (opts E es,t)
              | Op(opr,es,t) => peepOp E (opr,opts E es,t)
              | Let (v,ty,e1,e2,t) => 
                let val e1 = opt E e1
                in if simple e1 then
                     let val E' = M.add(v,{shape=NONE,value=SOME e1},E)
                     in opt E' e2
                     end
                   else 
                     let val sh = getShape E e1
                         val E' = M.add(v,{shape=sh,value=NONE},E)
                         val e2 = opt E' e2
                     in Let(v,ty,e1,e2,t)
                     end
                end
              | Fn (v,t,e,t') => 
                let val E' = M.add(v,{shape=NONE,value=NONE},E)
                in Fn(v,t,opt E' e,t')
                end
        and opts E es = List.map (opt E) es
        val initE = M.empty
    in opt initE e
    end
end

(* Pretty printing *)

fun prInstanceLists opr es t =
    let fun unArr' at =       (* return the base type and the rank of an array *)
            case unArr at of
                SOME p => p
              | NONE => case unSh at of
                            SOME _ => (IntB, T.rnk 1)
                          | NONE => case unVi at of
                                        SOME _ => (IntB, T.rnk 1)
                                      | NONE => case unSi at of
                                                    SOME _ => (IntB, T.rnk 0)
                                                  | NONE => raise Fail ("Tail.unArr': " ^ opr)
            
        val rnk = #2 o unArr'  (* return the rank of an array *)
        val bt  = #1 o unArr'  (* return the base type of an array type *)
        val ts = List.map typeOf es
        fun wrap il1 il2 = "{[" ^ String.concatWith "," (List.map prBty il1) ^ "],[" ^
                           String.concatWith "," (List.map prRnk il2) ^"]}"
        val none = ""
    in case (opr, ts) of
           ("each", [tf,ta]) => wrap [bt ta,bt t] [rnk t]
         | ("reduce", [tf,te,ta]) => wrap [bt te] [rnk t]
         | ("shape", [ta]) => wrap [bt ta] [rnk ta] 
         | ("take", [_,ta]) => wrap [bt ta] [rnk ta]
         | ("drop", [_,ta]) => wrap [bt ta] [rnk ta] 
         | ("cat", [t1,t2]) => wrap [bt t1] [rnk t1]
         | ("cons", [t1,t2]) => wrap [bt t1] [rnk t1]
         | ("snoc", [t1,t2]) => wrap [bt t1] [rnk t2]
         | ("rotate", [_,t2]) => wrap [bt t2] [rnk t2]
         | ("first", [ta]) => wrap [bt ta] [rnk t]
         | ("transp", [ta]) => wrap [bt ta] [rnk ta]
         | ("transp2", [_,ta]) => wrap [bt ta] [rnk ta]
         | ("reshape", [_,ta]) => wrap [bt t] [rnk ta,rnk t]
         | ("zipWith", [ft,t1,t2]) => wrap [bt t1,bt t2,bt t] [rnk t1]
         | ("prod",[ft,gt,et,t1,t2]) => none
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
                Var (v,_) => $v
              | I i => $(Int.toString i)
              | D r => $(Real.fmt (StringCvt.FIX (SOME 2)) r)
              | B b => $(Bool.toString b)
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
                | Let (v,ty,e1,e2,_) => $"let " @@ $v @@ $":" @@ $(prType ty) @@ $" = " @@ pp (i+2) e1 @@ $" in" @@ 
                                         indent i @@ pp i e2
                | Fn (v,t,e,_) =>
                  (case lookForOp [v] e of
                       SOME (opr,t) => $opr
                     | NONE => $("fn " ^ v ^ ":" ^ prType t ^ " => ") @@ pp (i+2) e)
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
                   raise Fail msg
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
    let val de = Exp.addDE Exp.empDEnv "arg" v
        val v' = Exp.eval de p
    in v'
    end
    
end
