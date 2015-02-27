(* Pull vectors *)

structure Laila :> LAILA = struct

structure P = Program

val optimisationLevel = IL.optimisationLevel
val enableComments = ref false
val unsafeAsserts = ref false

fun die s = raise Fail ("Laila." ^ s)

infixr $
val op $ = Util.$

type 'a M = 'a * (P.ss -> P.ss)

type t = P.e

type value = IL.value

infix >>= ::=
val op := = P.:=

fun (v,ssT) >>= f = let val (v',ssT') = f v in (v', ssT o ssT') end
fun ret v = (v, fn ss => ss)

fun runM0 ((e,ssT) : 'a M) (k : 'a -> P.s) : P.ss =
    ssT [k e]

fun runMss ((e,ssT) : 'a M) (k : 'a -> P.ss) : P.ss =
    ssT (k e)

fun assert s b (m : 'a M) : 'a M  =
    if !unsafeAsserts then m
    else let val (v, ssT) = m
         in (v, fn ss => ssT(P.Ifs(b,[],[P.Halt s]) ss))
         end

fun comment s : unit M =
    if !enableComments then
      ((), fn ss => P.Comment s :: ss)
    else ((), fn ss => ss)

fun ifM t (b,m1,m2) =
    case P.unB b of
        SOME true => m1
      | SOME false => m2
      | NONE => 
        let val n = Name.new t
        in (P.Var n, 
            fn ss => P.Decl(n, NONE) ::
                     P.Ifs(b,
                           runMss m1 (fn v => [n := v]),
                           runMss m2 (fn v => [n := v])) ss)
        end

fun ifUnit (b,m1,m2) =
    case P.unB b of
        SOME true => m1
      | SOME false => m2
      | NONE => 
        ((), 
         fn ss => P.Ifs(b,
                        runMss m1 (fn () => []),
                        runMss m2 (fn () => [])) ss)

open Type
type INT     = t
type DOUBLE  = t
type BOOL    = t
type CHAR    = t

val I : Int32.int -> INT = P.I
val D : real -> DOUBLE = P.D
val B : bool -> BOOL = P.B
val C : word -> CHAR = P.C

fun lettWithName e =
    let open P
        val ty = ILUtil.typeExp e
        val name = Name.new ty
        fun ssT ss = Decl(name, SOME e) :: ss
    in ((Var name,name), ssT)
    end

fun lett e =
    let open P
    in if simpleExp e then ret e
       else
         let val ty = ILUtil.typeExp e
             val name = Name.new ty
             fun ssT ss = Decl(name, SOME e) :: ss
         in (Var name, ssT)
         end
    end

fun for (n:t) (f:INT->unit M) : unit M =
    let open P
        val name = Name.new Int
    in lett n >>= (fn n =>
       ((), For(n, name, runMss (f(Var name)) (fn () => nil))))
    end

fun asgnArr (n:Name.t,i:t,v:t) : unit M =
    let open P
    in ((), fn ss => ((n,i) ::= v) :: ss)
    end

fun assign (n:Name.t) (v:t) : unit M =
    let open P
    in ((), fn ss => (n := v) :: ss)
    end

fun alloc0 ty (n:t) : Name.t M =
    let open P
        val tyv = Type.Vec ty
        val name = Name.new tyv
        val (sz, store0) = if ty = Type.Char then 
                             (addi(n,I 1), fn ss => ((name,n) ::= C 0w0) :: ss)
                           else (n, fn ss => ss)
        fun ssT ss = Decl(name, SOME(Alloc(tyv,sz))) :: store0 ss
    in (name, ssT)
    end

fun alloc ty (n:t) : ((t -> t M) * (t * t -> unit M)) M =
    alloc0 ty n >>= (fn name =>
    let fun read i = ret (P.Subs(name,i))
        fun write (i,v) = asgnArr (name,i,v)
    in ret (read,write)
    end)

(* Vectors *)

datatype v = V of IL.Type * t * (t -> t M)

fun simple f =
    let open P 
        val v = Name.new Int
        val (e,ssT) = f (Var v)
    in case ssT nil of
           nil => simpleIdx v e
         | _ => false
    end

fun materialize (V(ty,n,f)) =
    let open P
        val tyv = Type.Vec ty
        val name = Name.new tyv
        val name_n = Name.new Int
        val name_i = Name.new Int
        val (sz, store0) = if ty = Type.Char then 
                             (addi(n,I 1), fn ss => ((name,Var name_n) ::= C 0w0) :: ss)
                           else (n, fn ss => ss)
        fun ssT ss = Decl(name_n, SOME n) ::
                     Decl(name, SOME(Alloc(tyv,sz))) ::
                     (For(Var name_n, name_i, runM0(f(Var name_i))(fn v => (name,Var name_i) ::= v)) (store0 ss))
    in ((name,name_n), ssT)
    end

fun materializeWithName (v as V(ty,n,_)) =
    let val ((name,name_n),ssT) = materialize v
    in ((V(ty,P.Var name_n, fn i => ret(P.Subs(name,i))), name, name_n), ssT)
    end

fun memoize (t as V(ty,n,f)) =
    if simple f then ret t
    else materializeWithName t >>= (fn (v, _, _) => ret v)

val letm = ret

local open P
in
  fun map ty f (V(_,n,g)) = V(ty, n, fn i => g i >>= f)

  fun map2unsafe ty f (V(_,n1,f1)) (V(_,n2,f2)) =  (* assumes n1=n2 *)
      V(ty, n1, fn i => f1 i >>= (fn v1 => f2 i >>= (fn v2 => f(v1,v2))))

(*
  fun map2 ty f (V(_,n1,f1)) (V(_,n2,f2)) =
      V(ty, 
        mini(n1,n2), fn i => 
                      f1 i >>= (fn v1 =>
                      f2 i >>= (fn v2 =>
                      f(v1,v2))))
*)

  fun rev (V(ty,n,g)) = V(ty,n, fn i => g(subi(subi(n,i),I 1)))

  fun tabulate ty t f = V(ty,t,f)

  fun proto t =
        if t = Int then I 0
        else if t = Double then D 0.0
        else if t = Bool then B false
        else if t = Char then C 0w32
        else die ("proto: unsupported type " ^ prType t)

  fun dummy_exp t =
      if t = Int then I 666
      else if t = Double then D 66.6
      else if t = Bool then B false
      else die ("empty: unknown type " ^ prType t)

  fun empty ty = V(ty, I 0, fn _ => ret(dummy_exp ty))

  fun emptyOf (V(ty,_,f)) = V(ty, I 0, f)

  fun single t = V(ILUtil.typeExp t, I 1, fn _ => ret t)

  fun tk t (V(ty,m,g)) = V(ty,mini(t,m), g)
  fun tk_unsafe t (V(ty,m,g)) = V(ty,t,g)   (* assumes 0 <= t <= m *)
             
  fun dr t (V(ty,m,g)) = V(ty,maxi(subi(m,t),I 0), fn i => g(addi(i,t)))
  fun dr_unsafe t (V(ty,m,g)) = V(ty,subi(m,t), fn i => g(addi(i,t)))  (* assumes 0 <= t <= m *) 

  fun length (V(_,n,_)) = n

  fun appi (g: INT -> t -> unit M) (V(ty,n,f):v) : unit M =
      let fun h i = f i >>= (fn v => g i v)
      in lett n >>= (fn n => for n h)
      end 
end

val addi  : INT * INT -> INT = P.addi
val subi  : INT * INT -> INT = P.subi
val muli  : INT * INT -> INT = P.muli
val divi  : INT * INT -> INT = P.divi
val modi  : INT * INT -> INT = P.modi
val resi  : INT * INT -> INT = P.resi
val maxi  : INT * INT -> INT = P.maxi
val mini  : INT * INT -> INT = P.mini
val negi  : INT -> INT = P.negi
val lti   : INT * INT -> BOOL = P.lti
val ltei  : INT * INT -> BOOL = P.ltei
val gti   : INT * INT -> BOOL = P.gti
val gtei  : INT * INT -> BOOL = P.gtei
val eqi   : INT * INT -> BOOL = P.eqi
val neqi  : INT * INT -> BOOL = P.neqi

val ori   : INT * INT -> INT = P.ori
val andi  : INT * INT -> INT = P.andi
val xori  : INT * INT -> INT = P.xori
val shli  : INT * INT -> INT = P.shli
val shri  : INT * INT -> INT = P.shri
val shari : INT * INT -> INT = P.shari

val addd  : DOUBLE * DOUBLE -> DOUBLE = P.addd
val subd  : DOUBLE * DOUBLE -> DOUBLE = P.subd
val muld  : DOUBLE * DOUBLE -> DOUBLE = P.muld
val divd  : DOUBLE * DOUBLE -> DOUBLE = P.divd
val ltd   : DOUBLE * DOUBLE -> BOOL = P.ltd
val lted  : DOUBLE * DOUBLE -> BOOL = P.lted
val gtd   : DOUBLE * DOUBLE -> BOOL = P.gtd
val gted  : DOUBLE * DOUBLE -> BOOL = P.gted
val eqd   : DOUBLE * DOUBLE -> BOOL = P.eqd
val neqd  : DOUBLE * DOUBLE -> BOOL = P.neqd
val maxd  : DOUBLE * DOUBLE -> DOUBLE = P.maxd
val mind  : DOUBLE * DOUBLE -> DOUBLE = P.mind
val powd  : DOUBLE * DOUBLE -> DOUBLE = P.powd
val negd  : DOUBLE -> DOUBLE = P.negd
val ln    : DOUBLE -> DOUBLE = P.ln
val sin   : DOUBLE -> DOUBLE = P.sin
val cos   : DOUBLE -> DOUBLE = P.cos
val tan   : DOUBLE -> DOUBLE = P.tan
val floor : DOUBLE -> INT = P.floor
val ceil  : DOUBLE -> INT = P.ceil
val pi    : DOUBLE = D Math.pi
val roll  : INT -> DOUBLE = P.roll

val eqc   : CHAR * CHAR -> BOOL = P.eqc

val eqb   : BOOL * BOOL -> BOOL = P.eqb
val neqb  : BOOL * BOOL -> BOOL = P.neqb
val andb  : BOOL * BOOL -> BOOL = P.andb
val orb   : BOOL * BOOL -> BOOL = P.orb
val xorb  : BOOL * BOOL -> BOOL = P.xorb
val notb  : BOOL -> BOOL = P.notb

val i2d  : INT -> DOUBLE = P.i2d
val d2i  : DOUBLE -> INT = P.d2i
val b2i  : BOOL -> INT = P.b2i

fun printf (s, es) = ((), fn ss => P.Printf(s,es)::ss)
fun sprintfV (s, es) = 
    let val sz = size s + 25 * List.length es
        val ty = Type.Char
        val tyv = Type.Vec ty
        val name = Name.new tyv
        fun ssT ss = P.Decl(name, SOME(P.Alloc(tyv,P.I sz))) :: 
                     P.Sprintf(name,s,es) :: ss
    in (V(ty,P.strlen (P.Var name), fn i => ret(P.Subs(name,i))), ssT)
    end

(* Values and Evaluation *)
type value   = IL.value
val Iv       = IL.IntV
val unIv     = fn IL.IntV i => i | _ => die "unIv"
val Dv       = IL.DoubleV
val unDv     = fn IL.DoubleV d => d | _ => die "unDv"
val Bv       = IL.BoolV
val unBv     = fn IL.BoolV b => b | _ => die "unBv"
fun Vv vs    = IL.ArrV(Vector.fromList(List.map (fn v => ref(SOME v)) vs))
fun vlist v = Vector.foldl (op ::) nil v
val unVv     = fn IL.ArrV v => List.map (fn ref (SOME a) => a
                                          | _ => die "unVv.1") (vlist v)
                | _ => die "unVv"
val Uv       = Iv 0
val ppV      = ILUtil.ppValue 

fun pr_wrap s f a =
    let (* val () = print("[starting " ^ s ^ "]\n") *)
        val r = f a
        (* val () = print("[finished " ^ s ^ "]\n") *)
    in r
    end

fun se_ss ss = pr_wrap "se_ss" (P.se_ss nil) ss
fun rm_decls0 ss = pr_wrap "rm_decls0" P.rm_decls0 ss
fun rm_decls p = pr_wrap "rm_decls" (fn (e,ss) => P.rm_decls e ss) p

(* Some utility functions *)
fun opt_ss0 ss =
    let val ss = se_ss ss
        val ss = se_ss ss
        val ss = se_ss ss
    in rm_decls0 ss
    end

fun opt_ss e ss =
    let val ss = se_ss ss
        val ss = se_ss ss
    in rm_decls (e,ss)
    end

type prog = Type.T * Type.T * (Name.t * (P.e -> P.s) -> P.ss)

fun pp_prog ((ta,tb,p): prog) : string =
    let val name_arg = Name.new ta
(*        val () = print "generating laila program\n" *)
        val ss = p (name_arg, P.Ret)
(*        val () = print "optimizing laila program\n" *)
        val ss = opt_ss0 ss
(*        val () = print "printing laila program\n" *)
    in ILUtil.ppFunction "kernel" (ta,tb) name_arg ss
    end

fun eval ((ta,tb,p): prog) (v: value) : value =
    let val name_arg = Name.new ta
        val ss = p (name_arg, P.Ret)
        val ss = opt_ss0 ss

        val () = print (ILUtil.ppFunction "kernel" (ta,tb) name_arg ss)
(*
        val () = print ("Program(" ^ Name.pr name_arg ^ ") {" ^ 
                        ILUtil.ppProgram 1 program ^ "\n}\n")
*)
        val name_res = Name.new tb
        val env0 = ILUtil.add ILUtil.emptyEnv (name_arg,v)
        val env = ILUtil.evalSS env0 ss name_res      
    in case ILUtil.lookup env name_res of
         SOME v => v
       | NONE => die ("Error finding '" ^ Name.pr name_res ^ 
                      "' in result environment for evaluation of\n" ^
                      ILUtil.ppSS 0 ss)
    end

fun runF (ta,tb) (f: t -> t M) =
    (ta,
     tb,
     fn (n0,k) =>
        let val (e,ssT) = f (IL.Var n0)
        in ssT [k e]
        end)

fun runM _ ta (e,ssT) =
  (Type.Int,
   ta,
   fn (_,k) => runM0 (e,ssT) k)

fun typeComp s c =
    let val (e,_) = c
    in ILUtil.typeExp e
    end

val Var = P.Var

fun get_exp (m1 as (e,f)) =
    let val ss = f nil
        val ss = opt_ss e ss
    in case ss of
         nil => SOME e
       | _ =>
         let (*val () = print("no_get:" ^ ILUtil.ppSS 0 ss ^ "\n")*)
         in NONE
         end
    end

fun If0 (x,m1,m2) =
    let val t = typeComp "If" m1
        val n = Name.new t
    in case (get_exp m1, get_exp m2) of
         (SOME e1,SOME e2) => 
         (Var n, fn ss => P.Decl(n,SOME(P.If(x,e1,e2)))::ss)
       | _ =>
         let val k = fn v => n := v
             val s1 = runM0 m1 k
             val s2 = runM0 m2 k
             val s1 = opt_ss0 s1
             val s2 = opt_ss0 s2
             fun default() =
                 (Var n, fn ss => P.Decl(n,NONE)::P.Ifs(x,s1,s2) ss)
         in case (s1, s2) of
              ([IL.Assign(n1,e1)],[IL.Assign(n2,e2)]) =>
              if n = n1 andalso n = n2 then
                (Var n, fn ss => P.Decl(n,SOME(P.If(x,e1,e2)))::ss)
              else default()
            | _ => default()
         end
    end

fun If (x,a1,a2) = P.If(x,a1,a2)

fun Ifv (x, v1 as V(ty,n1,f1), v2 as V(_,n2,f2)) =
    case P.unB x of
        SOME true => v1
      | SOME false => v2
      | NONE => 
        V(ty,P.If(x,n1,n2), 
          fn i =>
             let val m1 = f1 i
                 val m2 = f2 i
             in If0(x,m1,m2)
             end)
           
fun foldl (f: t * t -> t M) (e:t) (V(_,n,g)) =
    lettWithName e >>= (fn (a,name) =>
    for n (fn i => g i >>= (fn v => f(a,v) >>= assign name)) >>= (fn () => 
    ret a))

fun concat v1 v2 =
    let val V(ty,n1,f1) = v1
        val V(_,n2,f2) = v2
    in case P.unI n1 of
           SOME 0 => v2
         | _ =>
           case P.unI n2 of
               SOME 0 => v1
             | _ => V(ty,P.addi(n1,n2), 
                      fn i => 
                         let val m1 = f1 i
                             val m2 = f2 (P.subi(i,n1))
                         in If0(P.lti(i,n1),m1,m2)
                         end)
    end
   
  fun fromList nil = empty Int
    | fromList [t] = single t
    | fromList (t::ts) = concat (single t) (fromList ts)

  fun fromListMV ty nil = ret(empty Int)
    | fromListMV ty [t] = ret(single t)
    | fromListMV ty ts =
      let open P
          val tyv = Type.Vec ty
          val name = Name.new tyv
          val sz = I(List.length ts)
          fun ssT ss = Decl(name, SOME(Vect(tyv,ts))) :: ss
      in (V(ty,sz, fn i => ret(Subs(name,i))), ssT)
      end

(*
  fun eq f v1 v2 =
      let val v = map2 Bool (ret o f) v1 v2
          val base = eqi(length v1,length v2)
      in foldl (fn (b,a) => ret(If(a,b,a))) base v
      end
*)

  fun sub_unsafe (V(_,n,g)) i = g i

  fun tyOfV (V(ty,_,_)) = ty

  fun lprod nil = I 1
    | lprod (x::xs) = muli(x,lprod xs)
  
  (* reverse mul prescan; rprod [2, 3, 5] = [15,5,1] *)
  fun rprod [x] = ret [I 1]
    | rprod xs = 
      let fun prods nil acc = ret acc
            | prods [x] acc = ret acc
            | prods (x::xs) nil = prods xs [x,I 1]
            | prods (x::xs) (acc as y:: _) =
              lett (muli(x,y)) >>= (fn m => prods xs (m::acc))
      in prods (List.rev xs) nil
      end

  fun toSh sh i =
      let fun toSh' ps sh i =
              case (ps, sh) of
                  (nil, nil) => ret nil
                | (_, [_]) => ret [i]
                | (p::ps, _ ::xs) =>
                  lett (modi(i,p)) >>= (fn imodp =>
                  lett (divi(i,p)) >>= (fn x' =>
                  toSh' ps xs imodp >>= (fn xs' =>
                  ret (x' :: xs'))))
                | _ => die "toSh"
      in comment "toSh" >>= (fn () =>
         lett i >>= (fn i =>
         rprod sh >>= (fn ps =>
         toSh' ps sh i)))
      end

(*
  fun toSh nil i = ret nil
    | toSh (x::xs) i =
      lett (lprod xs) >>= (fn p =>
      lett (modi(i,p)) >>= (fn imodp =>
      lett (divi(i,p)) >>= (fn x' =>
      toSh xs imodp >>= (fn xs' =>
      ret (x' :: xs')))))
*)

  fun fromSh sh idx =
      let fun fromSh' ps nil nil = ret(I 0)
            | fromSh' (p::ps) (_::sh) (i::idx) = 
              fromSh' ps sh idx >>= (fn x =>
              lett (addi(muli(i,p),x)))
            | fromSh' _ _ _ = die "fromSh: dimension mismatch"
      in comment "fromSh" >>= (fn () =>
         rprod sh >>= (fn ps =>
         fromSh' ps sh idx))
      end

(*
  fun fromSh nil nil = ret(I 0)
    | fromSh (_::sh) (i::idx) = 
      fromSh sh idx >>= (fn x =>
      lett (addi(muli(i,lprod sh),x)) >>= (fn res =>
      ret res))
    | fromSh _ _ = die "fromSh: dimension mismatch"
*)

  fun getShape 0 f = ret nil
    | getShape n f = 
      f (P.I (n-1)) >>= (fn N =>
      getShape (n-1) f >>= (fn NS =>
      ret (NS @ [N])))

  fun getShapeV s (V(_,n,f)) =
      case P.unI n of
          SOME n => getShape n f
        | NONE => die ("getShapeV: " ^ s ^ ". Expecting static shape")
(*
  fun trans v d =
      let val V(_,n,f) = v
          val V(ty,m,g) = d
          fun h n i =
              comment "trans" >>= (fn () =>
              getShape n f >>= (fn sh => 
              let val sh' = List.rev sh
              in toSh sh' i >>= (fn sh'' =>
                 fromSh sh (List.rev sh'') >>= g)
              end))
      in case P.unI n of
             SOME 0 => ret d   (* known number of dimensions *)
           | SOME 1 => ret d
           | SOME n => ret (V(ty,m,h n))
           | NONE => (*ret(If(E n < I 2, d, V(ty,m,g')))*)
             die "trans: unknown number of dimensions not supported"
      end
*)

  fun exchange nil xs = nil
    | exchange (i::rest) xs = List.nth (xs,i-1) :: exchange rest xs

  fun appi0 _ f nil = ()
    | appi0 n f (x::xs) = (f (x,n); appi0 (n+1) f xs)
  
  fun exchange' (ctrl:int list) (xs: 'a list) : 'a list =
      let val sz = List.length ctrl
          val a = Array.tabulate (sz,fn _ => List.nth(xs,0))
      in appi0 0 (fn (c,i) => Array.update(a,c-1,List.nth(xs,i))) ctrl
       ; Array.foldr(op::) nil a
      end
(*          
  fun trans2 idxs sh vs =
      let fun check n =
              if n = 0 then ()
              else if List.exists (fn x => x = n) idxs then
                check (n-1)
              else die "trans2: index vector not a permutation"
          val V(_,n,f) = sh
          val V(ty,m,g) = vs
      in check (List.length idxs)
       ; case P.unI n of
             SOME n =>  (* known number of dimensions *)
             (if n <> List.length idxs then
                die "trans2: wrong index vector length"
              else if n<2 then ret (sh,vs)
              else 
                 comment "trans2" >>= (fn () =>
                 getShape n f >>= (fn sh =>
                 let val sh' = exchange' idxs sh
                     fun h i =
                         toSh sh' i >>= (fn sh'' =>
                         fromSh sh (exchange idxs sh'') >>= (fn x => g x))
                 in ret (fromList sh',
                         V(ty,m,h))
                 end)))
           | NONE => die "trans2: unknown number of dimensions not supported"
      end
*)

  fun compr bv v =
      let val V(_,n,f) = bv
          val V(ty,m,g) = v
      in comment "compr" >>= (fn () =>
         foldl (ret o addi) (I 0) (map Int (ret o b2i) bv) >>= (fn sz =>
         alloc ty sz >>= (fn (rd,wr) =>
         lettWithName (I 0) >>= (fn (count,count_name) =>
         for n (fn i => f i >>= (fn b => ifUnit(b, g i >>= (fn v => 
                                                     wr(count,v) >>= (fn () =>
                                                     assign count_name (addi(count,I 1)))),
                                                   ret ()))) >>= (fn () =>
         ret (V(ty,sz,rd)))))))
      end

  fun absi x = If(lti(x, I 0), negi x, x)
  fun absd x = If(ltd(x, D 0.0), negd x, x)

  fun repl def iv v =
      let val V(_,n,f) = iv
          val V(ty,m,g) = v
      in comment "repl" >>= (fn () =>
         foldl (ret o addi) (I 0) (map Int (ret o absi) iv) >>= (fn sz =>
         alloc ty sz >>= (fn (rd,wr) =>
         lettWithName (I 0) >>= (fn (count,count_name) =>
         for n (fn i => f i >>= (fn r => 
                        g i >>= (fn v =>
                        for r (fn _ => wr(count,v) >>= (fn () =>    (* MEMO: if r < 0 we should output ~r def elements *)
                                       assign count_name (addi(count,I 1))))))) >>= (fn () =>
         ret (V(ty,sz,rd)))))))
      end

  fun extend n (V(ty,m,f)) =
      Ifv(eqi(m,I 0), V(ty,n, fn _ => ret (proto ty)), V(ty,n, fn i => comment "extend" >>= (fn () => lett (P.modi(i,m)) >>= f)))

  fun outmain outln =
    ( outln "int main() {"
    ; outln "  initialize();"
    ; outln "  prScalarDouble(kernel(0));"
    ; outln "  printf(\"\\n\");"
    ; outln "  return 0;"
    ; outln "}")

  fun outprog ofile p =
    let val body = pp_prog p
        val os = TextIO.openOut ofile
        fun outln s = TextIO.output (os, s^"\n")
    in outln "#include <stdio.h>"
     ; outln "#include <stdlib.h>"
     ; outln "#include <math.h>"
     ; outln "#include <string.h>"
     ; outln "#include <apl.h>"
     ; outln body
     ; outmain outln
     ; TextIO.closeOut os
     ; print ("Wrote file " ^ ofile ^ "\n")
    end

fun resd (x,y) = die "resd not yet supported"
fun signi x = If(lti(x,I 0),I ~1,I 1)
fun signd x = If(ltd(x,D 0.0),I ~1,I 1)

structure Shape : sig
  type t = v
  val concat   : t -> t -> t
  val single   : INT -> t
  val singlez  : t
  val empty    : t
  val product  : t -> INT M
  val length   : t -> INT
  val dr       : INT -> t -> t
  val tk       : INT -> t -> t
  val dr_unsafe: INT -> t -> t
  val tk_unsafe: INT -> t -> t
  val eq       : t -> t -> BOOL M
  val fromShList : INT list -> t
end = struct
  type t = v
  val concat   = concat
  val empty    = empty Int
  val singlez  = single (I 0)
  val single   = single
  fun product (t:t) : INT M = 
      (getShapeV "Shape.product" t) >>= (lett o lprod)  (*was: foldl (ret o muli) (I 1)*)
  val length   : t -> INT = length
  val dr       : INT -> t -> t = dr
  val tk       : INT -> t -> t = tk
  val dr_unsafe: INT -> t -> t = dr_unsafe
  val tk_unsafe: INT -> t -> t = tk_unsafe
  fun eq_elems nil nil = B true
    | eq_elems (x::xs) (y::ys) = andb(eqi(x,y),eq_elems xs ys)
    | eq_elems _ _ = B false
  fun eq (t1: t) (t2: t) : BOOL M =
      getShapeV "Shape.eq1" t1 >>= (fn sh1 =>
      getShapeV "Shape.eq2" t2 >>= (fn sh2 =>
      ret (eq_elems sh1 sh2)))
  fun fromShList sh = List.foldr (fn (x,a) => concat (single x) a) empty sh
end

type sh = INT list                     (* Shapes are lists at the meta level*)
datatype idx = N of sh -> t M          (* Nested representation *)
             | F of INT -> t M         (* Flat representation *)
datatype m = Arr of Type.T * sh * idx

fun ArrF(ty,sh,f) = Arr(ty,sh,F f)
fun ArrN(ty,sh,f) = Arr(ty,sh,N f)

fun toF0(Arr(ty,sh,F f)) = (ty,sh,f)
  | toF0(Arr(ty,sh,N f)) = (ty,sh, fn i => toSh sh i >>= f)
fun toN0 (Arr(ty,sh,N f)) = (ty,sh,f)
  | toN0 (Arr(ty,sh,F f)) = (ty,sh, fn is => fromSh sh is >>= f)

val toF = ArrF o toF0
val toN = ArrN o toN0

fun toV a =
    let val (ty,sh,f) = toF0 a
    in lett (lprod sh) >>= (fn sz =>
       ret (V(ty,sz,f)))
    end

fun vec (V(ty,n,f)) = ArrF(ty,[n],f)
fun fromListM ty l = fromListMV ty l >>= (ret o vec)
fun enclose t = vec (single t)
fun scl ty t = ArrF(ty,[], fn _ => ret t)
fun first a =    
    toV a >>= (fn v =>
    let fun first_unsafe (V(_,_,f)) = f (I 0)
        fun maybe_pad (v as V(ty,n,f)) =
            Ifv(gti(n,I 0), v, V(ty,I 1, fn _ => ret(proto ty)))
    in first_unsafe(maybe_pad v)
    end)
fun zilde ty = vec (empty ty)
fun iota0 n = tabulate Int n (fn x => ret(addi(x,I 1)))
fun iota n = vec (iota0 n)
fun shapeV (Arr(_,sh,_)) = Shape.fromShList sh
fun shape a = vec (shapeV a)
fun rank (Arr(_,sh,_)) = I(List.length sh)

fun dimincr (Arr(ty,sh,F f)) = ArrF(ty,sh @ [I 1], f)
  | dimincr (Arr(ty,sh,N f)) = ArrN(ty,sh @ [I 1], fn ix => case List.rev ix of
                                                                nil => die "dimincr error"
                                                              | _ :: rix => f(List.rev rix))

fun mem (a as Arr(ty,sh,_)) =
    toV a >>= (fn v =>
    memoize v >>= (fn V(_,_,f) => ret (ArrF(ty,sh,f))))

fun materializeN (ty,sh,f) =
    lett (lprod sh) >>= (fn sz =>
    alloc0 ty sz >>= (fn name =>                     
    lettWithName (I 0) >>= (fn (n,name_n) =>
    let fun mat sh k =
            case sh of
                nil => k nil
              | s::sh => for s (fn i => mat sh (fn ix => k(i::ix)))
    in mat sh (fn ix => f ix >>= (fn v => 
                        asgnArr(name,n,v) >>= (fn () =>
                        assign name_n (addi(n,I 1))))) >>= (fn () => 
       ret name)
    end)))

fun materializeWithNameN (ty,sh,f) =
    materializeN (ty,sh,f) >>= (fn name =>
    let fun read i = ret(P.Subs(name,i))
        val (_,_,g) = toN0 (ArrF(ty,sh,read))
    in ret (g, name)
    end)
      
(* Restructuring *)
fun rav a = toV a >>= (ret o vec)

fun zildeOf (Arr(ty,sh,idx)) = Arr(ty,[I 0],idx)

fun each ty g (Arr(_,sh,F f)) = ArrF(ty,sh, fn i => f i >>= g)
  | each ty g (Arr(_,sh,N f)) = ArrN(ty,sh, fn i => f i >>= g)

fun shEq nil nil = B true
  | shEq (x::xs) (y::ys) = andb(eqi(x,y),shEq xs ys)
  | shEq _ _ = B false
      
fun zipWith ty g (a as Arr(_,sha,idxa)) (b as Arr(_,shb,idxb)) =
    case (idxa, idxb) of
        (F fa, F fb) =>
        let fun fr i = fa i >>= (fn va => fb i >>= (fn vb => g(va,vb)))
        in lett (shEq sha shb) >>= (fn shapeeq =>      
           assert "arguments to zipWith have different shape" shapeeq
           (ret(ArrF(ty,sha,fr))))
        end
     | (N fa, N fb) =>
        let fun fr i = fa i >>= (fn va => fb i >>= (fn vb => g(va,vb)))
        in lett (shEq sha shb) >>= (fn shapeeq =>      
           assert "arguments to zipWith have different shape" shapeeq
           (ret(ArrN(ty,sha,fr))))
        end
     | (N _, _) => zipWith ty g (toF a) b
     | (_, N _) => zipWith ty g a (toF b)

fun scanChunked ty sz n m g f = 
    alloc ty sz >>= (fn (read,write) =>
    for n (fn i =>
      lett (muli(i,m)) >>= (fn offset =>
      for m (fn j =>
        lett (addi(j,offset)) >>= (fn idx =>
        g idx >>= (fn v =>
        ifUnit(eqi(j,I 0),
               write (idx,v),
               read (subi(idx, I 1)) >>= (fn p => 
               f(p,v) >>= (fn res =>             
               write (idx,res))))))))) >>= (fn () =>
    ret read))

fun scan f a =
    let val (ty,sh,g) = toF0 a
    in case List.rev sh of
           nil => ret a
         | m::rsh =>
           lett (lprod sh) >>= (fn sz =>
           lett (lprod rsh) >>= (fn n =>   (* n times m chunks should be scanned *)
           scanChunked ty sz n m g f >>= (fn rd =>
           ret (ArrF(ty,sh,rd)))))
    end

fun catenate_first (a1:m) (a2:m) : m M =
      let val (ty1,sh1,f1) = toF0 a1
          val (ty2,sh2,f2) = toF0 a2
          val (v1,s1) = case sh1 of nil => (I 1, nil)
                                   | x::xs => (x, xs)
          val (v2,s2) = case sh2 of nil => (I 1, nil)
                                   | x::xs => (x, xs)
          val x = addi(v1,v2)
      in lett (shEq s1 s2) >>= (fn shapeeq =>
         assert "arguments to catenate_first have incompatible shapes" shapeeq
         (lett (lprod sh1) >>= (fn boundary => 
          ret(ArrF(ty1,x::s1, fn i => If0(lti(i,boundary), f1 i, f2 (subi(i,boundary))))))))
      end

fun take n (Arr(ty,sh,idx)) =
    lett n >>= (fn n =>
    lett (absi n) >>= (fn absn =>
    lett (lti(n,I 0)) >>= (fn negative_n =>
    let val default = proto ty
        val sh' = case sh of nil => [absn]
                           | _ :: sh => absn :: sh
    in case idx of
           F f =>
           (lett (lprod sh) >>= (fn sz =>
            lett (lprod sh') >>= (fn sz' =>
            lett (subi(sz',sz)) >>= (fn offset =>
            ret (ArrF(ty,sh',
                      fn i => ifM ty (andb(negative_n,lti(i,offset)), ret default,
                                    ifM ty (andb(gtei(n,I 0),gtei(i,sz)), ret default,
                                            lett (If(negative_n,subi(i,offset),i)) >>= f))))))))
         | N f =>
           (case (sh, sh') of
                (s::sh1,s'::sh1') =>
                lett (subi(s',s)) >>= (fn offset =>
                ret(ArrN(ty,sh',fn i::ix => ifM ty (andb(negative_n,lti(i,offset)), ret default,
                                                    ifM ty (andb(gtei(n,I 0),gtei(i,s)), ret default,
                                                            lett (If(negative_n,subi(i,offset),i)) >>= (fn i => f(i::ix))))
                                 | _ => die "expecting index vector in take.N")))
              | _ => die "take")
    end)))

fun drop i a =
    let val (ty,sh,f) = toF0 a
        val x = absi i
        val sh' = case sh of
                      nil => nil
                    | n :: subsh => let val y = maxi(I 0, subi(n,x))
                                    in y::subsh
                                    end
        val offset = case sh of
                         nil => I 0
                       | _ :: subsh => maxi(I 0, muli(i,lprod subsh))
    in ret (ArrF(ty,sh',
                 fn i => lett (addi(i,offset)) >>= f))
    end
  
fun rotate n a =
    let val (ty,sh,f) = toF0 a
    in case sh of
           [sz] => 
           let val d = V(ty,sz,f)
           in vec(Ifv(lti(n,I 0), concat (dr (addi(sz,n)) d) (tk (addi(sz,n)) d),
                      concat (dr n d) (tk n d)))
           end
         | _ => die "rotate works only for vectors"
    end

fun reshape (f: m) a : m M =
    let val (ty,sh,g) = toF0 a
    in comment "reshape" >>= (fn () =>
       toV f >>= (fn v =>
       getShapeV "reshape" v >>= (fn sh' =>
       lett (lprod sh) >>= (fn sz =>
       lett (lprod sh') >>= (fn sz' =>
       let val V(_,_,g') = extend sz' (V(ty,sz,g))
       in ret(ArrF(ty,sh',g'))
       end)))))
    end

fun transpose a =
    let val (ty,sh,f) = toN0 a
    in ret(ArrN(ty,List.rev sh, f o List.rev))
    end

fun transpose2 idxs a =
    let val (ty,sh,f) = toF0 a
        fun check n =
            if n = 0 then ()
            else if List.exists (fn x => x = n) idxs then
              check (n-1)
            else die "transpose2: index vector not a permutation"
        val r = List.length sh
        val sz_idxs = List.length idxs
    in check sz_idxs
     ; if r <> sz_idxs then die "transpose2: wrong index vector length"
       else if r < 2 then ret a
       else
         comment "transpose2" >>= (fn () =>
         let val sh' = exchange' idxs sh
         in ret (ArrN(ty,sh',fn ix => fromSh sh (exchange idxs ix) >>= f))
         end)
    end

fun catenate (t1:m) (t2:m) : m M = 
    transpose t1 >>= (fn a1 =>
    transpose t2 >>= (fn a2 =>
    catenate_first a1 a2 >>= transpose))

fun reduce f e (Arr(ty,sh,idx)) scalar array =
    case idx of
        N g =>
        (case List.rev sh of
             nil => ret (scalar e)
           | [sz] => foldl f e (V(ty,sz,fn i => g [i])) >>= (ret o scalar)
           | s::rsh =>
             let val sh' = List.rev rsh
             in ret(array(ArrN(ty,sh',
                               fn ix => foldl f e (V(ty,s,fn j => g(ix@[j]))))))
             end)
      | F g =>
        let val r = List.length sh
        in case List.rev sh of
               nil => ret (scalar e)
             | [sz] => foldl f e (V(ty,sz,g)) >>= (ret o scalar)
             | s::rsh =>
               let val sh' = List.rev rsh
               in ret(array(ArrF(ty,sh',
                                 fn i => foldl f e (V(ty,s, fn j => lett (addi(j,muli(i,s))) >>= g)))))
               end
    end

fun assert_length s n v =
    if List.length v = n then ()
    else die ("assert_length: " ^ s)

fun compress (is,vs) =
    let val (ty_is,sh_is,vs_is) = toF0 is
        val (ty_vs,sh_vs,vs_vs) = toF0 vs
    in case (sh_is, sh_vs) of
           ([s_is],[s_vs]) =>
           compr (V(ty_is,s_is,vs_is)) (V(ty_vs,s_vs,vs_vs)) >>= (fn V(ty,s,vs) =>
           ret (ArrF(ty_vs,[s],vs)))
         | _ => die "rank of bool array argument and source array argument to compress must be 1"
    end

fun replicate (def,is,vs) =
    let val (ty_is,sh_is,vs_is) = toF0 is
        val (ty_vs,sh_vs,vs_vs) = toF0 vs
    in case (sh_is, sh_vs) of
           ([s_is],[s_vs]) =>
           repl def (V(ty_is,s_is,vs_is)) (V(ty_vs,s_vs,vs_vs)) >>= (fn V(ty,s,vs) =>
           ret (ArrF(ty_vs,[s],vs)))
         | _ => die "rank of integer array argument and source array argument to replicate must be 1"
    end

fun vreverse a =
    let val (ty,sh,f) = toF0 a
    in case sh of
           [] => ret a
         | n :: subsh =>
          lett (lprod subsh) >>= (fn subsz =>
          ret (ArrF(ty,sh,
                    fn i => 
                       lett let val y = subi(subi(n,divi(i,subsz)),I 1)
                                val x = modi(i,subsz)
                            in addi(muli(y,subsz),x)
                            end >>= f))
              )
    end

fun vrotate n (a as Arr(ty,sh,idx)) =
    case sh of
        [] => ret a
      | s :: sh' =>
          let val n = If(gti(n,I 0),modi(n,s),subi(s,modi(negi n,s)))
          in case idx of
                 F f => 
                  (lett (lprod sh') >>= (fn sz' =>
                   lett (muli(s,sz')) >>= (fn sz =>
                   lett (muli(n,sz')) >>= (fn offset =>
                   let val v = V(ty,sz, fn i => f(modi(addi(i,offset),sz)))
                       val v0 = V(ty,sz,f)
                       val V(_,_,f) = Ifv(eqi(s,I 0),v0,v)
                   in ret(ArrF(ty,sh,f))
                   end))))
               | N f => 
                 ret(ArrN(ty,sh,fn i::ix => If0(eqi(s,I 0), f(i::ix), f(modi(addi(i,n),s)::ix))
                                 | _ => die "rotatev.N"))
          end

fun letts nil = ret (nil,nil)
  | letts (x::xs) = lettWithName x >>= (fn (e,n) => letts xs >>= (fn (es,ns) => ret (e::es,n::ns)))

fun letts0 nil = ret nil
  | letts0 (x::xs) = lett x >>= (fn e => letts0 xs >>= (fn es => ret (e::es)))

fun power (f: m -> m M) (n: INT) (Arr(ty,sh,idx)) : m M =
    letts sh >>= (fn (sh,names_sh) =>
    let fun multi_assign [] [] = []
          | multi_assign (n::ns) (x::xs) = (n := x) :: multi_assign ns xs
          | multi_assign _ _ = die "power - type mismatch"
        val sh = List.map Var names_sh
    in case idx of
           F g =>
           (lett (lprod sh) >>= (fn sz =>
            materializeWithName (V(ty,sz,g)) >>= (fn (V(_,_,g),name_vs,name_n) =>
            let open P
                val body = f (ArrF(ty,sh,g)) >>= (fn a' =>
                           let val (ty',sh',g') = toF0 a'
                           in lett (lprod sh') >>= (fn sz' =>
                              materializeWithName (V(ty',sz',g')) >>= (fn (V(_,_,g'), name_vs', name_n') =>
                              letts0 sh' >>= (fn sh' =>
                              ((), fn ss => [Free name_vs,
                                             name_vs := Var name_vs',
                                             name_n := Var name_n'] @ multi_assign names_sh sh' @ ss))))
                           end)
            in for n (fn _ => body) >>= (fn () => ret(ArrF(ty,sh,g)))
            end)))
         | N g =>
           (materializeWithNameN (ty,sh,g) >>= (fn (g,name_vs) =>
            let open P
                val body = f (ArrN(ty,sh,g)) >>= (fn a' =>
                           let val (ty',sh',g') = toN0 a'
                           in materializeWithNameN (ty',sh',g') >>= (fn (g', name_vs') =>
                              letts0 sh' >>= (fn sh' =>
                              ((), fn ss => [Free name_vs,
                                             name_vs := Var name_vs'] @ multi_assign names_sh sh' @ ss)))
                           end)
            in for n (fn _ => body) >>= (fn () => ret(ArrN(ty,sh,g)))
            end))
  end)

fun powerScl (f: t -> t M) (n: INT) (a: t) : t M =
    lettWithName a >>= (fn (a,name) =>
    for n (fn _ => f a >>= (assign name)) >>= (fn () => ret a))

fun condScl (f: t -> t M) (b: BOOL) (a: t) : t M =
    lettWithName a >>= (fn (a,name) =>
    ifUnit(b, f a >>= (assign name), ret ()) >>= (fn () =>
    ret a))

(* Indexing *)

fun indexFirst (n:INT) a (scalar: t -> 'b) (array: m -> 'b) : 'b M =
    let val (ty,sh,f) = toF0 a
    in lett (lprod sh) >>= (fn sz =>
       lett (subi(n,I 1)) >>= (fn nminus1 =>
       case sh of
           nil => die "indexFirst expect non-scalar array"
         | [s] => assert "VECTOR INDEX OUT OF BOUNDS" (ltei(n,s))
                         (f nminus1 >>= (ret o scalar))
         | s::sh' => assert "ARRAY INDEX OUT OF BOUNDS" (ltei(n,s))
                            (lett (lprod sh') >>= (fn bulksz =>
                             lett (muli(nminus1,bulksz)) >>= (fn offset =>
                             ret $ array $ ArrF(ty,sh',fn i => f(addi(i,offset))))))))
    end
(*
  d: dimension, n: index in dimension, a: array being indexed.
  The result may be a scalar (if a is of rank 1, i.e., a vector) or an
  array (if a is of rank > 1). The scalar and array functions provide
  embedding functions for both cases.
*)
fun idxS (d:INT) (n:INT) (a as Arr(_,sh,_)) (scalar: t -> 'b) (array: m -> 'b) : 'b M =
    let val r = List.length sh
    in case P.unI d of
           SOME d =>
           let fun tk n l = List.take(l,n)
               fun dr n l = List.drop(l,n)
               val () = if d < 1 orelse d > r then die "idxS.dimension index error"
                        else ()
               val iotar = List.tabulate (r, fn i => i+1)
               val iotar = dr 1 iotar
               val I = tk (d-1) iotar @ [1] @ dr (d-1) iotar  (* squeze in a 1 in position d *)
           in transpose2 I a >>= (fn a2 =>
              indexFirst n a2 scalar array)
           end
         | NONE => die "idxS.expecting statically known dimension specification"
    end

(* Printing *)

fun fmtOfTy ty =
    if ty = Int orelse ty = Bool then "%d"
    else if ty = Char then "%c"
    else if ty = Double then "%DOUBLE"     (* IL pretty printer will substitute the printf with a call to prDouble, defined in apl.h *)
    else die "fmtOfTy.type not supported"

fun fmtOfTyScl ty =
    "[](" ^ fmtOfTy ty ^ ")\n"

fun prScl (V(ty,_,f)) =
    f (I 0) >>= (fn e =>
    printf(fmtOfTyScl ty, [e]))

fun prSeq sep v =
    lett (subi(length v,I 1)) >>= (fn sz_sub_one =>
    let fun f i x =
            printf(fmtOfTy(tyOfV v), [x]) >>= (fn () =>
            if sep = "" then ret ()
            else ifUnit (lti(i,sz_sub_one), 
                    printf(sep,nil) >>= (fn () => ret ()),
                    ret ()))
    in appi f v
    end)

fun prVec thestart theend v =
   printf(thestart,[]) >>= (fn () =>
   prSeq "," v >>= (fn () =>
   printf(theend,[])))

fun prAr ty sh vs =
    let val r = List.length sh
        val sh = Shape.fromShList sh
        fun def() = prVec "[" "]" sh >>= (fn () => prVec "(" ")" vs)
    in if r = 1 andalso ty = Char then prSeq "" vs
       else def()
    end

fun sprintf (s,es) = sprintfV(s,es) >>= (ret o vec)

fun sepOfTy ty =
    if ty = Char then "" else " "

fun prMat n m (V(ty,_,f)) =
    let fun prRow m j =
            lett (muli(j,m)) >>= (fn k =>
            let val vec = V(ty,m,fn x => f(addi(x,k)))
            in printf(" ",[]) >>= (fn () =>
               prSeq (sepOfTy ty) vec >>= (fn () =>
               printf("\n",[])))
            end)
    in printf("\n",[]) >>= (fn () => 
       lett m >>= (fn m => 
       for n (prRow m)))
    end

fun prArr a =
    let val (ty,sh,f) = toF0 a
        val r = List.length sh
    in lett (lprod sh) >>= (fn sz =>
       let val vs = V(ty,sz,f)
       in case sh of
              [fst,snd] => ifUnit (gti(sz,I 0),
                                   prMat fst snd vs,
                                   prAr ty sh vs)
            | _ => prAr ty sh vs
       end >>= (fn () =>
       printf("\n",[])))
    end

datatype mm = MA of sh * IL.Type * t * Name.t
fun mk_mm a =
    let val (ty,sh,f) = toF0 a
    in lett (lprod sh) >>= (fn sz =>
       materialize (V(ty,sz,f)) >>= (fn (name,name_n) =>
       ret(MA(sh,ty,P.Var name_n, name))))
    end
    
fun idxassign a (MA(sh,ty,_,name)) v : unit M =
    toV a >>= (fn is =>
    getShapeV "idxassign.is" is >>= (fn is =>
    fromSh sh (List.map (fn i => subi(i,I 1)) is) >>= (fn i => asgnArr(name,i,v))))

fun mm2m (MA(sh,ty,n,name)) = ArrF(ty,sh,fn i => ret(P.Subs(name,i)))

(* Time.now *)

fun nowi x = P.nowi x
    
(* Reading files *)
fun readFile _ = die "readFile not implemented"

fun readVecFile ty reader a : m M =
    toV a >>= (fn v =>
    materialize v >>= (fn (name_file,name_n) =>
    alloc0 Type.Int (I 1) >>= (fn name_count =>
    lettWithName (reader(P.Var name_file, P.Var name_count)) >>= (fn (_, name_ivec) =>
    lett (P.Subs(name_count,I 0)) >>= (fn count =>
    ret(vec(V(ty,count,fn i => ret(P.Subs(name_ivec,i))))))))))

val readIntVecFile : m -> m M = 
    readVecFile Type.Int P.readIntVecFile

val readDoubleVecFile : m -> m M = 
    readVecFile Type.Double P.readDoubleVecFile

end
