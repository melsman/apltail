(* Pull vectors *)

structure Laila :> LAILA = struct

fun die s = raise Fail ("Laila." ^ s)

structure P = Program

type 'a M = 'a * (P.ss -> P.ss)

type t = P.e
datatype v = V of IL.Type * t * (t -> t M)

type Value = IL.Value

infix >>=
fun (v,ssT) >>= f = let val (v',ssT') = f v in (v', ssT o ssT') end
fun ret v = (v, fn ss => ss)
fun runM0 (e,ssT) k = ssT [k e]

fun runMss (e,ssT) k = ssT (k e)

fun assert s b (m : 'a M) : 'a M  =
       let val (v, ssT) = m
       in (v, fn ss => ssT(P.Ifs(b,[],[P.Halt s]) ss))
       end

open Type
type INT     = t
type DOUBLE  = t
type BOOL    = t

val I : Int32.int -> INT = P.I
val D : real -> DOUBLE = P.D
val B : bool -> BOOL = P.B

fun uncurry f (x,y) = f x y

infix ::=
val op := = P.:=

fun simple f =
    let open P 
        val tyv = Type.Vec Int
        val v = Name.new tyv
        val (e,ssT) = f ($ v)
    in case ssT nil of
           nil => simpleIdx v e
         | _ => false
    end

fun materialize (V(ty,n,f)) =
    let open P
        val tyv = Type.Vec ty
        val name = Name.new tyv
        fun ssT ss = Decl(name, SOME(Alloc(tyv,n))) ::
                     (For(n, fn i => runM0(f i)(fn v => (name,i) ::= v)) ss)
    in (name, ssT)
    end

fun materializeWithV (v as V(ty,n,_)) =
    let val (name,ssT) = materialize v
    in ((V(ty,n, fn i => ret(P.Subs(name,i))), name), ssT)
    end

fun memoize t =
    case t of
      V (ty,n,f) =>
      if simple f then ret t
      else materializeWithV t >>= (fn (v, _) => ret v)

fun lett ty t =
    let open P
    in if simpleExp t then ret t
       else let val name = Name.new ty
                fun ssT ss = Decl(name, SOME t) :: ss
            in ($ name, ssT)
            end
    end

val letm = ret

local open P
in
  fun map ty f (V(_,n,g)) = V(ty, n, fn i => g i >>= f)

  fun stride t (V(ty,m,g)) = 
      let val n = max (I 1) t
      in V(ty, m / n, fn i => g(n * i))
      end

  fun map2 ty f (V(_,n1,f1)) (V(_,n2,f2)) =
      V(ty, 
        min n1 n2, fn i => 
                      f1 i >>= (fn v1 =>
                      f2 i >>= (fn v2 =>
                      f(v1,v2))))

  fun rev (V(ty,n,g)) = V(ty,n, fn i => g(n - i - I 1))

  fun tabulate ty t f = V(ty,t,f)

  fun proto t =
        if t = Int then I 0
        else if t = Double then D 0.0
        else if t = Bool then B false
        else die ("proto: unsupported type " ^ prType t)

  fun dummy_exp t =
      if t = Int then I 666
      else if t = Double then D 66.6
      else if t = Bool then B false
      else die ("empty: unknown type " ^ prType t)

  fun empty ty = V(ty, I 0, fn _ => ret(dummy_exp ty))

  fun emptyOf (V(ty,_,f)) = V(ty, I 0, f)

  fun single ty t = V(ty, I 1, fn _ => ret t)

  fun tk t (V(ty,m,g)) = V(ty,min t m, g)
             
  fun dr t (V(ty,m,g)) = V(ty,max(m-t)(I 0), fn i => g(i+t))

  fun length (V(_,n,_)) = n
end

val addi  : INT * INT -> INT = P.+
val subi  : INT * INT -> INT = P.-
val muli  : INT * INT -> INT = P.*
val divi  : INT * INT -> INT = P./
val modi  : INT * INT -> INT = P.%
val resi  : INT * INT -> INT = P.resi
val maxi  : INT * INT -> INT = uncurry P.max
val mini  : INT * INT -> INT = uncurry P.min
val negi  : INT -> INT = P.~
val lti   : INT * INT -> BOOL = P.<
val ltei  : INT * INT -> BOOL = P.<=
val gti   : INT * INT -> BOOL = P.>
val gtei  : INT * INT -> BOOL = P.>=
val eqi   : INT * INT -> BOOL = P.==
val neqi  : INT * INT -> BOOL = P.<>

val addd  : DOUBLE * DOUBLE -> DOUBLE = P.+
val subd  : DOUBLE * DOUBLE -> DOUBLE = P.-
val muld  : DOUBLE * DOUBLE -> DOUBLE = P.*
val divd  : DOUBLE * DOUBLE -> DOUBLE = P./
val ltd   : DOUBLE * DOUBLE -> BOOL = P.<
val lted  : DOUBLE * DOUBLE -> BOOL = P.<=
val gtd   : DOUBLE * DOUBLE -> BOOL = P.>
val gted  : DOUBLE * DOUBLE -> BOOL = P.>=
val eqd   : DOUBLE * DOUBLE -> BOOL = P.==
val neqd  : DOUBLE * DOUBLE -> BOOL = P.<>
val maxd  : DOUBLE * DOUBLE -> DOUBLE = uncurry P.max
val mind  : DOUBLE * DOUBLE -> DOUBLE = uncurry P.min
val powd  : DOUBLE * DOUBLE -> DOUBLE = P.powd
val negd  : DOUBLE -> DOUBLE = P.~
val floor : DOUBLE -> INT = P.floor
val ceil  : DOUBLE -> INT = P.ceil

val eqb     : BOOL * BOOL -> BOOL = P.==
val andb    : BOOL * BOOL -> BOOL = P.andb
val orb     : BOOL * BOOL -> BOOL = P.orb
val xorb    : BOOL * BOOL -> BOOL = P.xorb
val notb    : BOOL -> BOOL = P.notb

val eqi  : INT * INT -> BOOL = P.==
val neqi : INT * INT -> BOOL = P.<>

val i2d  : INT -> DOUBLE = P.i2d
val d2i  : DOUBLE -> INT = P.d2i
val b2i  : BOOL -> INT = P.b2i

fun printf (s, es) = (P.I 0, fn ss => P.Printf(s,es)::ss)

(* Values and Evaluation *)
type V    = IL.Value
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

(* Some utility functions *)
fun opt_ss0 ss =
    let val ss = P.se_ss nil ss
        val ss = P.se_ss nil ss
    in P.rm_decls0 ss
    end

fun opt_ss e ss =
    let val ss = P.se_ss nil ss
        val ss = P.se_ss nil ss
    in P.rm_decls e ss
    end

type prog = Type.T * Type.T * (Name.t * (P.e -> P.s) -> P.ss)

fun pp_prog ((ta,tb,p): prog) : string =
    let val name_arg = Name.new ta
        val ss = p (name_arg, P.Ret)
        val ss = opt_ss0 ss
    in ILUtil.ppFunction "kernel" (ta,tb) name_arg ss
    end

fun eval ((ta,tb,p): prog) (v: V) : V =
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

val $ = P.$

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
         ($ n, fn ss => P.Decl(n,SOME(P.If(x,e1,e2)))::ss)
       | _ =>
         let val k = fn v => n := v
             val s1 = runM0 m1 k
             val s2 = runM0 m2 k
             val s1 = opt_ss0 s1
             val s2 = opt_ss0 s2
             fun default() =
                 ($ n, fn ss => P.Decl(n,NONE)::P.Ifs(x,s1,s2) ss)
         in case (s1, s2) of
              ([IL.Assign(n1,e1)],[IL.Assign(n2,e2)]) =>
              if n = n1 andalso n = n2 then
                ($ n, fn ss => P.Decl(n,SOME(P.If(x,e1,e2)))::ss)
              else default()
            | _ => default()
         end
    end

fun If (x,a1,a2) = P.If(x,a1,a2)

fun Ifv (x,V(ty,n1,f1), V(_,n2,f2)) =
    V(ty,P.If(x,n1,n2), 
      fn i =>
         let val m1 = f1 i
             val m2 = f2 i
         in If0(x,m1,m2)
         end)
           
fun For'(n,e,body) =
    let open P
    in case unI n of
         SOME 0 => (e, fn ss => ss)
       | SOME 1 => 
         let val ty = ILUtil.typeExp e
             val a = Name.new ty
             fun f e = Decl(a, SOME e)
             val ss = body e f (I 0)
         in case ss of
              [s] => 
              (case unDecl s of
                 SOME (a',SOME e) => if a' = a then (e, fn ss => ss)
                                     else die "For': weird"
               | SOME (a',NONE) => die "For': weird2"
               | NONE => ($a, fn ss => s :: ss))
            | ss0 => ($a, fn ss => ss0 @ ss)
         end
       | _ => 
         let val ty = ILUtil.typeExp e
             val a = Name.new ty
             fun f e = a := e
         in lett Int n >>= (fn n =>
               ($a, fn ss => 
                        Decl(a, SOME e) ::
                        For(n, body ($ a) f) ss))
         end
    end

fun foldl f e (V(_,n,g)) =
    let open P
        fun body e h i =
            runM0 (g i >>= (fn v => f(v,e))) h
    in For'(n,e,body)
    end

fun foldr f e (V(_,n,g)) =
    let open P
        fun body e h i =
            runM0 (g (n - I 1 - i) >>= (fn v => f(v,e))) h
    in For'(n,e,body)
    end
                     
fun concat v1 v2 =
    let val V(ty,n1,f1) = v1
        val V(_,n2,f2) = v2
    in case P.unI n1 of
           SOME 0 => v2
         | _ =>
           case P.unI n2 of
               SOME 0 => v1
             | _ => V(ty,P.+(n1,n2), 
                      fn i => 
                         let val m1 = f1 i
                             val m2 = f2 (P.-(i,n1))
                         in If0(P.<(i,n1),m1,m2)
                         end)
    end
   
  fun fromList ty nil = empty Int
    | fromList ty [t] = single ty t
    | fromList ty (t::ts) = concat (single ty t) (fromList ty ts)

  fun fromListM ty nil = ret(empty Int)
    | fromListM ty [t] = ret(single ty t)
    | fromListM ty ts =
      let open P
          val tyv = Type.Vec ty
          val name = Name.new tyv
          val sz = I(List.length ts)
          fun ssT ss = Decl(name, SOME(Vect(tyv,ts))) :: ss
      in (V(ty,sz, fn i => ret(Subs(name,i))), ssT)
      end

  fun eq f v1 v2 =
      let val v = map2 Bool (ret o f) v1 v2
          val base = eqi(length v1,length v2)
      in foldr (fn (b,a) => ret(If(a,b,a))) base v
      end

  fun sub_unsafe (V(_,n,g)) i = g i

  fun tyOfV (V(ty,_,_)) = ty

  fun merge v n t =
      concat (tk (subi(n,I 1)) v)
      (concat (single (tyOfV v) t) (dr (addi(n,I 1)) v))

  fun lprod nil = I 1
    | lprod (x::xs) = muli(x,lprod xs)
  
  fun toSh nil i = ret nil
    | toSh (x::xs) i =
      lett Int (lprod xs) >>= (fn p =>
      lett Int (modi(i,p)) >>= (fn imodp =>
      toSh xs imodp >>= (fn xs' =>
      ret (divi(i,p) :: xs'))))

  fun fromSh nil nil = ret(I 0)
    | fromSh (_::sh) (i::idx) = fromSh sh idx >>= (fn x => ret (addi(muli(i,lprod sh),x)))
    | fromSh _ _ = die "fromSh: dimension mismatch"

  fun getShape 0 f = ret nil
    | getShape n f = 
      f (P.I (n-1)) >>= (fn N =>
      getShape (n-1) f >>= (fn NS =>
      ret (NS @ [N])))

  fun getShapeV s (V(_,n,f)) =
      case P.unI n of
          SOME n => getShape n f
        | NONE => die ("getShapeV: " ^ s ^ ". Expecting static shape")

  fun trans v d =
      let val V(_,n,f) = v
          val V(ty,m,g) = d
          fun h n i =
              getShape n f >>= (fn sh => 
              let val sh' = List.rev sh
              in toSh sh' i >>= (fn sh'' =>
                 fromSh sh (List.rev sh'') >>= (fn x => g x))
              end)
      in case P.unI n of
             SOME 0 => ret d   (* known number of dimensions *)
           | SOME 1 => ret d
           | SOME n => ret (V(ty,m,h n))
           | NONE => (*ret(If(E n < I 2, d, V(ty,m,g')))*)
             die "trans: unknown number of dimensions not supported"
      end

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
              else getShape n f >>= (fn sh =>
                 let val sh' = exchange' idxs sh
                     fun h i =
                         toSh sh' i >>= (fn sh'' =>
                         fromSh sh (exchange idxs sh'') >>= (fn x => g x))
                 in ret (fromList Int sh',
                         V(ty,m,h))
                 end))
           | NONE => die "trans2: unknown number of dimensions not supported"
      end

  fun compr bv v =
      let val V(_,n,f) = bv
          val V(ty,m,g) = v
      in foldl (ret o addi) (I 0) (map Int (ret o b2i) bv) >>= (fn sz =>
         let open P
             val tyv = Type.Vec ty
             val name = Name.new tyv
             val count = Name.new Type.Int
             fun ssT ss = Decl(name, SOME(Alloc(tyv,sz))) ::
                          Decl(count, SOME(I 0)) ::
                          For(n, fn i => runMss(f i)(fn b =>
                            Ifs(b,
                                runMss(g i)(fn v =>
                                   [(name, $count) ::= v,
                                    count := $count + I 1]),
                                [])
                            []))
                          ss
         in (V(ty,sz,fn i => ret(Subs(name,i))),
             ssT)
         end)
      end

  fun extend n (V(ty,m,f)) =
      Ifv(eqi(m,I 0), V(ty,n, fn _ => ret (proto ty)), V(ty,n,f o (fn i => P.%(i, m))))

  fun outmain outln =
    ( outln "int main() {"
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
     ; outln "#include \"apl.h\""
     ; outln body
     ; outmain outln
     ; TextIO.closeOut os
     ; print ("Wrote file " ^ ofile ^ "\n")
    end

(* fun resi(x,y) = If(eqi(x,I 0), y, modi(y,x)) *)
fun resd(x,y) = die "resd not yet supported"

fun signi x = If(lti(x,I 0),I ~1,I 1)
fun signd x = If(ltd(x,D 0.0),I ~1,I 1)
fun absi x = If(lti(x, I 0), negi x, x)

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
  val eq       : t -> t -> BOOL M
end = struct
  type t = v
  val concat   = concat
  val empty    = empty Int
  val singlez  = single Int (I 0)
  val single   = single Int
  val product  : t -> INT M = foldl (ret o muli) (I 1)
  val length   : t -> INT = length
  val dr       : INT -> t -> t = dr
  val tk       : INT -> t -> t = tk
  val eq       : t -> t -> BOOL M = eq eqi
end

datatype m = A of Shape.t * v

(* invariant: A(s,d) ==> product s = length d *) 

fun vec c = A(Shape.single (length c), c)
fun scl ty v = A(Shape.empty, single ty v)
fun first (A(_,v)) =
    let fun first_unsafe (V(_,_,f)) = f (I 0)
        fun maybe_pad (v as V(ty,n,f)) =
            Ifv(gti(n,I 0), v, V(ty,I 1, fn _ => ret(proto ty)))
    in first_unsafe(maybe_pad v)
    end
fun zilde ty = A(Shape.singlez, empty ty)
fun iota n = vec (tabulate Int n (fn x => ret(addi(x,I 1))))
fun iota' _ = die "iota' not implemented"
fun shape (A(f,_)) = f
fun snd (A(_,c)) = c
fun siz (A(_,c)) = length c
fun dim (A(f,_)) = Shape.length f

fun dimincr (A(f,v)) = A(Shape.concat f (Shape.single (I 1)),v)

fun mem (A(f,d)) = memoize d >>= (fn d => ret (A(f,d)))
              
(* Restructuring *)
fun rav (A(_,c)) = vec c

val rav0 = snd

fun mif (x, A(f1,v1), A(f2,v2)) = A(Ifv(x,f1,f2), Ifv(x,v1,v2))

fun zildeOf (A(f,v)) = A(Shape.singlez, emptyOf v)

fun each ty g (A(f,cs)) = A(f, map ty g cs)
      
fun meq _ f (A(f1,c1)) (A(f2,c2)) =
    Shape.eq f1 f2 >>= (fn shape_eq => 
     eq f c1 c2 >>= (fn content_eq =>
      ret(If(shape_eq,content_eq,B false))))

fun zipWith ty g a b =
    let val sha = shape a
	val shb = shape b
        val mv = A(sha,map2 ty g (snd a) (snd b))
    in (*ret mv *)
       Shape.eq sha shb >>= (fn shapeeq =>      
       assert "arguments to zipWith have different shape" shapeeq
       (ret mv))
    end

fun scan _ = die "scan: not implemented"

fun pad1 s = Ifv(eqi(length s,I 0), single Int (I 1), s) 

fun catenate_first (A(s1,d1) : m) (A(s2,d2): m) : m M =
      let val s1 = pad1 s1
          val s2 = pad1 s2                   
          val s1' = Shape.dr (I 1) s1
          val s2' = Shape.dr (I 1) s2
          val v1 = Shape.tk (I 1) s1
          val v2 = Shape.tk (I 1) s2
          val x = map2 Int (ret o addi) v1 v2
          val mv = A(Shape.concat x s1',
                     concat d1 d2)
      in Shape.eq s1' s2' >>= (fn shapeeq =>
         assert "arguments to catenate_first have incompatible shapes" shapeeq
         (ret mv))
      end

fun ifM t (b,m1,m2) =
    let val n = Name.new t
    in ($ n, 
        fn ss => P.Decl(n, NONE) ::
                 P.Ifs(b,
                       runMss m1 (fn v => [n := v]),
                       runMss m2 (fn v => [n := v])) ss)
    end

fun take n (a as A(shv,V(ty,sz,f))) =
    getShapeV "take" shv >>= (fn sh =>
    let val default = proto ty
        val (sh', shv') = case sh of
                              nil => ([absi n],Shape.single(absi n))
                            | _ :: sh => (absi n :: sh,
                                          Shape.concat (Shape.single(absi n)) (Shape.dr (I 1) shv))
        val sz' = lprod sh'
        val offset = subi(sz',sz)
    in ret (A(shv',
              V(ty,sz',
                fn i => ifM ty (andb(lti(n,I 0),lti(i,offset)), ret default,
                                ifM ty (andb(gtei(n,I 0),gtei(i,sz)), ret default,
                                        f (If(lti(n,I 0),subi(i,offset),i)))))))
    end)

fun drop i (a as A(shv,V(ty,sz,f))) =
    getShapeV "drop" shv >>= (fn sh =>
    let val x = absi i
        val (sh',shv') = case sh of
                             nil => (nil,Shape.empty)
                           | n :: subsh => let val y = maxi(I 0, subi(n,x))
                                           in (y::subsh,
                                               Shape.concat (Shape.single y) (Shape.dr (I 1) shv))
                                           end
        val sz' = lprod sh'
        val offset = case sh of
                         nil => I 0
                       | _ :: subsh => maxi(I 0, muli(i,lprod subsh))
    in ret (A(shv',
              V(ty,sz',
                fn i => f (addi(i,offset)))))
    end)
  
fun rotate n (A(f,d)) =
    let val sz = length d
    in mif(lti(n,I 0),
           A(f,concat (dr (addi(sz,n)) d) (tk (addi(sz,n)) d)),
           A(f,concat (dr n d) (tk n d)))
    end

fun reshape (f: v) (a: m) : m M =
    Shape.product f >>= (fn p =>
    ret(A(f,extend p (snd a))))

fun transpose (A(s,d)) = trans s d >>= (fn v => ret(A(rev s, v)))

fun transpose2 v (A(s,d)) = trans2 v s d >>= (fn p => ret(A p))

fun catenate t1 t2 = 
    transpose t1 >>= (fn a1 =>
    transpose t2 >>= (fn a2 =>
    catenate_first a1 a2 >>= transpose))

fun reduce f e (A(s,d)) scalar vector =
      let val r = length s
      in case P.unI r of
              SOME 1 => foldl f e d >>= (ret o scalar)
(*
            | SOME 2 =>  (* matrix: M x N *)
              let 
              in sub_unsafe s (I 0) >>= (fn M =>
                 sub_unsafe s (I 1) >>= (fn N =>
                 ret (A(Shape.single M,
                    V(tyOfV d, M,
                      fn i => foldl f e (tk N (dr (muli(i,N)) d))))))) >>= (ret o vector)
              end
*)
            | SOME n =>
              sub_unsafe s (I(n-1)) >>= (fn N =>
              let val sh' = Shape.tk (I(n-1)) s
              in Shape.product sh' >>= (fn M =>
                 ret (vector (
                   A(sh',
                     V(tyOfV d, M,
                        fn i => foldl f e (tk N (dr (muli(i,N)) d)))))))
              end)
            | NONE => die "reduce: unknown rank not supported"
      end

fun build2 ty M1 M2 f =
    let val N = muli(M1,M2)
    in ret (V(ty, N, fn i => f (divi(i,M2)) (modi(i,M2))))
    end
    
fun assert_length s n v =
    let val r = length v
    in case P.unI r of
           SOME i => if i = n then ()
                     else die ("assert_length: " ^ s)
         | NONE => die ("assert_length: unknown length: " ^ s)
    end

fun compress (A(sh_is,vs_is), A(sh_vs,vs_vs)) =
    (assert_length "rank of bool array argument to compress must be 1" 1 sh_is;
     assert_length "rank of source array argument to compress must be 1" 1 sh_vs;
     compr vs_is vs_vs >>= (fn vs =>
     ret (A (Shape.single(length vs),vs))))

fun vreverse (a as A(sh,V(ty,sz,f))) =
    getShapeV "vreverse" sh >>= 
      (fn [] => ret a
        | n :: subsh =>
         let val subsz = lprod subsh
         in ret (A(sh,
                   V(ty,sz,
                     fn i => 
                        let val y = subi(subi(n,divi(i,subsz)),I 1)
                            val x = modi(i,subsz)
                        in f (addi(muli(y,subsz),x))
                        end)))
         end)

fun vrotate n (a as A(sh, v0 as V(ty,sz,f))) =
    getShapeV "vrotate" sh >>=
      (fn [] => ret a
        | s :: sh' =>
         let val sz' = lprod sh'
             val sz = muli(s,sz')
             val n = modi(n,s)
             val n = If(gti(n,I 0),n,subi(s,n))
             val offset = muli(n,sz')
             val v = V(ty,sz, fn i => f(modi(addi(i,offset),sz)))
             val v = Ifv(eqi(s,I 0),v0,v)
         in ret(A(sh,v))
         end)

fun materializeA (A(sh,vs)) =
    materializeWithV sh >>= (fn (sh,name_sh) =>
    materializeWithV vs >>= (fn (vs,name_vs) =>
    ret (A(sh,vs), name_sh, name_vs)))

fun power (f: m -> m M) (n: INT) (a: m) : m M =
    let open P
    in materializeA a >>= (fn (a, name_sh, name_vs) =>
       let val body = f a >>= (fn r =>
                      materializeA r >>= (fn (r', name_sh', name_vs') =>
                      (I 0, fn ss => [Free name_sh, 
                                      Free name_vs,
                                      name_sh := $name_sh',
                                      name_vs := $name_vs'] @ ss)))
       in (a,
           fn ss => P.For(n, fn _ => runMss body (fn (v : t) => []))
                         ss)
       end)
    end

fun powerScl (f: t -> t M) (n: INT) (a: t) : t M =
    let val ty = ILUtil.typeExp a
        val name = Name.new ty
    in ($ name,
        fn ss => P.Decl(name, SOME a) ::
                 P.For(n, fn _ => runMss (f($name)) (fn (v : t) => [name := v]))
                 ss)
    end
           
fun fmtOfTy ty =
    if ty = Int orelse ty = Bool then "%d"
    else if ty = Double then "%DOUBLE"     (* IL pretty printer will substitute the printf with a call to prDouble, defined in apl.h *)
    else die "fmtOfTy.type not supported"

fun fmtOfTyScl ty =
    "[](" ^ fmtOfTy ty ^ ")\n"

fun prScl (V(ty,_,f)) =
    f (I 0) >>= (fn e =>
    printf(fmtOfTyScl ty, [e]))

fun prSeq sep (ty,n,f) =
    let fun pr i =
            f i >>= (fn v =>
            printf(fmtOfTy ty, [v]) >>= (fn _ =>
            ifM Int (lti(i,subi(n,I 1)), 
                     printf(sep,nil),
                     ret (I 0))))
        fun ssT ss = P.For (n,fn i => runMss (pr i) (fn _ => [])) ss
    in (I 0, ssT)
    end

fun prVec thestart theend (V dat) =
   printf(thestart,[]) >>= (fn _ =>
   prSeq "," dat >>= (fn _ =>
   printf(theend,[])))

fun prAr sh vs =
    prVec "[" "]" sh >>= (fn _ =>
    prVec "(" ")" vs)

fun prMat N M sep (V(ty,n,f)) =
    let fun prRow j =
            let val vec = (ty,M,fn x => f(addi(x,muli(j,M))))
            in printf(" ",[]) >>= (fn _ =>
               prSeq " " vec >>= (fn _ =>
               printf("\n",[])))
            end
        fun ssT ss = 
            P.For (N,fn i => runMss (prRow i) (fn _ => [])) ss
    in printf("\n",[]) >>= (fn _ => (I 0, ssT))
    end

fun fst (V(ty,n,f)) = f (I 0)
fun snd (V(ty,n,f)) = f (I 1)

fun szOf (V(_,n,_)) = n

fun prArr (a as A(sh,vs)) =
    let val len = Shape.length sh
    in ifM Int (andb(eqi(len,I 2),gti(szOf vs,I 0)),
                fst sh >>= (fn N =>
                snd sh >>= (fn M =>
                prMat N M " " vs)),
                prAr sh vs) >>= (fn _ =>
       printf("\n",[]))
    end
    

end
