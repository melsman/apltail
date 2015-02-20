(* Pull vectors *)

structure Laila :> LAILA = struct

fun die s = raise Fail ("Laila." ^ s)

structure P = Program

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
       let val (v, ssT) = m
       in (v, fn ss => ssT(P.Ifs(b,[],[P.Halt s]) ss))
       end

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

fun alloc ty (n:t) : ((t -> t M) * (t * t -> unit M)) M =
    let open P
        val tyv = Type.Vec ty
        val name = Name.new tyv
        fun ssT ss = Decl(name, SOME(Alloc(tyv,n))) :: ss
        fun read i = ret (Subs(name,i))
        fun write (i,v) = asgnArr (name,i,v)
    in ((read,write), ssT)
    end

(* Vectors *)

datatype v = V of IL.Type * t * (t -> t M)

fun simple f =
    let open P 
        val tyv = Type.Vec Int
        val v = Name.new tyv
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
        fun ssT ss = Decl(name_n, SOME n) ::
                     Decl(name, SOME(Alloc(tyv,n))) ::
                     (For(Var name_n, name_i, runM0(f(Var name_i))(fn v => (name,Var name_i) ::= v)) ss)
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

  fun map2 ty f (V(_,n1,f1)) (V(_,n2,f2)) =
      V(ty, 
        min n1 n2, fn i => 
                      f1 i >>= (fn v1 =>
                      f2 i >>= (fn v2 =>
                      f(v1,v2))))

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
    let (*val () = print("[starting " ^ s ^ "]\n")*)
        val r = f a
        (*val () = print("[finished " ^ s ^ "]\n")*)
    in r
    end

fun se_ss ss = pr_wrap "se_ss" (P.se_ss nil) ss
fun rm_decls0 ss = pr_wrap "rm_decls0" P.rm_decls0 ss
fun rm_decls p = pr_wrap "rm_decls" (fn (e,ss) => P.rm_decls e ss) p

(* Some utility functions *)
fun opt_ss0 ss =
    let val ss = se_ss ss
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

  fun eq f v1 v2 =
      let val v = map2 Bool (ret o f) v1 v2
          val base = eqi(length v1,length v2)
      in foldl (fn (b,a) => ret(If(a,b,a))) base v
      end

  fun sub_unsafe (V(_,n,g)) i = g i

  fun tyOfV (V(ty,_,_)) = ty

  fun lprod nil = I 1
    | lprod (x::xs) = muli(x,lprod xs)
  
  fun toSh nil i = ret nil
    | toSh (x::xs) i =
      lett (lprod xs) >>= (fn p =>
      lett (modi(i,p)) >>= (fn imodp =>
      lett (divi(i,p)) >>= (fn x' =>
      toSh xs imodp >>= (fn xs' =>
      ret (x' :: xs')))))

  fun fromSh nil nil = ret(I 0)
    | fromSh (_::sh) (i::idx) = 
      fromSh sh idx >>= (fn x =>
      lett (addi(muli(i,lprod sh),x)) >>= (fn res =>
      ret res))
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
                 in ret (fromList sh',
                         V(ty,m,h))
                 end))
           | NONE => die "trans2: unknown number of dimensions not supported"
      end

  fun compr bv v =
      let val V(_,n,f) = bv
          val V(ty,m,g) = v
      in foldl (ret o addi) (I 0) (map Int (ret o b2i) bv) >>= (fn sz =>
         alloc ty sz >>= (fn (rd,wr) =>
         lettWithName (I 0) >>= (fn (count,count_name) =>
         for n (fn i => f i >>= (fn b => ifUnit(b, g i >>= (fn v => 
                                                     wr(count,v) >>= (fn () =>
                                                     assign count_name (addi(count,I 1)))),
                                                   ret ()))) >>= (fn () =>
         ret (V(ty,sz,rd))))))
      end

  fun absi x = If(lti(x, I 0), negi x, x)
  fun absd x = If(ltd(x, D 0.0), negd x, x)

  fun repl def iv v =
      let val V(_,n,f) = iv
          val V(ty,m,g) = v
      in foldl (ret o addi) (I 0) (map Int (ret o absi) iv) >>= (fn sz =>
         alloc ty sz >>= (fn (rd,wr) =>
         lettWithName (I 0) >>= (fn (count,count_name) =>
         for n (fn i => f i >>= (fn r => 
                        g i >>= (fn v =>
                        for r (fn _ => wr(count,v) >>= (fn () =>    (* MEMO: if r < 0 we should output ~r def elements *)
                                       assign count_name (addi(count,I 1))))))) >>= (fn () =>
         ret (V(ty,sz,rd))))))
      end

  fun extend n (V(ty,m,f)) =
      Ifv(eqi(m,I 0), V(ty,n, fn _ => ret (proto ty)), V(ty,n,f o (fn i => P.modi(i, m))))

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
     ; outln "#include \"../../include/apl.h\""
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
end = struct
  type t = v
  val concat   = concat
  val empty    = empty Int
  val singlez  = single (I 0)
  val single   = single
  val product  : t -> INT M = foldl (ret o muli) (I 1)
  val length   : t -> INT = length
  val dr       : INT -> t -> t = dr
  val tk       : INT -> t -> t = tk
  val dr_unsafe: INT -> t -> t = dr_unsafe
  val tk_unsafe: INT -> t -> t = tk_unsafe
  val eq       : t -> t -> BOOL M = eq eqi
end

datatype m = A of Shape.t * v

(* invariant: A(s,d) ==> product s = length d *) 

fun vec c = A(Shape.single (length c), c)
fun fromListM ty l = fromListMV ty l >>= (ret o vec)
fun enclose t = vec (single t)
fun scl ty t = A(Shape.empty, single t)
fun first (A(_,v)) =
    let fun first_unsafe (V(_,_,f)) = f (I 0)
        fun maybe_pad (v as V(ty,n,f)) =
            Ifv(gti(n,I 0), v, V(ty,I 1, fn _ => ret(proto ty)))
    in first_unsafe(maybe_pad v)
    end
fun zilde ty = A(Shape.singlez, empty ty)
fun iota n = vec (tabulate Int n (fn x => ret(addi(x,I 1))))
fun iota' _ = die "iota' not implemented"
fun shapeV (A(f,_)) = f
fun shape (A(f,_)) = vec f
fun snd (A(_,c)) = c
fun siz (A(_,c)) = length c
fun rank (A(f,_)) = Shape.length f

fun dimincr (A(f,v)) = A(Shape.concat f (Shape.single (I 1)),v)

fun mem (A(f,d)) = memoize d >>= (fn d => ret (A(f,d)))
              
(* Restructuring *)
fun rav (A(_,c)) = vec c

fun mif (x, A(f1,v1), A(f2,v2)) = A(Ifv(x,f1,f2), Ifv(x,v1,v2))

fun zildeOf (A(f,v)) = A(Shape.singlez, emptyOf v)

fun each ty g (A(f,cs)) = A(f, map ty g cs)
      
fun zipWith ty g a b =
    let val sha = shapeV a
	val shb = shapeV b
        val mv = A(sha,map2unsafe ty g (snd a) (snd b))
    in Shape.eq sha shb >>= (fn shapeeq =>      
       assert "arguments to zipWith have different shape" shapeeq
       (ret mv))
    end

fun scan _ = die "scan: not implemented"

fun pad1 s = Ifv(eqi(length s,I 0), single (I 1), s) 

fun catenate_first (A(s1,d1) : m) (A(s2,d2): m) : m M =
      let val s1 = pad1 s1
          val s2 = pad1 s2                   
          val s1' = Shape.dr_unsafe (I 1) s1
          val s2' = Shape.dr_unsafe (I 1) s2
          val v1 = Shape.tk_unsafe (I 1) s1
          val v2 = Shape.tk_unsafe (I 1) s2
          val x = map2unsafe Int (ret o addi) v1 v2
          val mv = A(Shape.concat x s1',
                     concat d1 d2)
      in Shape.eq s1' s2' >>= (fn shapeeq =>
         assert "arguments to catenate_first have incompatible shapes" shapeeq
         (ret mv))
      end

fun take n (a as A(shv,V(ty,sz,f))) =
    getShapeV "take" shv >>= (fn sh =>
    lett n >>= (fn n =>
    lett (absi n) >>= (fn absn =>
    let val default = proto ty
        val (sh', shv') = case sh of
                              nil => ([absn],Shape.single absn)
                            | _ :: sh => (absn :: sh,
                                          Shape.concat (Shape.single absn) (Shape.dr_unsafe (I 1) shv))
    in lett (lprod sh') >>= (fn sz' =>
       lett (subi(sz',sz)) >>= (fn offset =>
       lett (lti(n,I 0)) >>= (fn negative_n =>
       ret (A(shv',
              V(ty,sz',
                fn i => ifM ty (andb(negative_n,lti(i,offset)), ret default,
                                ifM ty (andb(gtei(n,I 0),gtei(i,sz)), ret default,
                                        f (If(negative_n,subi(i,offset),i)))))))
       )))
    end)))

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

fun reshape (f: m) (a: m) : m M =
    let val v = snd f
    in Shape.product v >>= (fn p =>
       ret(A(v,extend p (snd a))))
    end

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
            | SOME n =>
              sub_unsafe s (I(n-1)) >>= (fn N =>
              let val sh' = Shape.tk_unsafe (I(n-1)) s
              in Shape.product sh' >>= (fn M =>
                 ret (vector (
                   A(sh',
                     V(tyOfV d, M,
                        fn i => foldl f e (tk_unsafe N (dr_unsafe (muli(i,N)) d)))))))
              end)
            | NONE => die "reduce: unknown rank not supported"
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

fun replicate (def,A(sh_is,vs_is), A(sh_vs,vs_vs)) =
    (assert_length "rank of integer array argument to replicate must be 1" 1 sh_is;
     assert_length "rank of source array argument to replicate must be 1" 1 sh_vs;
     repl def vs_is vs_vs >>= (fn vs =>
     ret (A (Shape.single(length vs),vs))))

fun vreverse (a as A(sh,V(ty,sz,f))) =
    getShapeV "vreverse" sh >>= 
      (fn [] => ret a
        | n :: subsh =>
          lett (lprod subsh) >>= (fn subsz =>
          ret (A(sh,
                   V(ty,sz,
                     fn i => 
                        let val y = subi(subi(n,divi(i,subsz)),I 1)
                            val x = modi(i,subsz)
                        in f (addi(muli(y,subsz),x))
                        end)))
          ))

fun vrotate n (a as A(sh, v0 as V(ty,sz,f))) =
    getShapeV "vrotate" sh >>=
      (fn [] => ret a
        | s :: sh' =>
          lett (lprod sh') >>= (fn sz' =>
          lett (muli(s,sz')) >>= (fn sz =>
          let val n = If(gti(n,I 0),modi(n,s),subi(s,modi(negi n,s)))
          in lett (muli(n,sz')) >>= (fn offset =>
             let val v = V(ty,sz, fn i => f(modi(addi(i,offset),sz)))
                 val v = Ifv(eqi(s,I 0),v0,v)
             in ret(A(sh,v))
             end)
          end)))

fun letts nil = ret (nil,nil)
  | letts (x::xs) = lettWithName x >>= (fn (e,n) => letts xs >>= (fn (es,ns) => ret (e::es,n::ns)))

fun letts0 nil = ret nil
  | letts0 (x::xs) = lett x >>= (fn e => letts0 xs >>= (fn es => ret (e::es)))

fun power (f: m -> m M) (n: INT) (A(sh,vs)) : m M =
  getShapeV "power" sh >>= (fn sh =>
  letts sh >>= (fn (sh,names_sh) =>
  materializeWithName vs >>= (fn (vs,name_vs,name_n) =>
  fromListMV Int (List.map Var names_sh) >>= (fn sh =>
  let open P
      fun multi_assign [] [] = []
        | multi_assign (n::ns) (x::xs) = (n := x) :: multi_assign ns xs
        | multi_assign _ _ = die "power - type mismatch"
      val body = f (A(sh,vs)) >>= (fn A(sh',vs') =>
                 materializeWithName vs' >>= (fn (vs', name_vs', name_n') =>
                 getShapeV "power" sh' >>= (fn sh' =>
                 letts0 sh' >>= (fn sh' =>
                 ((), fn ss => [Free name_vs,
                                name_vs := Var name_vs',
                                name_n := Var name_n'] @ multi_assign names_sh sh' @ ss)))))
  in for n (fn _ => body) >>= (fn () => ret(A(sh,vs)))
  end))))

fun powerScl (f: t -> t M) (n: INT) (a: t) : t M =
    lettWithName a >>= (fn (a,name) =>
    for n (fn _ => f a >>= (assign name)) >>= (fn () => ret a))

(* Indexing *)

(*
  d: dimension, n: index in dimension, a: array being indexed.
  The result may be a scalar (if a is of rank 1, i.e., a vector) or an
  array (if a is of rank > 1). The scalar and array functions provide
  embedding functions for both cases.
*)
fun idxS (d:INT) (n:INT) (a:m) (scalar: t -> 'b) (array: m -> 'b) : 'b M =
    die "idxS not implemented"

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

fun prAr (sh as V(_,rank,_)) (vs as V dat) =
    let fun def() = prVec "[" "]" sh >>= (fn () => prVec "(" ")" vs)
    in case P.unI rank of
           SOME 1 => if #1 dat = Char then prSeq "" vs
                     else def()
         | _ => def()
    end

fun sepOfTy ty =
    if ty = Char then "" else " "

fun prMat N M (V(ty,_,f)) =
    let fun prRow M j =
            lett (muli(j,M)) >>= (fn k =>
            let val vec = V(ty,M,fn x => f(addi(x,k)))
            in printf(" ",[]) >>= (fn () =>
               prSeq (sepOfTy ty) vec >>= (fn () =>
               printf("\n",[])))
            end)
    in printf("\n",[]) >>= (fn () => 
       lett M >>= (fn M => 
       for N (prRow M)))
    end

fun fst (V(ty,n,f)) = f (I 0)
fun snd (V(ty,n,f)) = f (I 1)

fun prArr (a as A(sh,vs)) =
    let val len = Shape.length sh
    in ifUnit (andb(eqi(len,I 2),gti(length vs,I 0)),
               fst sh >>= (fn N =>
                 snd sh >>= (fn M =>
                 prMat N M vs)),
               prAr sh vs) >>= (fn () =>
       printf("\n",[]))
    end

end
