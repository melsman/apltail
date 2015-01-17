(* Pull vectors *)

structure Laila :> LAILA = struct

fun die s = raise Fail ("Laila." ^ s)

structure P = Program

type 'a M = 'a * (P.ss -> P.ss)
datatype t0 = E of P.e
            | V of IL.Type * P.e * (P.e -> t0 M)
            | A of t0 * t0
fun unE (E e) = SOME e
  | unE _ = NONE
fun unV (V v) = SOME v
  | unV _ = NONE

type Value = IL.Value
type t = t0
type v = t0

infix >>=
fun (v,ssT) >>= f = let val (v',ssT') = f v in (v', ssT o ssT') end
fun ret v = (v, fn ss => ss)
fun runM0 (e,ssT) k =
    case unE e of
      SOME e => ssT [k e]
    | NONE => die "runM0: expecting expression"

fun runMss (e,ssT) k =
    case unE e of
      SOME e => ssT (k e)
    | NONE => die "runMss: expecting expression"

open Type
type NUM     = t
type INT     = NUM
type DOUBLE  = NUM
type BOOL    = t

val I : int -> INT = E o P.I
val D : real -> DOUBLE = E o P.D
val B : bool -> BOOL = E o P.B

fun binop opr (t1,t2) =
    case (unE t1,unE t2) of
      (SOME t1, SOME t2) => E(opr(t1,t2))
    | _ => die "binop: expecting expressions"

fun unop opr t =
    case unE t of
      SOME t => E(opr t)
    | _ => die "unop: expecting expression"

fun curry f x y = f(x,y)
fun uncurry f (x,y) = f x y

infix ::=
val op := = P.:=

fun simple f =
    let open P 
        val tyv = Type.Vec Int
        val v = Name.new tyv
        val (e,ssT) = f ($ v)
        val e = case unE e of SOME e => e 
                            | NONE => die "simple: expecting expression"
    in case ssT nil of
           nil => simpleIdx v e
         | _ => false
    end

fun memoize t =
    case unV t of
      SOME (ty,n,f) =>
      if simple f then ret t
      else let open P
               val tyv = Type.Vec ty
               val name = Name.new tyv
               fun ssT ss = Decl(name, SOME(Alloc(tyv,n))) ::
                            (For(n, fn i => runM0(f i)(fn v => (name,i) ::= v)) ss)
           in (V(ty,n, fn i => ret(E(Subs(name,i)))), ssT)
           end
    | _ => die "memoize: expecting vector"

fun mem (A(f,d)) = memoize d >>= (fn d => ret (A(f,d)))
  | mem _ = die "mem: expecting apl array"

fun lett ty t =
    case unE t of
        SOME x =>
        let open P
        in if simpleExp x then ret t
           else let val name = Name.new ty
                    fun ssT ss = Decl(name, SOME x) :: ss
                in (E ($ name), ssT)
                end
        end
      | NONE => die "lett expects an expression"

val letm = ret

local open P
in
  fun map ty f t =
      case unV t of
        SOME (_,n,g) => V(ty, n, fn i => g i >>= f)
      | NONE => die "map: expecting vector"

  fun stride t v =
      case (unE t, unV v) of
        (SOME n, SOME (ty,m,g)) => 
        let val n = max (I 1) n
        in V(ty, m / n, fn i => g(n * i))
        end
      | (NONE, _) => die "stride: expecting expression"
      | _ => die "stride: expecting vector"

  fun map2 ty f t1 t2 =
      case (unV t1, unV t2) of
        (SOME(_,n1,f1), SOME(_,n2,f2)) => 
        V(ty, 
          min n1 n2, fn i => 
                        f1 i >>= (fn v1 =>
                        f2 i >>= (fn v2 =>
                        f(v1,v2))))
      | _ => die "map2: expecting vectors"

  fun rev t =
      case unV t of
        SOME(ty,n,g) => V(ty,n, fn i => g(n - i - I 1))
      | NONE => die "rev: expecting vector"

  fun tabulate ty t f =
      case unE t of
        SOME n => V(ty,n,f o E)
      | NONE => die "tabulate: expecting expression"

  fun proto t =
        if t = Int then I 0
        else if t = Double then D 0.0
        else if t = Bool then B true
        else die ("proto: unsupported type " ^ prType t)

  fun dummy_exp t =
      if t = Int then I 666
      else if t = Double then D 66.6
      else if t = Bool then B false
      else die ("empty: unknown type " ^ prType t)

  fun empty ty = V(ty, I 0, fn _ => ret(E(dummy_exp ty)))

  fun emptyOf t =
      case unV t of
        SOME(ty,_,f) => V(ty, I 0, f)
      | NONE => (*V(I 0, fn _ => ret t)*) die "emptyOf: expecting vector"

  fun single ty t = V(ty, I 1, fn _ => ret t)

  fun tk t v =
      case (unE t, unV v) of
        (SOME n, SOME (ty,m,g)) => V(ty,min n m, g)
      | (NONE, _) => die "tk: expecting expression"
      | _ => die "tk: expecting vector"
             
  fun dr t v =
      case (unE t, unV v) of
        (SOME n, SOME(ty,m,g)) => V(ty,max (m-n) (I 0), fn i => g(i+n))
      | (NONE, _) => die "dr: expecting expression"
      | _ => die "dr: expecting vector"

  fun length t =
      case unV t of
        SOME (_,n,_) => E n
      | NONE => die "length: expecting vector"
end

val op +  : NUM * NUM -> NUM = binop P.+
val op -  : NUM * NUM -> NUM = binop P.-
val op *  : NUM * NUM -> NUM = binop P.*
val op /  : NUM * NUM -> NUM = binop P./
val op %  : INT * INT -> INT = binop P.%
val op <  : NUM * NUM -> BOOL = binop P.<
val op <= : NUM * NUM -> BOOL = binop P.<=
val op >  : NUM * NUM -> BOOL = binop P.>
val op >= : NUM * NUM -> BOOL = binop P.>=
val op == : NUM * NUM -> BOOL = binop P.==
val op noteq : NUM * NUM -> BOOL = binop P.<>
val max   : NUM -> NUM -> NUM = curry(binop(uncurry P.max))
val min   : NUM -> NUM -> NUM = curry(binop(uncurry P.min))
val ~     : NUM -> NUM = unop P.~
val i2d   : INT -> DOUBLE = unop P.i2d
val d2i   : DOUBLE -> INT = unop P.d2i
val b2i   : BOOL -> INT = unop P.b2i

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
fun unE' s t =
    case unE t of
      SOME e => e
    | NONE => die s

fun opt_ss0 ss =
    let val ss = P.se_ss nil ss
        val ss = P.se_ss nil ss
    in P.rm_decls0 ss
    end

fun opt_ss e ss =
    let val ss = P.se_ss nil ss
        val ss = P.se_ss nil ss
    in P.rm_decls (unE' "opt_ss" e) ss
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
        let val (e,ssT) = f (E(IL.Var n0))
        in case unE e of
             SOME e => ssT [k e]
           | NONE => die "runF: expecting expression"
        end)

fun runM _ ta (e,ssT) =
  (Type.Int,
   ta,
   fn (_,k) => runM0 (e,ssT) k)

fun typeComp s c =
    let val (e,_) = c
    in ILUtil.typeExp(unE' ("typeComp." ^ s ^ ": only plain computations are supported") e)
    end

val $ = E o P.$

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
         ($ n, fn ss => P.Decl(n,SOME(P.If(x,unE' "If0.true" e1,
                                           unE' "If0.false" e2)))::ss)
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

fun If (x0,a1,a2) =
    case (unE x0, unV a1, unV a2) of
      (SOME x, SOME (ty,n1,f1), SOME (_,n2,f2)) =>
      V(ty,P.If(x,n1,n2), 
        fn i =>
           let val m1 = f1 i
               val m2 = f2 i
           in If0(x,m1,m2)
           end)
    | _ =>
      case (unE x0, unE a1, unE a2) of
        (SOME x, SOME a1, SOME a2) => E(P.If(x,a1,a2))
      | _ => die "If: expecting branches to be of same kind and cond to be an expression"
           
fun For'(n,e,body) =
    let open P
    in case unI n of
         SOME 0 => (E e, fn ss => ss)
       | SOME 1 => 
         let val ty = ILUtil.typeExp e
             val a = Name.new ty
             fun f e = Decl(a, SOME e)
             val ss = body e f (I 0)
         in case ss of
              [s] => 
              (case unDecl s of
                 SOME (a',SOME e) => if a' = a then (E e, fn ss => ss)
                                     else die "For': weird"
               | SOME (a',NONE) => die "For': weird2"
               | NONE => (E($ a), fn ss => s :: ss))
            | ss0 => (E($ a), fn ss => ss0 @ ss)
         end
       | _ => 
         let val ty = ILUtil.typeExp e
             val a = Name.new ty
             fun f e = a := e
         in (E($ a), fn ss => 
                        Decl(a, SOME e) ::
                        For(n, body ($ a) f) ss)
         end
    end

fun foldl f e v =
    let open P
    in case (unE e, unV v) of
         (SOME e, SOME (_,n,g)) =>
         let fun body e h i =
                 runM0 (g i >>= (fn v => f(v,E e))) h
         in For'(n,e,body)
         end
       | (NONE, SOME (_,n,g)) =>
         (case unI n of
            SOME n =>
            let fun loop i a = if Int.>=(i,n) then ret a
                               else g (I i) >>= (fn v => 
                                    f (v,a) >>= (fn x => 
                                    loop(Int.+(i,1)) x))
            in loop 0 e
            end
          | _ => die "foldl: expecting expression as accumulator or index vector to be constant-sized")
       | (_, NONE) => die "foldl: expecting vector to iterate over"
    end

fun foldr f e v =
      case (unE e, unV v) of
        (SOME e, SOME (_,n,g)) =>
        let open P
            fun body e h i =
                runM0 (g (n - I 1 - i) >>= (fn v => f(v,E e))) h
        in For'(n,e,body)
        end
      | (NONE, _) => die "foldr: expecting expression as accumulator"
      | (_, NONE) => die "foldr: expecting vector to iterate over"
                     
fun concat v1 v2 =
    case (unV v1, unV v2) of
      (SOME(ty,n1,f1), SOME(_,n2,f2)) => 
      (case P.unI n1 of
         SOME 0 => v2
       | _ =>
         case P.unI n2 of
           SOME 0 => v1
         | _ => V(ty,P.+(n1,n2), 
                  fn i => 
                     let val m1 = f1 i
                         val m2 = f2 (P.-(i,n1))
                     in If0(P.<(i,n1),m1,m2)
                     end))
    | _ => die "concat: expecting vectors"
             
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
          val ts = List.map (fn e => case unE e of
                                         SOME t => t
                                       | NONE => die "fromListM: expecting expressions") ts
          fun ssT ss = Decl(name, SOME(Vect(tyv,ts))) :: ss
      in (V(ty,sz, fn i => ret(E(Subs(name,i)))), ssT)
      end

  fun assert_vector s v =
      case unV v of
        SOME _ => ()
      | NONE => die ("assert_vector: " ^ s)
                       
  fun flatten ty v =
      (assert_vector "flatten" v;
       foldl (fn (v,a) =>
                 (assert_vector "flatten_a" a;
                  assert_vector "flatten_v" v;
                  ret(concat a v))) (empty ty) v)

  fun flattenOf v0 v =
      (assert_vector "flatten" v;
       foldl (fn (v,a) =>
                 (assert_vector "flatten_a" a;
                  assert_vector "flatten_v" v;
                  ret(concat a v))) (emptyOf v0) v)

  infix ==
  fun eq f v1 v2 =
      let val v = map2 Bool (ret o f) v1 v2
          val base = length v1 == length v2
      in foldr (fn (b,a) => ret(If(a,b,a))) base v
      end

  fun sub_unsafe v i =
    case (unV v, unE i) of
      (SOME (_,n,g), SOME i) => g i
    | _ => die "sub_unsafe: expecting vector and integer"

  fun tyOfV v =
      case unV v of
          SOME (ty,_,_) => ty
        | NONE => die "tyOfV: expecting vector"

  fun merge v n t =
      concat (tk (n - I 1) v)
      (concat (single (tyOfV v) t) (dr (n + I 1) v))

  infix %
            
  fun lprod nil = I 1
    | lprod (x::xs) = x * lprod xs
  
  fun toSh nil i = ret nil
    | toSh (x::xs) i =
      lett Int (lprod xs) >>= (fn p =>
      lett Int (i%p) >>= (fn imodp =>
      toSh xs imodp >>= (fn xs' =>
      ret (i / p :: xs'))))

  fun fromSh nil nil = ret(I 0)
    | fromSh (_::sh) (i::idx) = fromSh sh idx >>= (fn x => ret (i * lprod sh + x))
    | fromSh _ _ = die "fromSh: dimension mismatch"

  fun getShape f 0 = ret nil
    | getShape f n = 
      f (P.I (Int.-(n,1))) >>= (fn N =>
      getShape f (Int.-(n,1)) >>= (fn NS =>
      ret (NS @ [N])))

  fun trans v d =
      case (unV v, unV d) of
        (SOME (_,n,f), SOME(ty,m,g)) =>
        let fun h n i =
                getShape f n >>= (fn sh => 
                let val i = E i
                    val sh' = List.rev sh
                in toSh sh' i >>= (fn sh'' =>
                   fromSh sh (List.rev sh'') >>= (fn x =>
                   case unE x of SOME x => g x
                               | NONE => die "trans:impossible"))
                end)
        in case P.unI n of
               SOME 0 => ret d   (* known number of dimensions *)
             | SOME 1 => ret d
             | SOME n => ret (V(ty,m,h n))
             | NONE => (*ret(If(E n < I 2, d, V(ty,m,g')))*)
                die "trans: unknown number of dimensions not supported"
        end
      | _ => die "trans: expecting vectors"

  fun exchange nil xs = nil
    | exchange (i::rest) xs = List.nth (xs,Int.-(i,1)) :: exchange rest xs

  fun appi0 _ f nil = ()
    | appi0 n f (x::xs) = (f (x,n); appi0 (Int.+(n,1)) f xs)
  
  fun exchange' (ctrl:int list) (xs: 'a list) : 'a list =
      let val sz = List.length ctrl
          val a = Array.tabulate (sz,fn _ => List.nth(xs,0))
      in appi0 0 (fn (c,i) => Array.update(a,Int.-(c,1),List.nth(xs,i))) ctrl
       ; Array.foldr(op::) nil a
      end
          
  fun trans2 idxs sh vs =
      let fun check n =
              if n = 0 then ()
              else if List.exists (fn x => x = n) idxs then
                check (Int.-(n,1))
              else die "trans2: index vector not a permutation"
      in check (List.length idxs)
       ; case (unV sh, unV vs) of
             (SOME (_,n,f), SOME(ty,m,g)) =>
             (case P.unI n of
                  SOME n =>  (* known number of dimensions *)
                   (if n <> List.length idxs then
                      die "trans2: wrong index vector length"
                    else if Int.<(n,2) then ret (sh,vs)
                    else getShape f n >>= (fn sh =>
                         let val sh' = exchange' idxs sh
                             fun h i =
                                 let val i = E i
                                 in toSh sh' i >>= (fn sh'' =>
                                    fromSh sh (exchange idxs sh'') >>= (fn x =>
                                    case unE x of SOME x => g x
                                                | NONE => die "trans2:impossible"))
                                 end
                         in ret (fromList Int sh',
                                 V(ty,m,h))
                         end))
                | NONE => die "trans2: unknown number of dimensions not supported"
             )
           | _ => die "trans2: expecting vectors"
      end

  fun compr bv v =
      case (unV bv, unV v) of
        (SOME (_,n,f), SOME(ty,m,g)) =>
        foldl (ret o op +) (I 0) (map Int (ret o b2i) bv) >>= (fn sz =>
        let open P
            val tyv = Type.Vec ty
            val name = Name.new tyv
            val count = Name.new Type.Int
            val sz = case unE sz of
                         SOME sz => sz
                       | _ => die "compr.unE"
            fun ssT ss = Decl(name, SOME(Alloc(tyv,sz))) ::
                         Decl(count, SOME(I 0)) ::
                         For(n, fn i => runMss(f i)(fn b =>
                           Ifs(b,
                               runMss(g i)(fn v =>
                                  [(name, $count) ::= v,
                                   count := ($count) + I 1]),
                               []) 
                           []))
                         ss
        in (V(ty,sz,fn i => ret(E(Subs(name,i)))),
            ssT)
        end)
       | _ => die "compr"


  fun extend n v =
      case (unE n, unV v) of
        (SOME n', SOME(ty,m,f)) =>
        If(E m == I 0, V(ty,n', fn _ => ret (E(proto ty))), V(ty,n',f o (fn i => P.%(i, m))))
      | _ => die "extend: expecting term and array"

  fun outmain outln =
    ( outln "int main() {"
    ; outln "  printf(\"%f\\n\", kernel(0));"
    ; outln "  return 0;"
    ; outln "}")

  fun outprog ofile p =
    let val body = pp_prog p
        val os = TextIO.openOut ofile
        fun outln s = TextIO.output (os, s^"\n")
    in outln "#include <stdio.h>"
     ; outln "#include <stdlib.h>"
     ; outln "#include <math.h>"
     ; outln "#include \"apl.h\""
     ; outln body
     ; outmain outln
     ; TextIO.closeOut os
     ; print ("Wrote file " ^ ofile ^ "\n")
    end

val addi = op +
val subi = op -
val muli = op *
val divi = op /
fun resi(x,y) = If(x == I 0, y, y % x)
val lti = op <
val ltei = op <=
val gti = op >
val gtei = op >=
val eqi = op ==
val neqi = op noteq
fun maxi(x,y) = max x y
fun mini(x,y) = min x y
val negi = ~
fun signi x = If(lti(x,I 0),I ~1,I 1)
val addd = op +
val subd = op -
val muld = op *
val divd = op /
val resd = fn _ => die "resd not yet supported"
val ltd = op <
val lted = op <=
val gtd = op >
val gted = op >=
val eqd = op ==
val neqd = op noteq
fun maxd(x,y) = max x y
fun mind(x,y) = min x y
val negd = ~
fun signd x = If(ltd(x,D 0.0),I ~1,I 1)

infix >>= == %

structure Shape : sig
  type t = t0
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
  type t = t0
  val concat   = concat
  val empty    = empty Int
  val singlez  = single Int (I 0)
  val single   = single Int
  val product  : t -> INT M = foldl (ret o op *) (I 1)
  val length   : t -> INT = length
  val dr       : INT -> t -> t = dr
  val tk       : INT -> t -> t = tk
  val eq       : t -> t -> BOOL M = eq (op ==)
end

type m = t0

(* invariant: A(s,d) ==> product s = length d *) 

fun vec c = A(Shape.single (length c), c)
fun scl ty v = A(Shape.empty, single ty v)
fun first _ = die "first unimplemented"
fun zilde ty = A(Shape.singlez, empty ty)
fun iota n = vec (tabulate Int n (fn x => ret(x + (I 1))))
fun iota' _ = die "iota' not implemented"
fun shape (A(f,_)) = f
  | shape _ = die "shape: expecting apl array"
fun snd (A(_,c)) = c
  | snd _ = die "snd: expecting apl array"
fun siz (A(_,c)) = length c
  | siz _ = die "siz: expecting apl array"
fun dim (A(f,_)) = Shape.length f
  | dim _ = die "dim: expecting apl array"
              
(* Restructuring *)
fun rav (A(_,c)) = vec c
  | rav _ = die "rav: expecting apl array"

val rav0 = snd

fun mif (x, A(f1,v1), A(f2,v2)) = A(If(x,f1,f2), If(x,v1,v2))
  | mif _ = die "mif: expecting apl arrays"

fun zildeOf (A(f,v)) = A(Shape.singlez, emptyOf v)
  | zildeOf _ = die "zildeOf: expecting apl array"

fun each ty g (A(f,cs)) = A(f, map ty g cs)
  | each _ _ _ = die "each: expecting apl array"
      
fun meq _ f (A(f1,c1)) (A(f2,c2)) =
    Shape.eq f1 f2 >>= (fn shape_eq => 
     eq f c1 c2 >>= (fn content_eq =>
      ret(If(shape_eq,content_eq,B false))))
  | meq _ _ _ _ = die "meq: expecting apl arrays"

fun zipWith ty g a b =
    let val sha = shape a
	val shb = shape b
        val mv = A(sha,map2 ty g (snd a) (snd b))
    in ret mv 
       (*Shape.eq sha shb >>= (fn shapeeq =>
       ret(mif(shapeeq, mv, zilde ty)))*)
    end

fun pre (a: v) : v =
    let val n = length a
        val iotan = tabulate Int n (fn x => ret(x + I 1))
    in map (Vec(tyOfV a)) (fn i => ret(tk i a)) iotan
    end

fun mapm emp f xs = 
    foldl (fn (x,a) => f x >>= (fn y => ret(concat a (single (tyOfV a) y)))) emp xs

fun scan0 g e a = 
    mapm (emptyOf e) (foldl (ret o g) e) (pre a)

fun scan _ _ g e (A(f,c)) = scan0 g e c >>= (fn c' => ret(A(f,c')))
  | scan _ _ _ _ _ = die "scan: expecting apl array"

fun pad1 s = If(length s == I 0, single Int (I 1), s) 
fun catenate_first (A(s1,d1) : m) (A(s2,d2): m) : m M =
      let val s1 = pad1 s1
          val s2 = pad1 s2                   
          val s1' = Shape.dr (I 1) s1
          val s2' = Shape.dr (I 1) s2
          val v1 = Shape.tk (I 1) s1
          val v2 = Shape.tk (I 1) s2
          val x = map2 Int (ret o op +) v1 v2
          val mv = A(Shape.concat x s1',
                     concat d1 d2)
      in ret mv (*Shape.eq s1' s2' >>= (fn shapeeq =>
          ret(mif(shapeeq,mv,zildeOf t1)))*)
      end
  | catenate_first _ _ = die "catenate_first: expecting apl arrays"

fun take0 n (t : m) : m = vec(tk n (snd t))
fun drop0 n (t : m) : m = vec(dr n (snd t))

val take : INT -> m -> m = fn n => fn t =>
   mif(n < I 0, drop0 (siz t + n) t, take0 n t)
and drop : INT -> m -> m = fn n => fn t =>
   mif(n < I 0, take0 (siz t + n) t, drop0 n t)

fun rotate n (A(f,d)) =
    let val sz = length d
    in mif(n < I 0,
           A(f,concat (dr (sz + n) d) (tk (sz + n) d)),
           A(f,concat (dr n d) (tk n d)))
    end
  | rotate _ _ = die "rotate: expecting apl array"

fun reshape (f: v) (a: m) : m M =
    Shape.product f >>= (fn p =>
    ret(A(f,extend p (snd a))))

fun transpose (A(s,d)) = trans s d >>= (fn v => ret(A(rev s, v)))
  | transpose _ = die "transpose: expecting array"

fun transpose2 v (A(s,d)) = trans2 v s d >>= (fn p => ret(A p))
  | transpose2 _ _ = die "transpose2: expecting array"

fun reverse _ = raise Fail "ilapl.reverse: not implemented"

fun catenate t1 t2 = 
    transpose t1 >>= (fn a1 =>
    transpose t2 >>= (fn a2 =>
    catenate_first a1 a2 >>= transpose))

fun eOfT t =
    case unE t of
        SOME e => e
      | NONE => die "APL.eOfT: expecting E"

fun reduce _ f e (A(s,d)) scalar vector =
      let val r = length s
      in case unE r of
           SOME r =>
           (case P.unI r of
              SOME 1 => foldl f e d >>= (ret o scalar)
            | SOME 2 =>  (* matrix: M x N *)
              let 
              in sub_unsafe s (I 0) >>= (fn M =>
                 sub_unsafe s (I 1) >>= (fn N =>
                 ret (A(Shape.single M,
                    V(tyOfV d, eOfT M,
                      fn i => foldl f e (tk N (dr (E i * N) d))))))) >>= (ret o vector)
              end
            | SOME n =>
              sub_unsafe s (I(Int.-(n,1))) >>= (fn N =>
              let val sh' = Shape.tk (I(Int.-(n,1))) s
              in Shape.product sh' >>= (fn M =>
                 ret (vector (
                   A(sh',
                     V(tyOfV d, eOfT M,
                        fn i => foldl f e (tk N (dr (E i * N) d)))))))
              end)
            | NONE => die "reduce: unknown rank not supported")
         | _ => die "reduce: expecting length to return an expression"
      end
  | reduce _ _ _ _ _ _ = die "reduce: expecting vector"

fun build2 ty M1 M2 f =
    case unE (M1*M2) of
        SOME N => ret (V(ty, N, fn i => f (E i / M2) (E i % M2)))
      | NONE => die "build2.expecting expression"
    
fun assert_length s n v =
    let val r = length v
    in case unE r of
           SOME r => (case P.unI r of
                          SOME i => if i = n then ()
                                    else die ("assert_length: " ^ s)
                        | NONE => die ("assert_length: unknown length: " ^ s))
         | NONE => die ("assert_length: not expression: " ^ s)
    end

fun compress (A(sh_is,vs_is), A(sh_vs,vs_vs)) =
    (assert_length "rank of bool array argument to compress must be 1" 1 sh_is;
     assert_length "rank of source array argument to compress must be 1" 1 sh_vs;
     compr vs_is vs_vs >>= (fn vs =>
     ret (A (Shape.single(length vs),vs))))
  | compress _ = die "compress.expecting arrays"

end
