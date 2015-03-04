structure IL = struct

val optimisationLevel = ref 0
fun optlevel() = !optimisationLevel

fun die s = raise Fail ("IL." ^ s)

datatype Type = Int | Double | Bool | Char | Vec of Type

structure Name :> sig
  eqtype t
  val new : Type -> t
  val pr : t -> string
  val typeOf : t -> Type
  val iref   : t -> int ref
end =
struct
  type t = string * Type * int ref
  val count = ref 0
  fun prefix Int = "n"
    | prefix Double = "d"
    | prefix Bool = "b"
    | prefix Char = "c"
    | prefix (Vec _) = "a"
  fun new t = (prefix t ^ Int.toString(!count) before count := !count + 1, t, ref 0)
  fun pr (s,_,_) = s
  fun typeOf (_,t,_) = t
  fun iref (_,_,iref) = iref
end

structure NameSet = OrderSet(struct type t = Name.t
                                    fun compare (x, y) = String.compare (Name.pr x, Name.pr y)
                             end)
datatype value =
         IntV of int
       | DoubleV of real
       | BoolV of bool
       | CharV of word
       | ArrV of value option ref vector
datatype Unop = Neg | I2D | D2I | B2I | Not | Floor | Ceil | Ln | Sin | Cos | Tan | Expd | Sqrt | Roll | Now | Strlen 
datatype Binop = Add | Sub | Mul | Divv | Modv | Resi | Mini | Maxi | Mind | Maxd | Lt | Lteq | Eq | Andb | Orb | Xorb | Powd | Ori | Andi | Xori | Shli | Shri | Shari
datatype Exp =
         Var of Name.t
       | I of Int32.int
       | D of real
       | T | F
       | C of word
       | If of Exp * Exp * Exp
       | Subs of Name.t * Exp
       | Alloc of Type * Exp
       | Vect of Type * Exp list
       | Binop of Binop * Exp * Exp
       | Unop of Unop * Exp
                
datatype Stmt = For of Exp * Name.t * Block  (* name is a binding occurence *)
              | Ifs of Exp * Block * Block
              | Assign of Name.t * Exp
              | AssignArr of Name.t * Exp * Exp
              | Decl of Name.t * Exp option
              | Nop
              | Free of Name.t
              | Ret of Exp
              | Halt of string
              | Printf of string * Exp list
              | Sprintf of Name.t * string * Exp list
              | ReadIntVecFile of Name.t * Name.t * Exp
              | ReadDoubleVecFile of Name.t * Name.t * Exp
              | Comment of string

withtype Block = Stmt list

(* kernel names, kernel definitions, and programs *)
type kname = string
type kd = kname * Name.t list * Block
type p = kd list * Block

fun eq(e1,e2) =
    case (e1, e2) of
      (Var n1, Var n2) => n1 = n2
    | (I i1, I i2) => i1 = i2
    | (D d1, D d2) => Real.==(d1, d2)
    | (T,T) => true
    | (F,F) => true
    | (C w1, C w2) => w1 = w2
    | (If(a1,a2,a3), If(b1,b2,b3)) => eq(a1,b1) andalso eq(a2,b2) andalso eq(a3,b3)
    | (Subs(n1,a),Subs(n2,b)) => n1=n2 andalso eq(a,b)
    | (Alloc(t1,a),Alloc(t2,b)) => t1 = t2 andalso eq(a,b)
    | (Vect(t1,a),Vect(t2,b)) => t1 = t2 andalso eqs(a,b)
    | (Binop(p1,a1,a2),Binop(p2,b1,b2)) => p1=p2 andalso eq(a1,b1) andalso eq(a2,b2)
    | (Unop(p1,a1),Unop(p2,b1)) => p1=p2 andalso eq(a1,b1)
    | _ => false
and eqs (nil,nil) = true
  | eqs (x::xs,y::ys) = eq(x,y) andalso eqs(xs,ys)
  | eqs _ = false
fun eq_s(s1,s2) =
    case (s1, s2) of
      (For _, For _) => false
    | (Ifs(e1,ss11,ss12), Ifs(e2,ss21,ss22)) => false
    | (Assign (n1,e1), Assign (n2,e2)) => n1 = n2 andalso eq(e1,e2)
    | (AssignArr(n1,e1,e2), AssignArr(n2,e12,e22)) => false
    | (Decl (n1,NONE), Decl(n2,NONE)) => n1 = n2
    | (Decl (n1,SOME e1), Decl(n2,SOME e2)) => n1 = n2 andalso eq(e1,e2)
    | (Nop,Nop) => true
    | (Free n1, Free n2) => n1 = n2
    | (Ret e1, Ret e2) => eq(e1,e2)
    | (Halt s1, Halt s2) => s1 = s2
    | (Printf(s1,es1), Printf(s2,es2)) => s1 = s2 andalso eqs(es1,es2)
    | (Sprintf(n1,s1,es1), Sprintf(n2,s2,es2)) => n1 = n2 andalso s1 = s2 andalso eqs(es1,es2)
    | (ReadIntVecFile(n1,n1',e1), ReadIntVecFile(n2,n2',e2)) => n1 = n2 andalso n1' = n2' andalso eq(e1,e2)
    | (ReadDoubleVecFile(n1,n1',e1), ReadDoubleVecFile(n2,n2',e2)) => n1 = n2 andalso n1' = n2' andalso eq(e1,e2)
    | (Comment s1, Comment s2) => s1 = s2
    | _ => false
and eq_ss (nil,nil) = true
  | eq_ss (s1::ss1,s2::ss2) = eq_s(s1,s2) andalso eq_ss(ss1,ss2)
  | eq_ss _ = false

end

structure Type : TYPE = struct
  type T = IL.Type
  val Int = IL.Int
  val Double = IL.Double
  val Bool = IL.Bool
  val Char = IL.Char
  val Vec = IL.Vec
  fun prType IL.Int = "int"
    | prType IL.Double = "double"
    | prType IL.Bool = "bool"
    | prType IL.Char = "char"
    | prType (IL.Vec t) = prType t ^ "*"
  fun vecElem (IL.Vec t) = t
    | vecElem t = raise Fail ("vecElem: Expecting vector type - got " ^ prType t)
end

signature NAME = sig
  eqtype t
  val new : Type.T -> t
  val pr  : t -> string
end

structure Name : NAME = IL.Name

signature PROGRAM = sig
  type e
  val Var   : Name.t -> e
  val Subs  : Name.t * e -> e
  val Alloc : Type.T * e -> e
  val Vect  : Type.T * e list -> e
  val I     : int -> e
  val D     : real -> e
  val B     : bool -> e
  val C     : word -> e
  val If    : e * e * e -> e
  val strlen : e -> e

  val addi  : e * e -> e   (* interger operations *)
  val subi  : e * e -> e
  val muli  : e * e -> e
  val divi  : e * e -> e
  val modi  : e * e -> e
  val lti   : e * e -> e
  val ltei  : e * e -> e
  val gti   : e * e -> e
  val gtei  : e * e -> e
  val eqi   : e * e -> e
  val neqi  : e * e -> e
  val maxi  : e * e -> e
  val mini  : e * e -> e
  val resi  : e * e -> e
  val ori   : e * e -> e
  val andi  : e * e -> e
  val xori  : e * e -> e
  val shli  : e * e -> e
  val shri  : e * e -> e
  val shari : e * e -> e
  val negi  : e -> e
  val nowi  : e -> e

  val addd  : e * e -> e  (* double operations *)
  val subd  : e * e -> e
  val muld  : e * e -> e
  val divd  : e * e -> e
  val modd  : e * e -> e
  val ltd   : e * e -> e
  val lted  : e * e -> e
  val gtd   : e * e -> e
  val gted  : e * e -> e
  val eqd   : e * e -> e
  val neqd  : e * e -> e
  val maxd  : e * e -> e
  val mind  : e * e -> e
  val negd  : e -> e

  val eqc   : e * e -> e

  val orb   : e * e -> e   (* boolean operations *)
  val andb  : e * e -> e
  val xorb  : e * e -> e
  val eqb   : e * e -> e
  val neqb  : e * e -> e

  val i2d   : e -> e   (* miscellaneous operations *) 
  val d2i   : e -> e
  val b2i   : e -> e
  val ceil  : e -> e
  val floor : e -> e
  val ln    : e -> e
  val sin   : e -> e
  val cos   : e -> e
  val tan   : e -> e
  val expd  : e -> e
  val sqrt  : e -> e
  val powd  : e * e -> e
  val notb  : e -> e
  val roll  : e -> e
  val unI   : e -> int option
  val unB   : e -> bool option
  val unD   : e -> real option

  type s
  type ss = s list
  val For : e * Name.t * ss -> ss -> ss
  val Ifs : e * ss * ss -> ss -> ss
  val :=  : Name.t * e -> s
  val ::= : (Name.t * e) * e -> s
  val Decl: Name.t * e option -> s
  val Ret : e -> s
  val Free : Name.t -> s
  val Halt : string -> s
  val Printf : string * e list -> s
  val Sprintf : Name.t * string * e list -> s
  val ReadIntVecFile : Name.t * Name.t * e -> s
  val ReadDoubleVecFile : Name.t * Name.t * e -> s
  val Comment : string -> s
  val emp : s
  val unDecl : s -> (Name.t * e option) option

  type kd = string * Name.t list * ss
  type p = kd list * ss

  (* Basic analyses *)
  val uses      : e -> IL.NameSet.set -> IL.NameSet.set
  val usess     : e list -> IL.NameSet.set -> IL.NameSet.set
  val uses_s    : s -> IL.NameSet.set
  val uses_ss   : ss -> IL.NameSet.set
  val defs_s    : s -> IL.NameSet.set
  val defs_ss   : ss -> IL.NameSet.set
  val decls_s   : s -> IL.NameSet.set

  (* static evaluation *)
  datatype info = EqI of e
                | LtI of e
                | GtEqI of e 
  type env = (Name.t * info) list

  (* static evaluation *)
  val se_e  : env -> e -> e
  val se_ss : env -> ss -> ss

  (* remove unused declarations *)
  val rm_decls : e -> ss -> ss
  val rm_decls0 : ss -> ss

  val simpleIdx    : Name.t -> e -> bool
  val simpleExp    : e -> bool
  val side_effects : e -> bool
end

structure Program : PROGRAM = struct
local open IL
in
  type e = Exp

  fun closed e =
      case e of
         Var _ => false
       | I _ => true
       | D _ => true
       | T => true
       | F => true
       | C _ => true
       | If(e0,e1,e2) => closed e0 andalso closed e1 andalso closed e2
       | Subs _ => false
       | Alloc (_,e) => closed e
       | Vect (_,es) => List.all closed es
       | Binop(_,e1,e2) => closed e1 andalso closed e2
       | Unop(_,e) => closed e

  fun simpleIdx v e =
      case e of
          Subs(_,Var n) => n = v
        | _ => false

  fun simpleExp e =
      case e of
          Var _ => true
        | I _ => true
        | D _ => true
        | T => true
        | F => true
        | C _ => true
        | _ => false

  fun side_effects e =
      case e of
          Alloc _ => true
        | Unop(unop,e) => unop = Now orelse unop = Roll orelse side_effects e
        | If (e1,e2,e3) => side_effects e1 orelse side_effects e2 orelse side_effects e3
        | T => false
        | F => false
        | Var _ => false
        | I _ => false
        | D _ => false        
        | C _ => false
        | Subs _ => false
        | Vect (_,nil) => false
        | Vect(t,e::es) => side_effects e orelse side_effects(Vect(t,es))
        | Binop(binop,e1,e2)  => side_effects e1 orelse side_effects e2

  structure N = NameSet
  fun uses e acc =
      case e of
         Var n => N.insert (acc,n)
       | I _ => acc
       | D _ => acc
       | T => acc
       | F => acc
       | C _ => acc
       | If(e0,e1,e2) => uses e0 (uses e1 (uses e2 acc))
       | Subs (n,e) => uses e (N.insert (acc,n))
       | Alloc (_,e) => uses e acc
       | Vect (t,es) => usess es acc
       | Binop(_,e1,e2) => uses e1 (uses e2 acc)
       | Unop(_,e) => uses e acc
  and usess nil acc = acc
    | usess (e::es) acc = uses e (usess es acc)

  fun decls_s s =
      case s of
          IL.Decl(n,_) => N.singleton n
        | _ => N.empty

  fun defs_s s =
      case s of
          IL.Nop => N.empty
        | IL.Ret _ => N.empty
        | IL.Halt _ => N.empty
        | IL.Free n => N.empty
        | IL.Decl(n,_) => N.empty
        | IL.Assign(n,_) => N.singleton n
        | IL.AssignArr(n,_,_) => N.singleton n
        | IL.For(_,n,body) => N.remove (defs_ss body,n)
        | IL.Ifs(_,ss1,ss2) => N.union(defs_ss ss1,defs_ss ss2)
        | IL.Printf _ => N.empty
        | IL.Sprintf (n,_,_) => N.singleton n
        | IL.ReadIntVecFile (n1,n2,_) => N.fromList[n1,n2]
        | IL.ReadDoubleVecFile (n1,n2,_) => N.fromList[n1,n2]
        | IL.Comment _ => N.empty

  and defs_ss nil = N.empty
    | defs_ss (s::ss) = N.union(defs_s s, N.difference(defs_ss ss,decls_s s))

  fun uses_s s =
      case s of
          IL.Nop => N.empty
        | IL.Ret e => uses e N.empty
        | IL.Halt _ => N.empty
        | IL.Free n => N.singleton n
        | IL.Decl(n,SOME e) => uses e N.empty
        | IL.Decl(n,NONE) => N.empty
        | IL.Assign(n,e) => uses e N.empty
        | IL.AssignArr(n,e1,e2) => uses e1 (uses e2 (N.singleton n))
        | IL.For(e,n,body) => uses e (N.remove(uses_ss body,n))
        | IL.Ifs(e,ss1,ss2) => uses e (N.union(uses_ss ss1,uses_ss ss2))
        | IL.Printf(_,es) => usess es N.empty
        | IL.Sprintf(n,_,es) => usess es N.empty                      (* notice: n is not used by the statement *)
        | IL.ReadIntVecFile (n1,n2,e) => uses e (N.singleton n2)      (* notice: n1 is not used by the statement *)
        | IL.ReadDoubleVecFile (n1,n2,e) => uses e (N.singleton n2)   (* notice: n1 is not used by the statement *)
        | IL.Comment _ => N.empty
  and uses_ss nil = N.empty
    | uses_ss (s::ss) = N.union(uses_s s, N.difference(uses_ss ss,N.union(decls_s s,defs_s s)))

(*
  fun dce ss =
      case ss of
        nil => (nil,N.empty)
      | s :: ss =>
        let val (ss',U) = dce ss
        in case s of
             IL.For(e,f) =>
             let val n = Name.new Type.Int
                 val body = f(Var n)
             in
             end
           | _ =>
             let val D = defs_s s
             in if N.isEmpty (N.intersect(D,U)) then
                  (ss',U)
                else
                  (s :: ss', N.union(uses_s s,N.difference(U,D))
             end
        end
*)

  fun nevereq(a,b) =
      case (a, b) of
        (IL.T,IL.F) => true
      | (IL.I x,IL.I y) => x <> y
      | (IL.D x,IL.D y) => Bool.not(Real.==(x,y))
      | (IL.C x,IL.C y) => x <> y
      | _ => false

  fun If(IL.T,b,c) = b
    | If(IL.F,b,c) = c
    | If(a,b,c) = 
      let fun default() = 
              if IL.eq(b,c) then b 
              else if IL.eq(b, IL.T) andalso IL.eq(a,c) then a
              else if IL.eq(c, IL.F) andalso IL.eq(a,b) then a
              else if IL.eq(b, IL.F) andalso IL.eq(a,c) then IL.F
              else if IL.eq(c, IL.T) andalso IL.eq(a,b) then IL.T
              else 
                case c of
                  IL.If(d,e,f) => if eq(a,d) then If(a,b,f) else IL.If(a,b,c)
                | _ => IL.If(a,b,c)
      in case a of
           IL.Binop(Eq,IL.If(x,y,z),e) => 
           (* If(If(x,y,z)==y,s,t) => If(x,s,t) and S(z)<>S(y), for all S *)
           if eq(y,e) andalso nevereq(z,y) then If(x,b,c)
           else default()
         | IL.Binop(Eq,c',b') => if eq(c,c') andalso eq(b,b') then c 
                                 else default()
         | _ => default()
      end

  fun Var n = IL.Var n
  fun I n = IL.I n
  fun unI (IL.I n) = SOME n
    | unI _ = NONE
  fun D n = IL.D n
  fun C c = IL.C c
  fun unD (IL.D n) = SOME n
    | unD _ = NONE
  fun B true = T
    | B false = F 
  fun unB T = SOME true
    | unB F = SOME false
    | unB _ = NONE

  fun comp0 neg t acc =
      case t of
          IL.I i =>
          let val a = case acc of
                          SOME(IL.I a) => a
                        | SOME _ => die "comp0: expecting I"
                        | NONE => 0
          in (SOME(IL.I(if neg then Int32.-(a,i) else Int32.+(a,i))), fn x => x)
          end
       | IL.D d =>
          let val a = case acc of
                          SOME(IL.D a) => a
                        | SOME _ => die "comp0: expecting D"
                        | NONE => 0.0
          in (SOME(IL.D(if neg then Real.-(a,d) else Real.+(a,d))), fn x => x)
          end
        | IL.Binop(IL.Add,a,b) => 
          let val (acc, f) = comp0 neg a acc
              val (acc, g) = comp0 neg b acc
          in (acc, f o g)
          end
        | IL.Binop(IL.Sub,a,b) =>
          let val (acc, f) = comp0 neg a acc
              val (acc, g) = comp0 (Bool.not neg) b acc
          in (acc, f o g)
          end        
        | IL.Var _ => (acc, fn x => Binop(if neg then IL.Sub else IL.Add, x, t))
        | _ => raise Fail "no"

  and comp p a b =
      let val t = Binop(p,a,b)
      in let val (acc, f) = comp0 false t NONE
         in case acc of
                SOME a => f a
              | NONE => t
         end handle Fail _ => t
      end

  and a        - (IL.I 0) = a
    | (IL.I a) - (IL.I b) = I(Int32.-(a,b))
    | (IL.D a) - (IL.D b) = D(Real.-(a,b))
    | (Binop(Sub,IL.I a,b)) - (IL.I c) = I(Int32.-(a,c)) - b 
    | (Binop(Sub,a,IL.I b)) - (IL.I c) = a - I(Int32.+(b,c)) 
    | (IL.If(x,y,z)) - (IL.I a) = If(x,y-(I a),z-(I a))
    | (Binop(Add,a,IL.I b)) - (IL.I c) = a + (I (Int32.-(b,c)))
    | (Binop(Add,IL.I a,b)) - (IL.I c) = I (Int32.-(a,c)) + b
    | a        - b        = comp Sub a b

  and (IL.I 0)              + b          = b
    | a                     + (IL.I 0)   = a
    | (IL.I a)              + (IL.I b)   = I(Int32.+(a,b))
    | (IL.D a)              + (IL.D b)   = D(Real.+(a,b))
    | (Binop(Sub,a,IL.I b)) + (IL.I c)   = let val d = Int32.-(c,b)
                                           in if d > 0 then a + (I d)
                                              else a - I (~d)
                                           end
    | (Binop(Sub,IL.I a,b)) + (IL.I c) = I(Int32.+(a,c)) - b
    | (Binop(Add,x,IL.I a)) + (IL.I b) = x + I (Int32.+(a,b))
    | (Binop(Add,IL.I a,x)) + (IL.I b) = x + I (Int32.+(a,b))
    | (IL.I b) + (Binop(Add,IL.I a,x)) = x + I (Int32.+(a,b))
    | (IL.I b) + (Binop(Add,x,IL.I a)) = x + I (Int32.+(a,b))
    | (IL.If(x,y,z))        + (IL.I a) = If(x,y+(I a),z+(I a))
    | (IL.I a)              + (IL.If(x,y,z)) = If(x,y+(I a),z+(I a))
    | (a as IL.If(e,x,y))   + (b as IL.If(e',x',y')) = if eq(e,e') then If(e,x+x',y+y')
                                                       else comp Add a b
    | a                     + b         =
      case (a,b) of
          (IL.D r, _) => if Real.==(r,0.0) then b
                         else comp Add a b
        | (_, IL.D r) => if Real.==(r,0.0) then a
                         else comp Add a b
        | _ => comp Add a b

  fun (IL.I a) * (IL.I b) = I(Int32.*(a,b))
    | (IL.D a) * (IL.D b) = D(Real.*(a,b))
    | (IL.I 1) * b        = b
    | a        * (IL.I 1) = a
    | (IL.I 0) * b        = I 0
    | a        * (IL.I 0) = I 0
    | a        * b        = Binop(Mul,a,b)

  fun x / y =
      let val def = Binop(Divv,x,y)
      in case (x, y) of
             (IL.I a, IL.I b) => (I(Int32.div(a,b)) handle _ => def)
           | (IL.D a, IL.D b) => (D(Real./(a,b)) handle _ => def)
           | (a, IL.I 1) => a
           | _ => def
      end

  infix %
  fun x % y =
      let val def = Binop(Modv,x,y)
      in case (x,y) of
             (IL.I a, IL.I b) => (I(Int32.mod(a,b)) handle _ => def)
           | (Binop(Modv,a,IL.I b), IL.I c) =>
             if Int32.<= (b,c) then x else Binop(Modv,x,IL.I c)
           | (a, IL.I 1) => I 0
           | _ => def
      end

  fun mini (IL.I a, IL.I b) = I(if a < b then a else b)
    | mini (y as IL.I d, x as IL.If(a,IL.I b,IL.I c)) = 
      if Int32.<=(b,d) andalso Int32.<=(c,d) then x else
      if Int32.<=(d,b) andalso Int32.<=(d,c) then y else Binop(Mini,y,x)
    | mini (a, b) = if eq(a,b) then a else Binop(Mini,a,b)

  fun mind (IL.D a, IL.D b) = D(if a < b then a else b)
    | mind (a, b) = if eq(a,b) then a else Binop(Mind,a,b)

  fun maxi (IL.I a, IL.I b) = I(if a > b then a else b)
    | maxi (x as IL.If(a,IL.I b,IL.I c), y as IL.I d) = 
      if Int32.>=(b,d) andalso Int32.>=(c,d) then x else
      if Int32.>=(d,b) andalso Int32.>=(d,c) then y else Binop(Maxi,x,y)
    | maxi (a, b) = if eq(a,b) then a else Binop(Maxi,a,b)

  fun maxd (IL.D a, IL.D b) = D(if a > b then a else b)
    | maxd (a, b) = if eq(a,b) then a else Binop(Maxd,a,b)

  fun sqrt a = Unop(Sqrt,a)

  fun powd (a,b) =
      let fun default() = Binop(Powd,a,b)
      in case b of
             IL.D x => if Real.==(x,0.5) then sqrt a
                       else default()
           | _ => 
             if optlevel() > 0 then 
               case (a,b) of
                   (IL.D a,IL.D b) => IL.D(Math.pow(a,b))
                 | _ => default()
             else default()
      end

  fun ceil a = Unop(Ceil,a)
  fun floor a = Unop(Floor,a)
  fun ln a = Unop(Ln,a)
  fun sin a = Unop(Sin,a)
  fun cos a = Unop(Cos,a)
  fun tan a = Unop(Tan,a)
  fun expd a = Unop(Expd,a)
  fun roll a = Unop(Roll,a)

  fun andb (a,b) =
      case (a,b) of
          (IL.T,b) => b
        | (a,IL.T) => a
        | (IL.F,_) => IL.F
        | (_,IL.F) => IL.F
        | _ => Binop(Andb,a,b)
  fun orb (a,b) =
      case (a,b) of
          (IL.T,_) => IL.T
        | (_,IL.T) => IL.T
        | (a,IL.F) => a
        | (IL.F,b) => b
        | _ => Binop(Orb,a,b)

  fun xorb (a,b) = Binop(Xorb,a,b)
  fun resi (a,b) = Binop(Resi,a,b)
  fun ori (a,b) = Binop(Ori,a,b)
  fun andi (a,b) = Binop(Andi,a,b)
  fun xori (a,b) = Binop(Xori,a,b)
  fun shli (a,b) = Binop(Shli,a,b)
  fun shri (a,b) = Binop(Shri,a,b)
  fun shari (a,b) = Binop(Shari,a,b)

  fun (IL.I a) < (IL.I b) = B(Int32.<(a,b))
    | (IL.D a) < (IL.D b) = B(Real.<(a,b))
    | (IL.C a) < (IL.C b) = B(Word.<(a,b))
    | (IL.Binop(Sub,IL.I a,b)) < (IL.I c) = I(Int32.-(a,c)) < b
    | (IL.Binop(Sub,a,IL.I b)) < (IL.I c) = a < I(Int32.+(b,c))
    | (IL.Binop(Add,IL.I a,b)) < (IL.I c) = b < I(Int32.-(c,a))
    | (IL.Binop(Add,a,IL.I b)) < (IL.I c) = a < I(Int32.-(c,b))
    | a < b = Binop(Lt,a,b)

  fun (IL.I a) <= (IL.I b) = B(Int32.<=(a,b))
    | (IL.D a) <= (IL.D b) = B(Real.<=(a,b))
    | (IL.C a) <= (IL.C b) = B(Word.<=(a,b))
    | (IL.Binop(Addi,IL.I 1,e)) <= e' = e < e'
    | a <= (IL.I b) =  a < (I(Int.+(b,1)))
    | a <= b = Binop(Lteq,a,b)

  fun notb IL.T = B false
    | notb IL.F = B true
    | notb (Unop(Not,a)) = a
    | notb a = Unop(Not,a)

  infix ==
  fun (IL.I a) == (IL.I b) = B(a=b)
    | (IL.D a) == (IL.D b) = B(Real.==(a,b))
    | (IL.C a) == (IL.C b) = B(a=b)
    | IL.T == b = b
    | a == IL.T = a
    | (Binop(Mul,IL.I 2,_)) == (IL.I 1) = IL.F
    | (IL.Binop(Add,IL.I a,b)) == (IL.I c) = b == I(Int32.-(c,a))
    | (IL.Binop(Add,a,IL.I b)) == (IL.I c) = a == I(Int32.-(c,b))
    | a == b = 
      if eq(a,b) then IL.T 
      else
        case a of
          IL.If(e,x,y) =>
          (if nevereq(b,x) andalso nevereq(b,y) then IL.F
           else if eq(x,b) andalso nevereq(x,y) then e
           else case b of
                  IL.If(e',x',y') => if eq(x,x') andalso eq(y,y') andalso nevereq(x,y) then e == e'
                                     else Binop(Eq,a,b)
                | _ => Binop(Eq,a,b))
        | _ => case b of 
                 IL.If _ => b == a
               | _ => Binop(Eq,a,b)

  fun a <> b = notb (a == b)
  fun a > b = notb (a <= b)
  fun a >= b = notb (a < b)

  fun ~ e =
      case e of
        IL.I c => I (Int32.~c)
      | IL.D c => D (Real.~c)
      | _ => Unop(Neg,e)

  fun strlen x = Unop(Strlen,x)

  val addi = op +
  val subi = op -
  val muli = op *
  val divi = op /
  val modi = op %
  val eqi = op ==
  val neqi = op <>
  val lti = op <
  val ltei = op <=
  val gti = op >
  val gtei = op >=
  val negi = ~
  fun nowi x = Unop(Now,x)
  val addd = op +
  val subd = op -
  val muld = op *
  val divd = op /
  val modd = op %
  val eqd = op ==
  val neqd = op <>
  val ltd = op <
  val lted = op <=
  val gtd = op >
  val gted = op >=
  val negd = ~

  val eqc = op ==

  val eqb = op ==
  val neqb = op <>

  fun i2d e =
      case e of
        IL.I c => D (real c)
      | _ => Unop(I2D,e)
  fun d2i e = Unop(D2I,e)
  fun b2i IL.T = I 1
    | b2i IL.F = I 0
    | b2i e = Unop(B2I,e)
end
fun Subs(n,e) = IL.Subs(n,e)
val Alloc = IL.Alloc
val Vect = IL.Vect

val T = IL.T
val F = IL.F

type s = IL.Stmt
type ss = s list
type kd = string * Name.t list * ss
type p = kd list * ss

val emp = IL.Nop
val Decl = IL.Decl
val Ret = IL.Ret
val Halt = IL.Halt
val Free = IL.Free
val Printf = IL.Printf
val Sprintf = IL.Sprintf
val ReadIntVecFile = IL.ReadIntVecFile
val ReadDoubleVecFile = IL.ReadDoubleVecFile
val Comment = IL.Comment
fun isEmp ss = List.all (fn IL.Nop => true | _ => false) ss
fun size ss =
    case ss of
      nil => 0
    | IL.Nop :: ss => size ss
    | s :: ss => Int32.+(1, size ss)

fun unDecl (IL.Decl x) = SOME x
  | unDecl _ = NONE

val inlinethreshold = 3

fun Ifs(e,ss1,ss2) ss =
    case e of
      IL.T => ss1 @ ss
    | IL.F => ss2 @ ss
    | _ =>
      case (ss1, ss2) of
        (nil, nil) => ss
      | _ => 
        if IL.eq_ss(ss1, ss2) then ss1 @ ss
        else IL.Ifs(e,ss1,ss2) :: ss

fun ForOptimize optimize (e,n,body) ss =
    case e of
      IL.I 0 => optimize ss
    | IL.I 1 => optimize (IL.Decl(n,SOME(IL.I 0)) :: body @ ss)
    | _ => 
      let fun default() = IL.For (e,n,body) :: optimize ss
      in if isEmp body then optimize ss
         else (*
           case e of
             IL.I n =>
             if Int32.<(Int32.*(size body, n), inlinethreshold) then
               let fun iter x f a =
                       if Int32.<(x,0) then a
                       else iter (Int32.-(x,1)) f (f(x,a))
                   val ss = iter (Int32.-(n,1)) (fn (i,a) => f(I i) @ a) ss
               in optimize ss
               end
             else default()
           | _ =>*) default()
      end

val For = ForOptimize (fn x => x)

local open IL infix := ::= 
in
  fun n := e = 
     if IL.eq(e, Var n) then emp 
     else Assign(n,e)
  fun (n,i) ::= e = AssignArr(n, i, e)      
end

fun defs ss : N.set = 
    case ss of
      nil => N.empty
    | s::ss =>
      case s of
        IL.Nop => defs ss
      | IL.Ret e => N.empty
      | IL.Halt _ => N.empty
      | IL.Free n => defs ss
      | IL.Decl(n,e) => N.remove (defs ss,n)
      | IL.Assign(n,e) => N.insert (defs ss,n)
      | IL.AssignArr(n,e0,e) => defs ss
      | IL.For(e,n,body) =>
        let val ns = N.remove (defs body,n)
        in N.union (ns,defs ss)
        end
      | IL.Ifs(e,ss1,ss2) => N.union(N.union(defs ss1,defs ss2),defs ss)
      | IL.Printf _ => defs ss
      | IL.Sprintf (n,_,_) => N.insert(defs ss,n)
      | IL.ReadIntVecFile (n1,n2,_) => N.union(defs ss,N.fromList[n1,n2])
      | IL.ReadDoubleVecFile (n1,n2,_) => N.union(defs ss,N.fromList[n1,n2])
      | IL.Comment _ => defs ss

fun rm_declsU U ss =
    let fun rm nil = (nil,U)
          | rm (s::ss) =
            let val (ss,U) = rm ss
                fun uds_s s = N.union(uses_s s,defs_s s)
            in case s of 
                 IL.Decl (n,_) =>
                 if N.member(U,n) then
                   (s::ss,N.union(U,uds_s s))
                 else (ss,U)
               | _ => (s::ss,N.union(U,uds_s s))
            end
    in #1(rm ss)
    end

fun rm_decls e ss =
    let val U = uses e N.empty
    in rm_declsU U ss
    end

fun rm_decls0 ss =
    rm_declsU N.empty ss

infix ::=
datatype info = EqI of e
              | LtI of e
              | GtEqI of e 

fun names_info (EqI e) = uses e N.empty
  | names_info (LtI e) = uses e N.empty
  | names_info (GtEqI e) = uses e N.empty

type env = (Name.t * info) list
val env_empty : env = nil
fun names_env E = 
  foldl (fn ((n,i),a) => N.union(N.singleton n, N.union(a,names_info i))) N.empty E
fun assert_name (E, n) =
    if N.member(names_env E,n) then raise Fail ("assert_name:" ^ Name.pr n)
    else ()
fun dom (E:env) = N.fromList(map #1 E)
fun env_cut E names =
    List.filter (fn (n,i) => Bool.not(N.member (names,n)) andalso 
                             N.isEmpty(N.intersect(names,names_info i))) E
fun env_lookeq E n =
    case E of nil => NONE
            | (n', EqI e)::E => if n = n' then SOME e
                                else env_lookeq E n
            | _ :: E => env_lookeq E n

fun lt E e1 e2 =
    let fun look E n i =
            case E of
              (n', LtI(IL.I i'))::E => if n=n' andalso Int32.<=(i',i) then IL.T
                                       else look E n i
            | (n', GtEqI(IL.I i'))::E => if n=n' andalso Int32.>=(i',i) then IL.F
                                         else look E n i
            | x::E => look E n i
            | nil => e1 < e2
    in
      case (e1,e2) of
        (IL.Var n, IL.I i) => look E n i
      | _ => e1 < e2
    end

fun modu E e1 e2 =
    let fun default() = e1 % e2
        fun look E n i =
            case E of
              (n', LtI(IL.I i'))::E => if n=n' andalso Int32.>=(i,i') then IL.Var n
                                       else look E n i
            | x::E => look E n i
            | nil => default()
    in
      case (e1,e2) of
        (IL.Var n, IL.I i) => look E n i
      | _ => default()
    end

(* Static evaluation *)
fun se_e (E:env) (e:e) : e =
    case e of
      IL.Var n => (case env_lookeq E n of SOME e => e (*if simpleExp e then e else Var n *)
                                        | NONE => Var n)
    | IL.I i => I i
    | IL.D d => D d
    | IL.C c => C c
    | IL.T => B true
    | IL.F => B false
    | IL.If(e0,e1,e2) => If(se_e E e0, se_e E e1, se_e E e2)
    | IL.Subs(n,e) =>
      let val e = se_e E e
      in case (env_lookeq E n, e) of 
             (SOME (IL.Vect(_,es)), IL.I i) => 
             (List.nth (es,i) handle _ => raise Fail "se_e: Subs")
           | _ => Subs(n,e)
      end
    | IL.Alloc(t,e) => Alloc(t,se_e E e)
    | IL.Vect(t,es) => Vect(t,List.map (se_e E) es)
    | IL.Binop(IL.Add,e1,e2) => (se_e E e1) + (se_e E e2)
    | IL.Binop(IL.Sub,e1,e2) => (se_e E e1) - (se_e E e2)
    | IL.Binop(IL.Mul,e1,e2) => (se_e E e1) * (se_e E e2)
    | IL.Binop(IL.Divv,e1,e2) => (se_e E e1) / (se_e E e2)
    | IL.Binop(IL.Modv,e1,e2) => modu E (se_e E e1) (se_e E e2)
    | IL.Binop(IL.Mini,e1,e2) => mini (se_e E e1, se_e E e2)
    | IL.Binop(IL.Maxi,e1,e2) => maxi (se_e E e1, se_e E e2)
    | IL.Binop(IL.Mind,e1,e2) => mind (se_e E e1, se_e E e2)
    | IL.Binop(IL.Maxd,e1,e2) => maxd (se_e E e1, se_e E e2)
    | IL.Binop(IL.Powd,e1,e2) => powd (se_e E e1, se_e E e2)
    | IL.Binop(IL.Lt,e1,e2) => lt E (se_e E e1) (se_e E e2)
    | IL.Binop(IL.Lteq,e1,e2) => (se_e E e1) <= (se_e E e2)
    | IL.Binop(IL.Eq,e1,e2) => (se_e E e1) == (se_e E e2)
    | IL.Binop(IL.Resi,e1,e2) => resi(se_e E e1, se_e E e2)
    | IL.Binop(IL.Ori,e1,e2) => ori(se_e E e1, se_e E e2)
    | IL.Binop(IL.Andi,e1,e2) => andi(se_e E e1, se_e E e2)
    | IL.Binop(IL.Xori,e1,e2) => xori(se_e E e1, se_e E e2)
    | IL.Binop(IL.Shli,e1,e2) => shli(se_e E e1, se_e E e2)
    | IL.Binop(IL.Shri,e1,e2) => shri(se_e E e1, se_e E e2)
    | IL.Binop(IL.Shari,e1,e2) => shari(se_e E e1, se_e E e2)
    | IL.Binop(IL.Andb,e1,e2) => andb(se_e E e1, se_e E e2)
    | IL.Binop(IL.Orb,e1,e2) => orb(se_e E e1, se_e E e2)
    | IL.Binop(IL.Xorb,e1,e2) => xorb(se_e E e1, se_e E e2)
    | IL.Unop(IL.Neg,e1) => ~(se_e E e1)
    | IL.Unop(IL.I2D,e1) => i2d (se_e E e1)
    | IL.Unop(IL.D2I,e1) => d2i (se_e E e1)
    | IL.Unop(IL.Ceil,e1) => ceil (se_e E e1)
    | IL.Unop(IL.Floor,e1) => floor (se_e E e1)
    | IL.Unop(IL.Ln,e1) => ln (se_e E e1)
    | IL.Unop(IL.Sin,e1) => sin (se_e E e1)
    | IL.Unop(IL.Cos,e1) => cos (se_e E e1)
    | IL.Unop(IL.Tan,e1) => tan (se_e E e1)
    | IL.Unop(IL.Expd,e1) => expd (se_e E e1)
    | IL.Unop(IL.Sqrt,e1) => sqrt (se_e E e1)
    | IL.Unop(IL.Roll,e1) => roll (se_e E e1)
    | IL.Unop(IL.B2I,e1) => b2i (se_e E e1)
    | IL.Unop(IL.Not,e1) => notb(se_e E e1)
    | IL.Unop(IL.Now,e1) => nowi(se_e E e1)
    | IL.Unop(IL.Strlen,e1) => strlen(se_e E e1)

fun se_ss (E:env) (ss:ss) : ss =
    case peep ss of
      nil => nil
    | s::ss =>
      case s of
        IL.Nop => se_ss E ss
      | IL.Ret e => Ret(se_e E e)::nil  (* ss2 is dead *)
      | IL.Halt str => Halt str::nil    (* ss2 is dead *)
      | IL.Free n => 
        let val ss = se_ss E ss
        in Free n :: ss
        end
      | IL.Decl(n,SOME e) =>
        let val e = se_e E e
	    (* val () = assert_name(E,n) *)
            val E2 = 
                case (IL.Name.typeOf n, e) of
                    (IL.Vec _, IL.Vect _) => (n,EqI e)::E
                  | (IL.Vec _, _) => E
                  | (IL.Int, IL.Binop(IL.Modv, _, e)) => (n, LtI e)::E
                  | _ => if simpleExp e then (n,EqI e)::E
                         else E
            val ss = se_ss E2 ss
        in Decl(n,SOME e) :: ss
        end
      | IL.Decl(n,NONE) =>
        let fun default() = Decl(n,NONE) :: se_ss E ss
        in case ss of
               IL.Assign(n',e) :: ss3 =>
               if n = n' then se_ss E (IL.Decl(n,SOME e) :: ss3)
               else default()
             | _ => default()
        end
      | IL.Assign(n,e) =>
        let val e = se_e E e
	    val E' = env_cut E (N.singleton n)
	    (* val () = assert_name(E',n) *)
            val E2 = (*(n,EqI e)::*)E'   (* unsafe to add to environment *)
            val ss = se_ss E2 ss
        in (n := e) :: ss
        end
      | IL.AssignArr(n,e0,e) =>
        let val e0 = se_e E e0
            val e = se_e E e                    
            val ss = se_ss E ss
        in ((n,e0) ::= e) :: ss
        end
      | IL.For(e,n,body) =>
        let val defs_body = defs body
            val E' = env_cut E defs_body
            val e' = se_e E' e
            val E2 = (n,GtEqI(I 0))::E'
	    val	E2 = (n,LtI e')::E2
            val ss1 = se_ss E2 body
            val ss1 = rm_decls0 ss1
        in ForOptimize (se_ss E') (e', n, ss1) ss
        end
      | IL.Ifs(e,[],[IL.Halt str]) =>
        let val e = se_e E e
            val ss = case e of
                         IL.Binop(IL.Lt,IL.Var n,x) =>
                         let val E2 = (n,LtI x)::E
                         in se_ss E2 ss
                         end
                       | _ => se_ss E ss
        in Ifs(e,[],[Halt str]) ss
        end
        | IL.Ifs(e,ss0,ss1) =>
        let val e = se_e E e
            val ss0 = se_ss E ss0
            val ss1 = se_ss E ss1
            val defs_branches = N.union(defs_ss ss0,defs_ss ss1)
            val ss0 = rm_decls0 ss0
            val ss1 = rm_decls0 ss1
            val E' = env_cut E defs_branches
            val ss = se_ss E' ss
        in Ifs(e,ss0,ss1) ss
        end
      | IL.Printf(s,es) => Printf(s, List.map(se_e E)es) :: se_ss E ss
      | IL.Sprintf(n,s,es) => Sprintf(n, s, List.map(se_e E)es) :: se_ss E ss
      | IL.ReadIntVecFile(n1,n2,e) => ReadIntVecFile(n1, n2, se_e E e) :: se_ss E ss
      | IL.ReadDoubleVecFile(n1,n2,e) => ReadDoubleVecFile(n1, n2, se_e E e) :: se_ss E ss
      | IL.Comment s => Comment s :: se_ss E ss

(* Peep hole optimization *)
and peep ss =
    case ss of
      (s1 as IL.Decl(n,NONE)) :: (s2 as IL.Assign(n',e')) :: ss' =>
      ((* Util.prln ("peep candidate: " ^ Name.pr n ^ ", " ^ Name.pr n'); *)
       if n = n' then 
         ((*Util.prln ("peep: " ^ Name.pr n);*)
          peep(IL.Decl(n,SOME e') :: ss'))
       else peep(s2::s1::ss'))
    | IL.Decl(n,SOME e) :: IL.Assign(n',e') :: ss' =>
       if n = n' then 
         peep(IL.Decl(n,SOME(se_e [(n,EqI e)] e')) :: ss')
       else ss
    | IL.Assign(n,e) :: IL.Assign(n',e') :: ss' =>
       if n = n' then 
         peep((n := (se_e [(n,EqI e)] e')) :: ss')
       else ss
    | s :: IL.Nop :: ss => peep (s::ss)
    | IL.Nop :: ss => peep ss
    | (s1 as IL.Decl(n,NONE)) :: (s2 as IL.Decl(_,SOME _)) :: ss' => 
      peep(s2::s1::ss')
    | (s1 as IL.Decl(n,NONE)) :: (s2 as IL.Comment _) :: ss' => 
      peep(s2::s1::ss')
    | _ => ss

end
