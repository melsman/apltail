structure TailType : TAIL_TYPE = struct
type var = string
type opr = string
open URef
type bv = string
type tv = string
type rv = string

datatype r = R of int
           | Rv of rv * (int->string option)
withtype rnk = r uref
datatype b = IntT
           | DoubleT
           | BoolT
           | Bv of bv 
withtype bty = b uref
datatype t = ShT of rnk
           | SiT of rnk
           | ViT of rnk
           | ArrT of bty * rnk
           | FunT of typ * typ 
           | TyvT of tv
withtype typ = t uref

local
  fun newcount s =
      let val c = ref 0
      in fn () => s ^ Int.toString(!c before c:= !c + 1)
      end
in fun RnkVarCon f : rnk = uref(Rv(newcount "'r" (),f))
   fun RnkVar ()   : rnk = RnkVarCon (fn _ => NONE)
   fun TyVarB ()   : bty = uref(Bv(newcount "'b" ()))
   fun TyVar ()    : typ = uref(TyvT(newcount "'a" ()))
end

fun rnk n = uref (R n)
val rnk0 = rnk 0
val rnk1 = rnk 1
fun unRnk r = case !!r of R i => SOME i | _ => NONE

local fun Scl b = uref (ArrT(b,rnk0))
in val IntB    = uref IntT
   val DoubleB = uref DoubleT
   val BoolB   = uref BoolT
   val Int     = Scl IntB
   val Double  = Scl DoubleB
   val Bool    = Scl BoolB
end
fun Sh r = uref(ShT r)
fun Si r = uref(SiT r)
fun Vi r = uref(ViT r)
fun Arr bt r = uref(ArrT(bt,r))
fun Fun (t1,t2) = uref(FunT(t1,t2))

fun prR r = case r of R i => Int.toString i | Rv (rv,_) => rv
and prRnk r = prR (!!r)
and prB b =
    case b of
        IntT => "int"
      | DoubleT => "double"
      | BoolT => "bool"
      | Bv bv => bv
and prBty bty = prB(!!bty)
and prT t =
    case t of
        ShT r => "Sh(" ^ prRnk r ^ ")"
      | SiT r => "Si(" ^ prRnk r ^ ")"
      | ViT r => "Vi(" ^ prRnk r ^ ")"
      | ArrT (bt,r) => "[" ^ prBty bt ^ "]" ^ prRnk r
      | FunT (t1,t2) => "(" ^ prTy t1 ^ ")->" ^ prTy t2
      | TyvT tv => tv
and prTy t = prT(!!t)
val prType = prTy

fun unArr t = case !!t of ArrT p => SOME p | _ => NONE
fun unSh t = case !!t of ShT r => SOME r | _ => NONE
fun unSi t = case !!t of SiT r => SOME r | _ => NONE
fun unVi t = case !!t of ViT r => SOME r | _ => NONE
fun unFun t = case !!t of FunT p => SOME p | _ => NONE

fun comb f1 f2 t = case f1 t of NONE => f2 t | x => x
fun check f t = case f t of SOME s => raise Fail s | NONE => ()
fun isInt bt = case !!bt of IntT => true | _ => false
fun isDouble bt = case !!bt of DoubleT => true | _ => false
fun isBool bt = case !!bt of BoolT => true | _ => false

fun combB (b1,b2) =
    case (b1,b2) of
        (Bv _, _) => b2
      | (_, Bv _) => b1
      | (IntT, IntT) => b1
      | (DoubleT, DoubleT) => b1
      | (BoolT, BoolT) => b1
      | _ => raise Fail ("cannot unify " ^ prB b1 ^ " and " ^ prB b2)
and unifB b1 b2 = URef.unify combB (b1,b2)
and combT (t1,t2) =
    case (t1,t2) of
        (TyvT _, _) => t2
      | (_, TyvT _) => t1
      | (t as ArrT (b1,r1), ArrT (b2,r2)) => (unifB b1 b2; unifR r1 r2; t) 
      | (t as FunT (t1,t2), FunT (t1',t2')) => (unif t1 t1'; unif t2 t2'; t)
      | (ShT r1, ShT r2) => (unifR r1 r2; t1)
      | (SiT r1, SiT r2) => (unifR r1 r2; t1)
      | (ViT r1, ViT r2) => (unifR r1 r2; t1)
      | _ => raise Fail ("cannot unify " ^ prT t1 ^ " and " ^ prT t2)
and unif t1 t2 = URef.unify combT (t1,t2)
and combR (r1,r2) =
    case (r1,r2) of
        (R i1, R i2) => if i1 = i2 then r1
                        else raise Fail ("cannot unify rank " ^ prR r1 ^ " and rank " ^ prR r2)
      | (Rv(rv1,f1), Rv(_,f2)) => Rv(rv1,comb f1 f2)
      | (Rv(_,f), R i) => (check f i; r2)
      | (R i, Rv(_,f)) => (check f i; r1)
and unifR r1 r2 = URef.unify combR (r1,r2)

fun relateR _ = raise Fail "relateR not implemented"
fun relateR2 _ = raise Fail "relateR2 not implemented"

fun Vec t =
    let val bt = TyVarB()
        val r = rnk0
    in unif t (Arr bt r);
       Arr bt rnk1
    end

fun wrap f x y = (f x y; NONE) handle Fail s => SOME s
val unify = wrap unif
val unifyR = wrap unifR
val unifyB = wrap unifB

fun subtype t1 t2 =
    case unSi t1 of
        SOME r1 => (case unSi t2 of
                        SOME r2 => unifyR r1 r2
                      | NONE => unify t2 Int)
      | NONE => 
        case unVi t1 of
            SOME r1 => (case unVi t2 of
                            SOME r2 => unifyR r1 r2
                          | NONE => case unSh t2 of
                                        SOME r => unifyR rnk1 r
                                      | NONE => unify t2 (Vec Int))
          | NONE =>
            case unSh t1 of
                SOME r1 => (case unSh t2 of
                                SOME r2 => unifyR r1 r2
                              | NONE => unify t2 (Vec Int))
              | NONE => unify t1 t2
end            
