structure TailType : TAIL_TYPE = struct
type var = string
type opr = string
open URef
type bv = string
type tv = string
type rv = string

datatype unify_result  = SUCCESS | ERROR of string

datatype r = R of int
           | Rv of rv * (int -> unify_result)
withtype rnk = r uref
datatype b = IntT
           | DoubleT
           | BoolT
           | CharT
           | Bv of bv 
withtype bty = b uref
datatype t = ArrT of bty * rnk
           | VccT of bty * rnk
           | ST   of bty * rnk
           | SVT  of bty * rnk
           | FunT of typ * typ 
           | TupT of typ list
           | TyvT of tv
withtype typ = t uref

local
  fun newcount s =
      let val c = ref 0
      in fn () => s ^ Int.toString(!c before c:= !c + 1)
      end
in fun RnkVarCon f : rnk = uref(Rv(newcount "'r" (),f))
   fun RnkVar ()   : rnk = RnkVarCon (fn _ => SUCCESS)
   fun TyVarB ()   : bty = uref(Bv(newcount "'b" ()))
   fun TyVar ()    : typ = uref(TyvT(newcount "'a" ()))
end

val rnk0 = uref (R 0)
val rnk1 = uref (R 1)
fun rnk 0 = rnk0
  | rnk 1 = rnk1
  | rnk n = uref (R n)
fun unRnk r = case !!r of R i => SOME i | _ => NONE

val IntB    = uref IntT
val DoubleB = uref DoubleT
val BoolB   = uref BoolT
val CharB   = uref CharT

fun Arr bt r = uref(ArrT(bt,r))
fun Vcc bt r = uref(VccT(bt,r))
fun S   bt r = uref(ST(bt,r))
fun SV  bt r = uref(SVT(bt,r))
fun Fun (t1,t2) = uref(FunT(t1,t2))
fun Tup ts = uref(TupT ts)

fun Scl bt  = Arr bt rnk0
fun VecB bt = Arr bt rnk1
val Int     = Scl IntB
val Double  = Scl DoubleB
val Bool    = Scl BoolB
val Char    = Scl CharB

val Sh = Vcc IntB
val Si = S IntB
val Vi = SV IntB

fun prR r = case r of R i => Int.toString i | Rv (rv,_) => rv
and prRnk r = prR (!!r)
and prB b =
    case b of
        IntT => "int"
      | DoubleT => "double"
      | BoolT => "bool"
      | CharT => "char"
      | Bv bv => bv
and prBty bty = prB(!!bty)
and prT t =
    case t of
        ArrT (bt,r) => "[" ^ prBty bt ^ "]" ^ prRnk r
      | VccT (bt, r) => "<" ^ prBty bt ^ ">" ^ prRnk r
      | ST (bt,r) => "S(" ^ prBty bt ^ "," ^ prRnk r ^ ")"
      | SVT (bt,r) => "SV(" ^ prBty bt ^ "," ^ prRnk r ^ ")"
      | FunT (t1,t2) => "(" ^ prType t1 ^ ")->" ^ prType t2
      | TupT ts => "(" ^ String.concatWith " * " (List.map prType ts) ^ ")"
      | TyvT tv => tv
and prType t = prT(!!t)

fun unArr t = case !!t of ArrT p => SOME p | _ => NONE
fun unVcc t = case !!t of VccT p => SOME p | _ => NONE
fun unS   t = case !!t of ST p   => SOME p | _ => NONE
fun unSV  t = case !!t of SVT p  => SOME p | _ => NONE
fun unFun t = case !!t of FunT p => SOME p | _ => NONE
fun unTup t = case !!t of TupT p => SOME p | _ => NONE

fun comb f1 f2 t = case f1 t of SUCCESS => f2 t | x => x
fun check f t = case f t of ERROR s => raise Fail s | SUCCESS => ()

fun isInt    bt = case !!bt of IntT    => true | _ => false
fun isDouble bt = case !!bt of DoubleT => true | _ => false
fun isBool   bt = case !!bt of BoolT   => true | _ => false
fun isChar   bt = case !!bt of CharT   => true | _ => false

fun Vec t =
    case unArr t of
        SOME (bt, r) =>
        (case unRnk r of
             SOME 0 => VecB bt
           | _ => raise Fail "Vec assumes a known scalar type argument")
      | NONE => case unS t of
                    SOME (bt,_) => VecB bt
                  | NONE => raise Fail "Vec assumes a known scalar type argument"

fun combB (b1,b2) =
    case (b1,b2) of
        (Bv _, _) => b2
      | (_, Bv _) => b1
      | (IntT, IntT) => b1
      | (DoubleT, DoubleT) => b1
      | (BoolT, BoolT) => b1
      | (CharT, CharT) => b1
      | _ => raise Fail ("cannot unify " ^ prB b1 ^ " and " ^ prB b2)
and unifB b1 b2 = URef.unify combB (b1,b2)
and combT (t1,t2) =
    case (t1,t2) of
        (TyvT _, _) => t2
      | (_, TyvT _) => t1
      | (t as ArrT (b1,r1), ArrT (b2,r2)) => (unifB b1 b2; unifR r1 r2; t) 
      | (t as VccT (b1,r1), VccT (b2,r2)) => (unifB b1 b2; unifR r1 r2; t) 
      | (t as ST   (b1,r1), ST   (b2,r2)) => (unifB b1 b2; unifR r1 r2; t) 
      | (t as SVT  (b1,r1), SVT  (b2,r2)) => (unifB b1 b2; unifR r1 r2; t) 
      | (t as FunT (t1,t2), FunT (t1',t2')) => (unif t1 t1'; unif t2 t2'; t)
      | (t as TupT ts, t' as TupT ts') => (unifs t t' ts ts'; t)
      | _ => raise Fail ("cannot unify " ^ prT t1 ^ " and " ^ prT t2)
and unif t1 t2 = URef.unify combT (t1,t2)
and unifs _ _ nil nil = ()
  | unifs t0 t0' (t::ts) (t'::ts') = (unif t t'; unifs t0 t0' ts ts')
  | unifs t0 t0' _ _ = raise Fail ("cannot unify tuple types " ^ prT t0 ^ " and " ^ prT t0')
and combR (r1,r2) =
    case (r1,r2) of
        (R i1, R i2) => if i1 = i2 then r1
                        else raise Fail ("cannot unify rank " ^ prR r1 ^ " and rank " ^ prR r2)
      | (Rv(rv1,f1), Rv(_,f2)) => Rv(rv1,comb f1 f2)
      | (Rv(_,f), R i) => (check f i; r2)
      | (R i, Rv(_,f)) => (check f i; r1)
and unifR r1 r2 = URef.unify combR (r1,r2)

(* fun relateR _ = raise Fail "relateR not implemented" *)
(* fun relateR2 _ = raise Fail "relateR2 not implemented" *)

(* The applications of unify below should be replaced with conditional unifications *)

fun unif_btr (bt1,r1) (bt2,r2) =
    (unifB bt1 bt2; unifR r1 r2)

fun subtype t1 t2 =
    case unS t1 of
        SOME btr1 => (case unS t2 of
                          SOME btr2 => unif_btr btr1 btr2
                        | NONE => unif t2 (Scl (#1 btr1))
                     )
      | NONE => 
        case unSV t1 of
            SOME btr1 => (case unSV t2 of
                              SOME btr2 => unif_btr btr1 btr2
                            | NONE => case unVcc t2 of
                                          SOME (bt2,r2) => (unifB (#1 btr1) bt2; 
                                                            unifR rnk1 r2)
                                        | NONE => unif t2 (VecB (#1 btr1))
                         )
          | NONE =>
            case unVcc t1 of
                SOME btr1 => (case unVcc t2 of
                                  SOME btr2 => unif_btr btr1 btr2
                                | NONE => unif t2 (VecB (#1 btr1))
                             )
              | NONE => unif t1 t2

fun join t1 t2 =
    case (unS t1, unS t2) of
        (SOME (bt1,r1), SOME (bt2,r2)) => 
        (unifB bt1 bt2;
         case (unRnk r1, unRnk r2) of
             (SOME i1, SOME i2) => if i1 = i2 then t1
                                   else Scl bt1
           | _ => (unifR r1 r2; t1))
      | (SOME(bt1,_), NONE) => join (Scl bt1) t2
      | (NONE, SOME(bt2,_)) => join t1 (Scl bt2)
      | _ => 
        case (unSV t1, unSV t2) of
            (SOME (bt1,r1), SOME (bt2,r2)) =>
            (unifB bt1 bt2;
             case (unRnk r1, unRnk r2) of
                 (SOME i1, SOME i2) => if i1 = i2 then t1
                                       else Vcc bt1 (rnk 1)
               | _ => (unifR r1 r2; t1))
          | (SOME(bt1,_), NONE) => join (Vcc bt1 (rnk 1)) t2
          | (NONE, SOME(bt2,_)) => join t1 (Vcc bt2 (rnk 1))
          | _ => 
            case (unVcc t1, unVcc t2) of
                (SOME (bt1,r1), SOME (bt2,r2)) =>
                (unifB bt1 bt2;
                 case (unRnk r1, unRnk r2) of
                     (SOME i1, SOME i2) => if i1 = i2 then t1
                                           else VecB bt1
                   | _ => (unifR r1 r2; t1))
              | (SOME(bt1,_), NONE) => join (VecB bt1) t2
              | (NONE, SOME(bt2,_)) => join t1 (VecB bt2)
              | _ => (unif t1 t2; t1)


fun wrap f x y = (f x y; SUCCESS) handle Fail s => ERROR s
val unify = wrap unif
val subtype = wrap subtype
val unifyR = wrap unifR
val unifyB = wrap unifB

fun unArr' at =       (* return the base type and the rank of an array *)
    case unArr at of
        SOME p => SOME p
      | NONE => case unVcc at of
                    SOME (bt,_) => SOME (bt, rnk 1)
                  | NONE => case unSV at of
                                SOME (bt,_) => SOME (bt, rnk 1)
                              | NONE => case unS at of
                                            SOME (bt,_) => SOME (bt, rnk 0)
                                          | NONE => NONE
end            
