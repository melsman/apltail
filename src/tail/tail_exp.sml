functor TailExp(T : TAIL_TYPE) : TAIL_EXP = struct
  structure T = T
  open T
  type opr = string

  type var = {id : string,
              mutable : bool ref}
  local val idCounter = ref 0
  in fun newVar() = 
         { id = "v" ^ Int.toString (!idCounter),
           mutable = ref false}
         before idCounter := !idCounter + 1
     fun mutableVar (v:var) = #mutable v
     fun ppVar (v:var) = if !(#mutable v) then "m" ^ #id v
                         else #id v
  end

  structure FM = OrderFinMap(struct type t = var 
                                    fun compare (v1:t,v2:t) = String.compare(#id v1,#id v2)
                             end)

  (* Some type utilities *)
  fun unScl s t =
      case unArr t of
          SOME (bt,r) => (case unifyR r (rnk 0) of
                              ERROR s' => raise Fail (s' ^ " in " ^ s)
                            | SUCCESS => bt)
        | NONE =>
          case unS t of
              SOME (bt,_) => bt
            | NONE => 
              let val bt = TyVarB()
              in case unify t (Arr bt (rnk 0)) of
                     ERROR s' => raise Fail (s' ^ " in " ^ s)
                   | SUCCESS => bt
              end

  fun unUnaryFun s ty =
      let fun err t = 
              raise Fail ("expected function type, but got " ^ prType t)
      in case unFun ty of
             SOME (t1,t2) =>
             (unScl "function argument" t1,
              unScl "function result" t2)
           | NONE => err ty
      end

  fun unBinFun s ty =
      let fun err t = 
              raise Fail ("expected function type, but got " ^ prType t)
      in case unFun ty of
             SOME (t1,t) =>
             (case unFun t of
                  SOME(t2,t3) => (unScl "first function argument" t1,
                                  unScl "second function argument" t2,
                                  unScl "function result" t3)
                | NONE => err t)
           | NONE => err ty
      end

  fun unArr' s t =
      case unArr t of
          SOME p => p
        | NONE =>
      case unVcc t of
          SOME (bt,_) => (bt,rnk 1)
        | NONE => 
      case unSV t of
          SOME (bt,_) => (bt,rnk 1)
        | NONE =>
      case unS t of
          SOME (bt,_) => (bt,rnk 0)
        | NONE => 
          let val tv = TyVarB()
              val r = RnkVar()
          in case unify t (Arr tv r) of
                 SUCCESS => (tv,r)
               | ERROR _ => 
                 raise Fail ("expecting array type, but got "
                             ^ prType t ^ " in " ^ s)
          end

  fun unS' s t =
      case unS t of
          SOME (bt,_) => (bt,rnk 0)
        | NONE =>
          let val tv = TyVarB()
              val r = RnkVar()
          in case unify t (S tv r) of
                 SUCCESS => (tv,r)
               | ERROR _ => 
                 raise Fail ("expecting singleton type, but got "
                             ^ prType t ^ " in " ^ s)
          end

  fun unFun' s t =
      case unFun t of
          SOME (t1,t2) => (unScl s t1, unScl s t2)
        | NONE => raise Fail ("expecting function type, but got "
                              ^ prType t ^ " in " ^ s)

  (* Expressions *)

  datatype texp =
           Var of var * typ
         | I of Int32.int
         | D of real
         | B of bool
         | C of word
         | Iff of texp * texp * texp * typ
         | Vc of texp list * typ
         | Op of opr * texp list * typ
         | Let of var * typ * texp * texp * typ
         | Fn of var * typ * texp * typ

  (* Environments *)
  type env = (var * typ) list

  val emptyEnv = nil

  fun lookup e v =
      case e of
          nil => NONE
        | (x,t)::e => if x = v then SOME t
                      else lookup e v

  fun add e v t = (v,t)::e

  (* Typing *)
  fun assert0 unify s t1 t2 =
      case unify t1 t2 of
          ERROR s' => raise Fail (s' ^ " in " ^ s)
        | SUCCESS => ()

  val assert = assert0 unify
  val assertR = assert0 unifyR
  val assertB = assert0 unifyB
  
  val assert_sub = assert0 subtype

  fun isBinOpIII opr =
      Util.listContains opr ["addi","subi","muli","divi","maxi","mini","mod","resi","andi","ori","shli","shri","shari","xori"]

  fun isBinOpDDD opr =
      Util.listContains opr ["addd","subd","muld","divd","resd","maxd","mind","powd"]

  fun isBinOpIIB opr =
      Util.listContains opr ["lti","ltei","eqi","gti","gtei"]

  fun isBinOpDDB opr =
      Util.listContains opr ["ltd","lted","eqd","gtd","gted"]

  fun isBinOpBBB opr =
      Util.listContains opr ["andb","orb","eqb","xorb","norb","nandb"]

  fun isBinOpCCB opr =
      Util.listContains opr ["ltc","ltec","eqc","gtc","gtec"]

  fun isInt' t =
      case unArr t of
          SOME (bt,r) => (case unRnk r of
                              SOME 0 => isInt bt
                            | _ => false)
        | NONE => case unS t of
                      SOME (bt,_) => isInt bt
                    | NONE => false

  fun isBool' t =
      case unArr t of
          SOME (bt,r) => (case unRnk r of
                              SOME 0 => isBool bt
                            | _ => false)
        | NONE => case unS t of
                      SOME (bt,_) => isBool bt
                    | NONE => false

  fun tyVc ts =
      case ts of
          nil => Vcc (TyVarB()) (rnk 0)
        | _ => 
          let val oneInt = List.foldl (fn (e,a) => a orelse isInt' e) false ts
              val oneBool = List.foldl (fn (e,a) => a orelse isBool' e) false ts
              val t1 = hd ts
              val btOpt = if oneInt then 
                            (List.app (fn t => assert_sub "vector" t Int) ts;
                             SOME IntB)
                          else if oneBool then
                            (List.app (fn t => assert_sub "vector" t Bool) ts;
                             SOME BoolB)
                          else 
                            (List.app (fn t => assert "vector" t t1) (tl ts);
                             NONE)
          in case btOpt of
                 SOME bt =>
                 (case ts of
                      [t] => (case unS t of
                                  SOME (bt,r) => SV bt r
                                | NONE => Vcc bt (rnk 1))
                    | _ => Vcc bt (rnk(length ts)))
               | NONE => 
                 let val (bt,r) = unArr' "vector expression" t1
                     val () = assertR "vector expression" r (rnk 0)
                 in Arr bt (rnk 1)                    (* vector type *)
                 end
          end

  fun unVec0 t =
      case unVcc t of
          SOME (bt,r) => SOME(bt,r)
        | NONE => case unSV t of
                      SOME (bt,_) => SOME (bt,rnk 1)
                    | NONE => NONE
                                  
  fun unVec t =
      case unVec0 t of
          NONE => NONE
        | SOME (bt,r) =>
          case unRnk r of
              SOME i => SOME(bt,i)
            | NONE => NONE

  fun unSii t =
      case unS t of
          NONE => NONE
        | SOME (bt,r) => if isInt bt then unRnk r
                         else NONE

  fun conssnoc sh opr t1 t2 =
      let val fstcons = if opr = "cons" then "first" else "second"
          val sndcons = if opr <> "cons" then "first" else "second"
          fun default() =
              if sh then raise Fail (opr ^ "expects argument of shape type")
              else let val (b1,r1) = unArr' (fstcons ^ " argument to " ^ opr) t1
                       val (b2,r2) = unArr' (sndcons ^ " argument to " ^ opr) t2
                       val rv = RnkVarCon (fn i => unifyR r2 (rnk(i+1)))
                   in assertR ("arguments to " ^ opr) rv r1
                    ; assertB ("arguments to " ^ opr) b1 b2
                    ; t2
                   end
      in case unVec t2 of
             SOME (bt2,r2) => (assert_sub opr t1 (Scl bt2); Vcc bt2 (rnk(r2+1)))
           | NONE => case unS t1 of
                         SOME (bt1,_) => (assert_sub opr t2 (VecB bt1); VecB bt1)
                       | NONE => default()
      end

  fun type_first sh t =
      let fun default() = 
              if sh then raise Fail "firstV expects argument of shape type"
              else let val (bt,_) = unArr' "disclose argument" t
                   in Arr bt (rnk 0)
                   end
      in case unSV t of
             SOME (bt,r) => S bt r
           | NONE =>
             case unVcc t of
                 SOME (bt,_) => Scl bt
               | NONE => default()
      end

  fun type_cat sh t1 t2 =
      let fun default() =
              if sh then raise Fail "catV expects arguments of shape types"
              else let val (bt1,r1) = unArr' "first argument to catenate" t1
                       val (bt2,r2) = unArr' "second argument to catenate" t2
                   in assertB "cat" bt1 bt2
                    ; assertR "cat" r1 r2
                    ; Arr bt1 r1
                   end
      in case (unVec t1, unVec t2) of
             (SOME(bt1,i1), SOME(bt2,i2)) => (assertB "cat" bt1 bt2; Vcc bt1 (rnk(i1 + i2)))
           | _ => default()
      end

  fun max (a:int) b = if a > b then a else b
  fun abs (a:int) = if a > 0 then a else ~ a

  fun type_drop sh t1 t2 =
      let fun default () =
              if sh then raise Fail "dropV expects arguments of singleton type and shape type"
              else let val (bt,r) = unArr' "drop" t2;
                   in assert_sub "first argument to drop" t1 Int; 
                      Arr bt r
                   end
      in case (unSii t1, unVec t2) of
             (SOME i1, SOME (bt2,i2)) => Vcc bt2 (rnk(max 0 (i2-abs i1)))
           | _ => default()
      end

  fun type_take sh t1 t2 =
      let fun default () =
              if sh then raise Fail "takeV expects arguments of singleton and shape types"
              else (assert "take" t2 (Arr(TyVarB())(RnkVar()));            
                    t2)
      in case unSii t1 of
             SOME i => (case unArr t2 of
                            SOME (bt, r) => 
                            (case unRnk r of
                                 SOME 1 => Vcc bt (rnk(abs i))
                               | _ => default())
                          | NONE => 
                            case unVcc t2 of
                                SOME (bt,_) => Vcc bt (rnk(abs i))
                              | NONE => case unSV t2 of
                                            SOME(bt,_) => Vcc bt (rnk(abs i))
                                          | NONE => default())
           | NONE => (assert "first argument to take" Int t1;
                      default())
      end

  fun type_each opr tf tv =
      let val (bt1,bt2) = unFun' ("first argument to " ^ opr) tf
          fun default() =
              if opr = "eachV" then
                raise Fail (opr ^ " expects argument as a sized vector type")
              else let val (bt,r) = unArr' opr tv                  
                   in assertB (opr ^ " elements") bt1 bt;
                      Arr bt2 r
                   end
      in case unVec0 tv of
             NONE => default()
           | SOME (bt,r) => (assertB (opr ^ " elements") bt1 bt; Vcc bt2 r)
      end

  fun tyOp opr ts =
      case (opr, ts) of
          ("zilde", nil) => tyVc nil
        | ("pi", nil) => Double
        | ("iota", [t]) =>
          (assert_sub "iota expression" t Int;
           case unS t of
               SOME (_,n) => Vcc IntB n
             | NONE => VecB IntB)
        | ("shape", [t]) =>
          (case unVec0 t of
               SOME (_,n) => SV IntB n
             | NONE => 
               let val (_,r) = unArr' "shape argument" t
               in Vcc IntB r
               end)
        | ("reshape", [t1,t2]) =>
          let val (bt,_) = unArr' "second argument to reshape" t2
              val r = RnkVar()
          in assert_sub "first argument to reshape" t1 (Vcc IntB r);
             Arr bt r
          end
        | ("take",[t1,t2]) => type_take false t1 t2
        | ("takeV",[t1,t2]) => type_take true t1 t2
        | ("drop",[t1,t2]) => type_drop false t1 t2
        | ("dropV",[t1,t2]) => type_drop true t1 t2
        | ("rav",[t]) => 
          let val (bt,_) = unArr' "argument to ravel" t
          in Arr bt (rnk 1)
          end
        | ("cat",[t1,t2]) => type_cat false t1 t2
        | ("catV",[t1,t2]) => type_cat true t1 t2
        | ("cons",[t1,t2]) => conssnoc false opr t1 t2
        | ("consV",[t1,t2]) => conssnoc true opr t1 t2
        | ("snoc",[t1,t2]) => conssnoc false opr t2 t1
        | ("snocV",[t1,t2]) => conssnoc true opr t2 t1
        | ("first",[t]) => type_first false t
        | ("firstV",[t]) => type_first true t
        | ("vreverse",[t]) => (unArr' "vreverse" t; t)
        | ("mem",[t]) => (unArr' "mem" t; t)
        | ("transp",[t]) => (unArr' "transp" t; t)
        | ("transp2",[t1,t2]) =>
          let val (bt,r) = unArr' "transp2" t2
          in assert "first argument to transpose2" (Vcc IntB r) t1;
             t2
          end
        | ("vrotate",[t1,t2]) =>
          (unArr' "vrotate" t2;
           assert_sub "first argument to vrotate" t1 Int;
           t2)
        | ("zipWith",[tf,t1,t2]) =>
          let val (bt1,bt2,bt) = unBinFun "first argument to zipWith" tf
              val (bt1',r1) = unArr' "zipWith first argument" t1
              val (bt2',r2) = unArr' "zipWith second argument" t2
          in assertB "first argument to zipWith" bt1 bt1'
           ; assertB "second argument to zipWith" bt2 bt2'
           ; assertR "zipWith argument ranks" r1 r2
           ; Arr bt r1
          end
        | ("power", [tf,tn,tv]) =>
          let val (t1,t2) = 
                  case unFun tf of
                      SOME(t1,t2) => (t1,t2)
                    | NONE => raise Fail "expecting function type" 
          in assert_sub opr tn Int
           ; assert opr t1 t2
           ; assert_sub opr tv t2
           ; t2
          end
        | ("powerScl", [tf,tn,tv]) =>
          let val (t1,t2) = 
                  case unFun tf of
                      SOME(t1,t2) => (t1,t2)
                    | NONE => raise Fail "expecting function type" 
          in assert_sub opr tn Int
           ; assert opr t1 t2
           ; assert_sub opr tv t2
           ; unScl "powerScl recursive argument" tv
           ; t2
          end
        | ("condScl", [tf,tb,tv]) =>
          let val (t1,t2) = 
                  case unFun tf of
                      SOME(t1,t2) => (t1,t2)
                    | NONE => raise Fail "expecting function type" 
          in assert_sub opr tb Bool
           ; assert opr t1 t2
           ; assert_sub opr tv t2
           ; unScl "condScl recursive argument" tv
           ; t2
          end
        | ("reduce", [tf,tn,tv]) =>
          let val (bt1,bt2,bt) = unBinFun "first argument to reduce" tf
              val btn = unScl "reduce neutral element" tn
              val (btv,r) = unArr' "reduce argument" tv
              val () = List.app (assertB "reduce function" btn) [bt1,bt2,bt,btv]
              val rv = RnkVarCon (fn i => unifyR r (rnk(i+1)))
              val rv' = RnkVarCon (fn i => unifyR rv (rnk(i-1)))
          in assertR "reduce" rv' r
           ; Arr bt rv
          end
        | ("scan", [tf,tv]) =>
          let val (bt1,bt2,bt) = unBinFun "first argument to scan" tf
              val (btv,r) = unArr' "scan argument" tv
            (* TODO: add assertions *)
          in Arr bt r
          end
        | ("idxS", [tsi,ti,ta]) =>
          let val (bta,r) = unArr' "idxS array argument" ta
              val (btsi,si) = unS' "idxS offset argument" tsi
              val rv = RnkVarCon (fn i => unifyR r (rnk(i+1)))
              val rv' = RnkVarCon (fn i => unifyR rv (rnk(i-1)))
          in assert_sub "idxS expression" ti Int
           ; assertB "idxS expects offset argument to be an integer" btsi IntB
           ; assertR "idxS" rv' r
           ; Arr bta rv
          end
        | ("idxassign", [tis,ta,tv]) =>
          let val (bta,ra) = unArr' "idxassign array argument" ta
              val btv = unScl "idxassign value element" tv
          in case unVec0 tis of
                 SOME (btis,r) =>
                 ( assertB "idxassign expects integer index vector" btis IntB
                 ; assertB "idxassign expects array argument and value argument to be of the same underlying type" bta btv
                 ; assertR "idxassign expects integer vector to be identical to argument array rank" ra r
                 ; Bool)
               | NONE => raise Fail "idxassign expects statically sized index vector"
          end
        | ("compress", [tb,ta]) =>
          let val (btb,rb) = unArr' "compress first argument" tb
              val (bta,ra) = unArr' "compress second argument" ta
          in assertB "compress expects boolean array as first argument" btb BoolB
           ; assertR "compress expects arrays of the same rank" rb ra
           ; Arr bta ra
          end
        | ("replicate", [t,tb,ta]) =>
          let val bt = unScl "replicate default element" t
              val (btb,rb) = unArr' "replicate control argument" tb
              val (bta,ra) = unArr' "replicate value argument" ta
          in assertB "replicate expects default element to be compatiple with the value argument" bt bta
           ; assertB "replicate expects integer array as control argument" btb IntB
           ; assertR "replicate expects arrays of the same rank" rb ra
           ; Arr bta ra
          end
        | ("each", [tf,tv]) => type_each opr tf tv
        | ("eachV", [tf,tv]) => type_each opr tf tv
        | ("i2d",[t]) => (assert_sub opr t Int; Double)
        | ("b2i",[t]) =>
          (assert_sub opr t Bool;
           case unS t of
               SOME(_,r) => S IntB r
             | NONE => Int)
        | ("b2iV",[t]) =>
          (case unS t of
               SOME(bt,r) => (assertB opr bt BoolB; S IntB r)
             | NONE => raise Fail (opr ^ " expects argument of singleton integer type"))
        | ("negi",[t]) => 
          let fun default() = (assert_sub opr t Int; Int)
          in case unSii t of
                 NONE => default()
               | SOME i => S IntB (rnk(~i))
          end
        | ("absi",[t]) => (assert_sub opr t Int; Int)
        | ("noti",[t]) => (assert_sub opr t Int; Int)
        | ("nowi",[t]) => (assert_sub opr t Int; Int)
        | ("signi",[t]) => (assert_sub opr t Int; Int)
        | ("signd",[t]) => (assert opr Double t; Int)
        | ("negd",[t]) => (assert opr Double t; Double)
        | ("floor",[t]) => (assert opr Double t; Int)
        | ("ceil",[t]) => (assert opr Double t; Int)
        | ("absd",[t]) => (assert opr Double t; Double)
        | ("expd",[t]) => (assert opr Double t; Double)
        | ("notb",[t]) => (assert_sub opr t Bool; Bool)
        | ("ln",[t]) => (assert opr Double t; Double)
        | ("cos",[t]) => (assert opr Double t; Double)
        | ("sin",[t]) => (assert opr Double t; Double)
        | ("tan",[t]) => (assert opr Double t; Double)
        | ("acos",[t]) => (assert opr Double t; Double)
        | ("asin",[t]) => (assert opr Double t; Double)
        | ("atan",[t]) => (assert opr Double t; Double)
        | ("cosh",[t]) => (assert opr Double t; Double)
        | ("sinh",[t]) => (assert opr Double t; Double)
        | ("tanh",[t]) => (assert opr Double t; Double)
        | ("roll",[t]) => (assert opr Int t; Double)
        | ("iotaV",[t]) =>
          (case unS t of
               SOME (bt, r) => (assertB opr IntB bt; Vcc IntB r)
             | NONE => raise Fail (opr ^ " expects argument of singleton type"))
        | ("shapeV", [t]) => 
          (case unVec0 t of
               SOME (_,n) => SV IntB n
             | NONE => raise Fail "shapeV expects argument of sized vector type")
        | ("vreverseV",[t1]) =>
          (case unVcc t1 of
               SOME _ => t1
             | NONE => raise Fail (opr ^ " expects argument to be a sized vector type"))
        | ("vrotateV",[t1,t2]) =>
          (assert_sub opr t1 Int;           
           case unVcc t2 of
               SOME _ => t2
             | NONE => raise Fail (opr ^ " expects second argument to be a sized vector type"))
        | ("prSclI",[t]) => (assert_sub opr t Int; t)
        | ("prSclB",[t]) => (assert_sub opr t Bool; t)
        | ("prSclD",[t]) => (assert_sub opr t Double; t)
        | ("prSclC",[t]) => (assert_sub opr t Char; t)
        | ("prArrI",[t]) => (assert_sub opr t (Arr IntB (RnkVar())); t)
        | ("prArrB",[t]) => (assert_sub opr t (Arr BoolB (RnkVar())); t)
        | ("prArrD",[t]) => (assert_sub opr t (Arr DoubleB (RnkVar())); t)
        | ("prArrC",[t]) => (assert_sub opr t (Arr CharB (RnkVar())); t)
        | ("formatI",[t]) => (assert_sub opr t Int; VecB CharB)
        | ("formatD",[t]) => (assert_sub opr t Double; VecB CharB)
        | ("readFile",[t]) => (assert_sub opr t (VecB CharB); VecB CharB)
        | ("readIntVecFile",[t]) => (assert_sub opr t (VecB CharB); VecB IntB)
        | ("readDoubleVecFile",[t]) => (assert_sub opr t (VecB CharB); VecB DoubleB)
        | (_,[t1,t2]) =>
          if isBinOpIII opr then tyBin Int Int Int opr t1 t2
          else if isBinOpDDD opr then tyBin Double Double Double opr t1 t2
          else if isBinOpIIB opr then tyBin Int Int Bool opr t1 t2
          else if isBinOpDDB opr then tyBin Double Double Bool opr t1 t2
          else if isBinOpBBB opr then tyBin Bool Bool Bool opr t1 t2
          else if isBinOpCCB opr then tyBin Char Char Bool opr t1 t2
          else raise Fail ("binary operator " ^ Util.quote opr ^ " not supported")
        | _ => raise Fail ("operator " ^ Util.quote opr ^ ", with "
                           ^ Int.toString (length ts) 
                           ^ " arguments, not supported")
  and tyBin t1 t2 t3 opr t1' t2' =
      (assert_sub ("first argument to " ^ opr) t1' t1;
       assert_sub ("second argument to " ^ opr) t2' t2;
       t3)

  fun tyIff (tc,t1,t2) =
      ( assert_sub "conditional expression" Bool tc
      ; join t1 t2 handle Fail s => raise Fail (s ^ " in conditional branches"))

  datatype 't report = OK of 't | ERR of string
  fun typeExp (E:env) e : typ report =
      let fun ty E e =
              case e of
                  Var(v, t0) => (case lookup E v of
                                     SOME t => (assert "var" t t0; t)
                                  |  NONE => raise Fail ("Unknown variable " ^ Util.quote (ppVar v)))
                | I n => S IntB (rnk n)
                | D _ => Double
                | B true => S BoolB (rnk 1)
                | B false => S BoolB (rnk 0)
                | C _ => Char
                | Iff (c,e1,e2,t0) =>
                  (assert_sub "conditional" (ty E c) Bool;
                   assert_sub "conditional true branch" (ty E e1) t0;
                   assert_sub "conditional false branch" (ty E e2) t0;
                   t0)
                | Vc(es,t0) =>
                  let val ts = List.map (ty E) es
                      val t = tyVc ts
                  in assert_sub "vector expression" t t0
                   ; t0
                  end
                | Let (v,t,e1,e2,t0) =>
                  (assert_sub ("let-binding of " ^ ppVar v) (ty E e1) t;
                   let val t' = ty (add E v t) e2
                   in assert_sub "let" t' t0
                    ; t'
                   end)
                | Fn (v,t,e,t0) =>
                  let val t' = ty (add E v t) e
                  in assert "fun" t' t0;
                     Fun(t,t')
                  end
                | Op (opr, es, t0) => 
                  let val ts = List.map (ty E) es
                      val t = tyOp opr ts
                  in assert_sub opr t t0
                   ; t
                  end
      in OK(ty E e) handle Fail s => ERR s
      end

  fun typeOf e : typ =
      case e of
          Var(_,t) => t
        | I i => S IntB (rnk i)
        | D _ => Double
        | B true => S BoolB (rnk 1)
        | B false => S BoolB (rnk 0)
        | C _ => Char
        | Iff(_,_,_,t) => t
        | Vc(_,t) => t
        | Op(_,_,t) => t
        | Let(_,_,_,_,t) => t
        | Fn(v,t,e,t') => Fun(t,t')

  fun isShOpr t opr =
    case (opr       , unS t    , unSV t   , unVcc t  ) of
         ("first"   , SOME _   , _        , _        ) => true
      |  ("shape"   , _        , SOME _   , _        ) => true
      |  ("take"    , _        , _        , SOME _   ) => true
      |  ("drop"    , _        , _        , SOME _   ) => true
      |  ("cat"     , _        , _        , SOME _   ) => true
      |  ("cons"    , _        , _        , SOME _   ) => true
      |  ("snoc"    , _        , _        , SOME _   ) => true
      |  ("iota"    , _        , _        , SOME _   ) => true
      |  ("vreverse", _        , _        , SOME _   ) => true
      |  ("vrotate" , _        , _        , SOME _   ) => true
      |  ("each"    , _        , _        , SOME _   ) => true
      |  ("b2i"     , SOME _   , _        , _        ) => true
      | _ => false

  fun prOpr t opr =
      if isShOpr t opr then opr ^ "V"
      else opr

  fun resolveShOpr e =
      case e of
          Var _ => e
        | I _ => e
        | D _ => e
        | B _ => e
        | C _ => e
        | Iff(e1,e2,e3,t) => Iff(resolveShOpr e1,resolveShOpr e2,resolveShOpr e3,t)
        | Vc(es,t) => Vc(List.map resolveShOpr es,t)
        | Op(opr,es,t) => Op(prOpr t opr, List.map resolveShOpr es,t)
        | Let(v,t,e1,e2,t') => Let(v,t,resolveShOpr e1,resolveShOpr e2,t')
        | Fn(v,t,e,t') => Fn(v,t,resolveShOpr e,t')

  fun Iff_e (c,e1,e2) =
      let val t0 = tyIff(typeOf c, typeOf e1, typeOf e2)
      in Iff(c,e1,e2,t0)
      end handle Fail s => raise Fail ("Iff_e: " ^ s)
         
  fun Vc_e es =
      let val ts = List.map typeOf es
          val t = tyVc ts
      in Vc(es,t)
      end handle Fail s => raise Fail ("Vc_e: " ^ s)

  fun Op_e (opr,es) =
      let val ts = List.map typeOf es
          val t = tyOp opr ts
      in Op(opr,es,t)
      end handle Fail s => raise Fail ("Op_e: " ^ s)

  fun Let_e (v,t,e,e') =
      let val t' = typeOf e'
      in Let(v,t,e,e',t')
      end

  fun Fn_e (v,t,e) =
      let val t' = typeOf e
      in Fn(v,t,e,t')
      end

  datatype bv = Ib of int
              | Db of real
              | Bb of bool
              | Cb of word
              | Fb of denv * var * typ * texp * typ
  withtype denv = (var * bv Apl.APLArray) list

  type value = bv Apl.APLArray

  fun Dvalue v = Apl.scl (Db 0.0) (Db v)
  fun unDvalue _ = raise Fail "exp.unDvalue: not implemented"
  val Uvalue = Dvalue 0.0

  fun pr_int i =
      if i = ~2147483648 then "-2147483648"
      else if i < 0 then "-" ^ pr_int (~i)
      else Int32.toString i

  fun pr_double d =
      if d < 0.0 then "-" ^ pr_double (~d)
      else
        if Real.==(d,Real.posInf) then "HUGE_VAL"
        else 
          let val s = Real.toString d
          in if CharVector.exists (fn c => c = #".") s then s
             else s ^ ".0"
          end

  fun wordToChar w =
      if w < 0w128 then
        let val c = Char.chr (Word.toInt w)
        in if Char.isAscii c then c
           else raise Fail "non-printable character not allowed"
        end
      else raise Fail "non-printable character not allowed"

  fun pr_char w =
      if w < 0w128 then
        let val c = Char.chr (Word.toInt w)
        in if Char.isAscii c then Char.toString c
           else Word.toString w
        end
      else Word.toString w

  fun pr_bv b =
      case b of
          Ib b => pr_int b
        | Db b => pr_double b
        | Bb true => "1"
        | Bb false => "0"
        | Cb w => pr_char w
        | Fb _ => "fn"
                      
  fun pr_value v = Apl.pr (pr_bv,",") v

  val emptyDEnv = nil
  val addDE = add

  fun unIb (Ib b) = b
    | unIb _ = raise Fail "exp.unIb"
  fun unCb (Cb w) = w
    | unCb _ = raise Fail "exp.unCb"
  fun unDb (Db b) = b
    | unDb _ = raise Fail "exp.unDb"
  fun unBb (Bb b) = b
    | unBb _ = raise Fail "exp.unBb"
  fun unFb (Fb b) = b
    | unFb _ = raise Fail "exp.unFb"

  fun unBase s t fi fd fb fc =
      let val (bt,_) = unArr' ("unBase:" ^ s) t
      in if isInt bt then fi()
         else if isDouble bt then fd()
         else if isBool bt then fb()
         else if isChar bt then fc()
         else fi() (*raise Fail ("exp.unBase: expecting base type: " ^ s) *) 
                   (* an unused zilde results in a type variable not being unified... *)
      end

  fun default t =
      unBase "default" t (fn() => Ib 0) (fn() => Db 0.0) (fn() => Bb false) (fn() => Cb 0w32)

  fun resType (_,_,_,_,_,_,t) = t

  fun println s = print (s ^ "\n")

  fun format pr unArg x =
      let val arg = Apl.unScl "format" x
          val s = pr (unArg arg)
          val elts = (List.map (Cb o Word.fromInt o Char.ord) o explode) s
      in Apl.vec (Cb 0w32) elts
      end

  fun prArr v = (println(pr_value v); v)
  fun prArrC v = (println(Apl.pr (pr_bv,"") v); v)

  val rgen = ref (Random.newgen ())
  fun roll 0 = Random.random (!rgen)
    | roll i = real (Random.range (0,i) (!rgen))

  fun fileVecReader fname (f: string -> 'a list) (d: 'a) (g : 'a -> 'b) : 'b Apl.APLArray =
      let val v = Apl.map #" " (fn Cb w => wordToChar w 
                                 | _ => raise Fail "eval:fileVecReader") fname
          fun finally x f g =
              (f x before g x) handle ? => (g x; raise ?)
          fun readFile fname = 
              finally (TextIO.openIn fname) TextIO.inputAll TextIO.closeIn
          val fname = Apl.pr (Char.toString,"") v
          val content = readFile fname
          val list = f content
      in Apl.vec (g d) (List.map g list)
      end

  val processStartTime = Time.now()
  fun nowMilliseconds() =
      let val t = Time.now()
          val delta = Time.-(t, processStartTime)
      in Int.fromLarge(Time.toMilliseconds delta)
      end

  fun eval DE e : value =
      case e of
          Var(x,_) =>
          (case lookup DE x of
               SOME v => v
             | NONE => raise Fail ("eval.cannot locate variable " ^ ppVar x))
        | I i => Apl.scl (Ib 0) (Ib i)
        | D d => Apl.scl (Db 0.0) (Db d)
        | B b => Apl.scl (Bb false) (Bb b)
        | C w => Apl.scl (Cb 0w32) (Cb w)
        | Iff (e1,e2,e3,t) =>
          let val b = Apl.liftU false (fn Bb b => b | _ => raise Fail "eval:Iff") (eval DE e1)
          in Apl.iff(b,fn() => eval DE e2, fn() => eval DE e3)
          end
        | Vc (nil,t) => Apl.zilde (default t)
        | Vc (x::xs,t) => eval DE (Op("cons",[x, Vc(xs,t)],t))
        | Op (opr, es, t) =>
          let fun fail() = raise Fail ("exp.eval: operator " ^ opr ^ " not supported with " 
                                       ^ Int.toString (length es) ^ " arguments")
              fun tryShOpr () = if String.isSuffix "V" opr then
                                  let val opr' = String.substring(opr,0,size opr-1)
                                  in eval DE (Op(opr',es,t))
                                  end
                                else fail()
          in case (opr,es) of
                 ("zilde", []) => Apl.zilde (default t)
               | ("pi", []) => Apl.scl (default t) (Db Math.pi)
               | ("i2d", [e]) => Apl.liftU (Db 0.0) (fn Ib i => Db(real i) | _ => raise Fail "eval:i2d") (eval DE e)
               | ("b2i", [e]) => Apl.liftU (Ib 0) (fn Bb b => Ib(if b then 1 else 0) | _ => raise Fail "eval:b2i") (eval DE e)
               | ("negi", [e]) => Apl.liftU (Ib 0) (fn Ib i => Ib(~i) | _ => raise Fail "eval:negi") (eval DE e)
               | ("absi", [e]) => Apl.liftU (Ib 0) (fn Ib i => Ib(Int.abs i) | _ => raise Fail "eval:absi") (eval DE e)
               | ("nowi", [e]) => Apl.liftU (Ib 0) (fn Ib 0 => Ib(nowMilliseconds()) | _ => raise Fail "eval:nowi") (eval DE e)
               | ("signi", [e]) => Apl.liftU (Ib 0) (fn Ib i => Ib(if i >= 0 then 1 else ~1) | _ => raise Fail "eval:signi") (eval DE e)
               | ("signd", [e]) => Apl.liftU (Ib 0) (fn Db i => Ib(if i >= 0.0 then 1 else ~1) | _ => raise Fail "eval:signd") (eval DE e)
               | ("negd", [e]) => Apl.liftU (Db 0.0) (fn Db i => Db(Real.~i) | _ => raise Fail "eval:negd") (eval DE e)
               | ("absd", [e]) => Apl.liftU (Db 0.0) (fn Db i => Db(Real.abs i) | _ => raise Fail "eval:absd") (eval DE e)
               | ("expd", [e]) => Apl.liftU (Db 0.0) (fn Db i => Db(Math.exp i) | _ => raise Fail "eval:expd") (eval DE e)
               | ("floor", [e]) => Apl.liftU (Ib 0) (fn Db i => Ib(Real.floor i) | _ => raise Fail "eval:floor") (eval DE e)
               | ("ceil", [e]) => Apl.liftU (Ib 0) (fn Db i => Ib(Real.ceil i) | _ => raise Fail "eval:ceil") (eval DE e)
               | ("notb", [e]) => Apl.liftU (Bb false) (fn Bb b => Bb(not b) | _ => raise Fail "eval:notb") (eval DE e)
               | ("ln", [e]) => Apl.liftU (Db 0.0) (fn Db d => Db(Math.ln d) | _ => raise Fail "eval:ln") (eval DE e)
               | ("sin", [e]) => Apl.liftU (Db 0.0) (fn Db d => Db(Math.sin d) | _ => raise Fail "eval:sin") (eval DE e)
               | ("cos", [e]) => Apl.liftU (Db 0.0) (fn Db d => Db(Math.cos d) | _ => raise Fail "eval:cos") (eval DE e)
               | ("tan", [e]) => Apl.liftU (Db 0.0) (fn Db d => Db(Math.tan d) | _ => raise Fail "eval:tan") (eval DE e)
               | ("asin", [e]) => Apl.liftU (Db 0.0) (fn Db d => Db(Math.asin d) | _ => raise Fail "eval:asin") (eval DE e)
               | ("acos", [e]) => Apl.liftU (Db 0.0) (fn Db d => Db(Math.acos d) | _ => raise Fail "eval:acos") (eval DE e)
               | ("atan", [e]) => Apl.liftU (Db 0.0) (fn Db d => Db(Math.atan d) | _ => raise Fail "eval:atan") (eval DE e)
               | ("sinh", [e]) => Apl.liftU (Db 0.0) (fn Db d => Db(Math.sinh d) | _ => raise Fail "eval:sinh") (eval DE e)
               | ("cosh", [e]) => Apl.liftU (Db 0.0) (fn Db d => Db(Math.cosh d) | _ => raise Fail "eval:cosh") (eval DE e)
               | ("tanh", [e]) => Apl.liftU (Db 0.0) (fn Db d => Db(Math.tanh d) | _ => raise Fail "eval:tanh") (eval DE e)
               | ("roll", [e]) => Apl.liftU (Db 0.0) (fn Ib i => Db(roll i) | _ => raise Fail "eval:roll") (eval DE e)
               | ("iota", [e]) => Apl.map (Ib 0) Ib (Apl.iota (Apl.map 0 unIb (eval DE e)))
               | ("reshape", [e1,e2]) =>
                 let val v1 = Apl.map 0 unIb (eval DE e1)
                 in Apl.reshape(v1,eval DE e2)
                 end
               | ("shape", [e]) =>
                 let val v = Apl.shape(eval DE e)
                 in Apl.map (Ib 0) Ib v
                 end
               | ("drop", [e1,e2]) =>
                 let val v1 = Apl.map 0 unIb (eval DE e1)
                 in Apl.drop(v1,eval DE e2)
                 end
               | ("take", [e1,e2]) =>
                 let val v1 = Apl.map 0 unIb (eval DE e1)
                 in Apl.take(v1,eval DE e2)
                 end
               | ("vrotate", [e1,e2]) =>
                 let val v1 = Apl.map 0 unIb (eval DE e1)
                 in Apl.vrotate(v1,eval DE e2)
                 end
               | ("vreverse", [e]) => Apl.vreverse (eval DE e)
               | ("first", [e]) => Apl.first (eval DE e)
               | ("mem", [e]) => eval DE e
               | ("transp", [e]) => Apl.transpose (eval DE e)
               | ("transp2", [e1,e2]) =>
                 let val v1 = Apl.map 0 unIb (eval DE e1)
                 in Apl.transpose2(v1,eval DE e2)
                 end
               | ("cons", [e1,e2]) => Apl.cons(eval DE e1,eval DE e2)
               | ("snoc", [e1,e2]) => Apl.snoc(eval DE e1,eval DE e2)
               | ("rav", [e]) => Apl.ravel(eval DE e)
               | ("cat", [e1,e2]) => Apl.catenate(eval DE e1,eval DE e2)
               | ("reduce", [f,n,a]) =>
                 let val F = unFb2 DE "reduce" f
                     val n = eval DE n
                     val a = eval DE a
                 in Apl.reduce (applyBin F) n a
                 end
               | ("scan", [f,a]) =>
                 let val F = unFb2 DE "scan" f
                     val a = eval DE a
                 in Apl.scan (applyBin F) a
                 end
               | ("idxS", [x,i,a]) =>
                 let val x = Apl.map 0 unIb (eval DE x)
                     val i = Apl.map 0 unIb (eval DE i)
                     val a = eval DE a
                 in Apl.idxS(x,i,a)
                 end
               | ("idxassign", [is,a,v]) =>
                 let val is = Apl.map 0 unIb (eval DE is)
                     val a = eval DE a
                     val v = eval DE v
                     val v = Apl.unScl "idxassign" v
                 in Apl.idxassign(is,a,v)
                  ; Apl.scl (Bb false) (Bb true)
                 end
               | ("compress", [e1,e2]) => 
                 let val v1 = Apl.map false (fn Bb b => b | _ => raise Fail "eval:compress") (eval DE e1)
                 in Apl.compress(v1,eval DE e2)
                 end
               | ("replicate", [_,e1,e2]) => 
                 let val v1 = Apl.map 0 (fn Ib i => i | _ => raise Fail "eval:replicate") (eval DE e1)
                 in Apl.replicate(v1,eval DE e2)
                 end
               | ("power", [ef,en,a]) =>
                 let val (DE0,v,_,e,_) = unFb(Apl.unScl"eval:power"(eval DE ef))
                     val vn = Apl.map 0 unIb (eval DE en)
                 in Apl.power (fn y => eval (addDE DE0 v y) e) vn (eval DE a)
                 end
               | ("powerScl", [ef,en,a]) =>
                 let val (DE0,v,_,e,_) = unFb(Apl.unScl"eval:powerScl"(eval DE ef))
                     val vn = Apl.map 0 unIb (eval DE en)
                 in Apl.power (fn y => eval (addDE DE0 v y) e) vn (eval DE a)
                 end
               | ("condScl", [ef,en,a]) =>
                 let val (DE0,v,_,e,_) = unFb(Apl.unScl"eval:condScl"(eval DE ef))
                     val vn = Apl.map 0 ((fn true => 1 | false => 0) o unBb) (eval DE en)
                 in Apl.power (fn y => eval (addDE DE0 v y) e) vn (eval DE a)
                 end
               | ("each", [e1,e2]) =>
                 let val (DE0,v,t,e,t') = unFb(Apl.unScl"eval:each"(eval DE e1))
                 in Apl.each (default t') (fn y => eval (addDE DE0 v y) e) (eval DE e2)
                 end
               | ("zipWith", [f,e2,e3]) =>
                 let val F = unFb2 DE "zipWith" f
                 in Apl.zipWith (default (resType F)) (applyBin F) (eval DE e2) (eval DE e3)
                 end
               | ("prArrI",[e]) => prArr (eval DE e)
               | ("prArrB",[e]) => prArr (eval DE e)
               | ("prArrD",[e]) => prArr (eval DE e)
               | ("prArrC",[e]) => prArrC (eval DE e)
               | ("prSclI",[e]) => prArr (eval DE e)
               | ("prSclB",[e]) => prArr (eval DE e)
               | ("prSclD",[e]) => prArr (eval DE e)
               | ("prSclC",[e]) => prArr (eval DE e)
               | ("formatI",[e]) => format pr_int unIb (eval DE e)
               | ("formatD",[e]) => format pr_double unDb (eval DE e)
               | ("readFile",[e]) => fileVecReader (eval DE e) (List.map (Word.fromInt o Char.ord) o explode) 0w32 Cb 
               | ("readIntVecFile",[e]) =>
                 let fun scanner s =
                         let val ints = String.tokens (fn x => Char.isSpace x orelse x = #",") s
                         in List.map (fn s => case Int.fromString s of
                                                  SOME i => i
                                                | NONE => raise Fail ("expecting only integers in file - found '" ^ s ^ "'")) ints
                         end
                 in fileVecReader (eval DE e) scanner 0 Ib
                 end
               | ("readDoubleVecFile",[e]) =>
                 let fun scanner s =
                         let val fields = String.tokens (fn x => Char.isSpace x orelse x = #",") s
                         in List.map (fn s => case Real.fromString s of
                                                  SOME i => i
                                                | NONE => raise Fail ("expecting only numbers in file - found '" ^ s ^ "'")) fields
                         end
                 in fileVecReader (eval DE e) scanner 0.0 Db
                 end
               | (opr,[e1,e2]) =>
                 let val v1 = eval DE e1
                     val v2 = eval DE e2
                 in if isBinOpIII opr then evalBinOpIII opr v1 v2
                    else if isBinOpDDD opr then evalBinOpDDD opr v1 v2
                    else if isBinOpIIB opr then evalBinOpIIB opr v1 v2
                    else if isBinOpCCB opr then evalBinOpCCB opr v1 v2
                    else if isBinOpDDB opr then evalBinOpDDB opr v1 v2
                    else if isBinOpBBB opr then evalBinOpBBB opr v1 v2
                    else tryShOpr()
                 end
               | (opr, _) => tryShOpr()
          end
        | Let (v,t,e1,e2,t') => eval (addDE DE v (eval DE e1)) e2
        | Fn (v,t,e,t') => Apl.scl (Ib ~1000) (Fb(DE,v,t,e,t'))
  and unFb2 DE s e =
      let val (DE0,x,tx,e,_) = unFb(Apl.unScl (s ^ ", first function argument") (eval DE e))
          val (_,y,ty,e,t) = unFb(Apl.unScl (s ^ ", second function argument") (eval DE e))
      in (DE0,x,y,e,tx,ty,t)
      end
  and applyBin (DE0,x,y,e,_,_,_) (a,b) =
      let val DE0 = addDE DE0 x a
          val DE0 = addDE DE0 y b
      in eval DE0 e
      end
  and evalBinOpIII opr v1 v2 =
      let fun wordOp f (x,y) =
              let val x = Word32.fromLargeInt(Int32.toLarge x)
                  val y = Word32.fromLargeInt(Int32.toLarge y)
              in Int32.fromLarge(Word32.toLargeIntX(f(x,y)))
              end
          val fct = case opr of
                        "addi" => (op +)
                      | "subi" => (op -)
                      | "muli" => (op * )
                      | "divi" => (op div)
                      | "resi" => (fn(x,y) => if x = 0 then y else Int32.mod(y,x))
                      | "maxi" => (fn (x,y) => if x > y then x else y)
                      | "mini" => (fn (x,y) => if x < y then x else y)
                      | "mod" => (op mod)
                      | "andi" => wordOp Word32.andb
                      | "ori" => wordOp Word32.orb
                      | "shli" => wordOp Word32.<<
                      | "shri" => wordOp Word32.>>
                      | "shari" => wordOp Word32.~>>
                      | "xori" => wordOp Word32.xorb
                      | _ => raise Fail ("evalBinOpIII: unsupported int*int->int operator " ^ opr)
      in Apl.liftB (Ib 0) (fn (b1,b2) => Ib(fct(unIb b1, unIb b2))) (v1,v2)
      end
  and evalBinOpIIB opr v1 v2 =
      let val fct = case opr of
                        "lti" => (op <)
                      | "ltei" => (op <=)
                      | "gti" => (op >)
                      | "gtei" => (op >=)
                      | "eqi" => (op =)
                      | _ => raise Fail ("evalBinOpIIB: unsupported int*int->bool operator " ^ opr)
      in Apl.liftB (Bb false) (fn (b1,b2) => Bb(fct(unIb b1, unIb b2))) (v1,v2)
      end
  and evalBinOpCCB opr v1 v2 =
      let val fct = case opr of
                        "ltc" => (op <)
                      | "ltec" => (op <=)
                      | "gtc" => (op >)
                      | "gtec" => (op >=)
                      | "eqc" => (op =)
                      | _ => raise Fail ("evalBinOpCCB: unsupported char*char->bool operator " ^ opr)
      in Apl.liftB (Bb false) (fn (b1,b2) => Bb(fct(unCb b1, unCb b2))) (v1,v2)
      end
  and evalBinOpDDD opr v1 v2 =
      let val fct = case opr of
                        "addd" => (op +)
                      | "subd" => (op -)
                      | "muld" => (op * )
                      | "divd" => (op /)
                      | "powd" => Math.pow
                      | "resd" => (fn(x,y) => if Real.==(x,0.0) then y 
                                              else y - real(Int32.fromInt(Real.floor(y/x))))
                      | "maxd" => (fn (x,y) => if x > y then x else y)
                      | "mind" => (fn (x,y) => if x < y then x else y)
                      | _ => raise Fail ("evalBinOpDDD: unsupported double*double->double operator " ^ opr)
      in Apl.liftB (Db 0.0) (fn (b1,b2) => Db(fct(unDb b1, unDb b2))) (v1,v2)
      end
  and evalBinOpDDB opr v1 v2 =
      let val fct = case opr of
                        "ltd" => (op <)
                      | "lted" => (op <=)
                      | "gtd" => (op >)
                      | "gted" => (op >=)
                      | "eqd" => Real.==
                      | _ => raise Fail ("evalBinOpDDB: unsupported double*double->bool operator " ^ opr)
      in Apl.liftB (Bb false) (fn (b1,b2) => Bb(fct(unDb b1, unDb b2))) (v1,v2)
      end
  and evalBinOpBBB opr v1 v2 =
      let val fct = case opr of
                        "andb" => (fn (x,y) => x andalso y)
                      | "orb" => (fn (x,y) => x orelse y)
                      | "eqb" => (fn (x,y) => x = y)
                      | "xorb" => (fn (x,y) => x <> y)
                      | "nandb" => (fn (x,y) => not(x andalso y))
                      | "norb" => (fn (x,y) => not(x orelse y))
                      | _ => raise Fail ("evalBinOpBBB: unsupported bool*bool->bool operator " ^ opr)
      in Apl.liftB (Bb false) (fn (b1,b2) => Bb(fct(unBb b1, unBb b2))) (v1,v2)
      end

end
