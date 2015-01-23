structure ILUtil : ILUTIL = struct
  open IL

  type e = Program.e
  type s = Program.s
  type ss = Program.s list
  type Value = IL.Value

  fun die s = raise Fail ("ILUtil." ^ s)

  fun iter f a (i,j) =
      let fun loop a n = if n > j then a
                         else loop (f (n,a)) (n+1)
      in loop a i
      end
      
  type Env = (Name.t * Value) list
  val empty = []
  fun add e (n,v) = (n,v)::e
  fun lookup E n =
      case List.find (fn (x,_) => x=n) E of
        SOME(_,v) => SOME v
      | NONE => NONE

  (* Simple pretty printing *)
  fun ppB Add = "+"
    | ppB Sub = "-"
    | ppB Mul = "*"
    | ppB Divv = "/"
    | ppB Modv = "%"
    | ppB Min = "min"
    | ppB Max = "max"
    | ppB Lt = "<"
    | ppB Lteq = "<="
    | ppB Eq = "=="
    | ppB Resi = "resi"
    | ppB Andb = "&&"
    | ppB Orb = "||"
    | ppB Xorb = "^"

  fun pp_int i =
      if i = ~2147483648 then "-2147483648"
      else if i < 0 then "-" ^ pp_int (~i)
      else Int32.toString i

  fun pp_double d =
      if d < 0.0 then "-" ^ pp_double (~d)
      else
        if Real.==(d,Real.posInf) then "HUGE_VAL"
        else 
          let val s = Real.toString d
          in if CharVector.exists (fn c => c = #".") s then s
             else s ^ ".0"
          end

  fun ppValue v = 
      case v of
        IntV i => pp_int i
      | DoubleV d => pp_double d
      | BoolV b => Bool.toString b
      | ArrV v => "vec"

  fun evalBinOp Add (IntV i1,IntV i2) = IntV(i1+i2)
    | evalBinOp Add (DoubleV i1,DoubleV i2) = DoubleV(i1+i2)
    | evalBinOp Sub (IntV i1,IntV i2) = IntV(i1-i2)
    | evalBinOp Sub (DoubleV i1,DoubleV i2) = DoubleV(i1-i2)
    | evalBinOp Mul (IntV i1,IntV i2) = IntV(i1*i2)
    | evalBinOp Mul (DoubleV i1,DoubleV i2) = DoubleV(i1*i2)
    | evalBinOp Divv (IntV i1,IntV i2) = IntV(i1 div i2)
    | evalBinOp Divv (DoubleV i1,DoubleV i2) = DoubleV(i1 / i2)
    | evalBinOp Modv (IntV i1,IntV i2) = IntV(i1 mod i2)
    | evalBinOp Resi (IntV i1,IntV i2) = IntV(if i2 = 0 then i1 else (i1 mod i2))
    | evalBinOp Modv (DoubleV i1,DoubleV i2) = die "evalBinOp.mod double not implemented"
    | evalBinOp Min (IntV i1,IntV i2) = IntV(if i1 < i2 then i1 else i2)
    | evalBinOp Min (DoubleV i1,DoubleV i2) = DoubleV(if i1 < i2 then i1 else i2)
    | evalBinOp Max (IntV i1,IntV i2) = IntV(if i1 > i2 then i1 else i2)
    | evalBinOp Max (DoubleV i1,DoubleV i2) = DoubleV(if i1 > i2 then i1 else i2)
    | evalBinOp Lt  (IntV i1,IntV i2) = BoolV(i1 < i2)
    | evalBinOp Lt  (DoubleV i1,DoubleV i2) = BoolV(i1 < i2)
    | evalBinOp Lteq  (IntV i1,IntV i2) = BoolV(i1 <= i2)
    | evalBinOp Lteq  (DoubleV i1,DoubleV i2) = BoolV(i1 <= i2)
    | evalBinOp Eq  (IntV i1,IntV i2) = BoolV(i1 = i2)
    | evalBinOp Eq  (DoubleV i1,DoubleV i2) = BoolV(Real.==(i1,i2))
    | evalBinOp Eq  (BoolV b1,BoolV b2) = BoolV(b1 = b2)
    | evalBinOp Andb (BoolV b1,BoolV b2) = BoolV(b1 andalso b2)
    | evalBinOp Orb (BoolV b1,BoolV b2) = BoolV(b1 orelse b2)
    | evalBinOp Xorb (BoolV b1,BoolV b2) = BoolV((b1 orelse b2) andalso b1 <> b2)
    | evalBinOp p (v1,v2) = die ("evalBinOp." ^ ppB p ^" - v1=" ^ ppValue v1 ^ ", v2=" ^ ppValue v2) 
        
  fun evalUnOp Neg (IntV i) = IntV(~i)
    | evalUnOp Neg (DoubleV d) = DoubleV(~d)
    | evalUnOp I2D (IntV i) = DoubleV(real i)
    | evalUnOp D2I (DoubleV d) = (IntV(Real.trunc d))
    | evalUnOp B2I (BoolV b) = (IntV(if b then 1 else 0))
    | evalUnOp _ _ = die "evalUnOp"

  fun eval (E:Env) (e:Exp) : Value =
      case e of
        Var n => (case lookup E n of
                    SOME v => v
                  | NONE => die("lookup: " ^ Name.pr n))
      | I i => IntV i
      | D d => DoubleV d
      | T => BoolV true
      | F => BoolV false
      | Binop(binop,e1,e2) => evalBinOp binop (eval E e1, eval E e2)
      | Unop(unop,e1) => evalUnOp unop (eval E e1)
      | Subs(n,e1) =>
        (case eval E e1 of
           IntV i => (case lookup E n of
                        SOME(ArrV v) => 
                        (case ! (Vector.sub(v,i)) of
                           SOME v => v
                         | NONE => die "eval.Subs.array value not initialized")                       
                      | _ => die("eval.Subs.lookup: " ^ Name.pr n))
         | _ => die "eval.Subs.expecting integer")
      | Alloc (t,e1) =>
        (case eval E e1 of
           IntV n => ArrV(Vector.tabulate(n,fn _ => ref NONE))
         | _ => die "eval.Alloc.expecting integer")
      | Vect (t,es) =>
        let val vs = List.map (ref o SOME o eval E) es
        in ArrV (Vector.fromList vs)
        end
      | If(e0,e1,e2) =>
        (case eval E e0 of
           BoolV b => eval E (if b then e1 else e2)
         | _  => die "eval.If.expecting boolean")

  exception Halted of string
  fun evalS E (s: Stmt) rn : Env =
      case s of
        For (e, f) =>
        (case eval E e of
           IntV n =>
           let val name = Name.new Type.Int
               val ss = f (Var name)
           in iter (fn (i,E) => 
                       let val E = add E (name,IntV i)
                       in evalSS E ss rn
                       end) E (0,n-1)
           end
         | _ => die "For")
      | Ifs(e,ss1,ss2) =>
        (case eval E e of
           BoolV b => evalSS E (if b then ss1 else ss2) rn
         | _ => die "eval.Ifs expects boolean")
      | Ret e => add E (rn, eval E e)
      | Halt s => raise Halted s
      | Assign (n,e) => add E (n, eval E e)
      | Decl (n,SOME e) => add E (n, eval E e)
      | Decl (n,NONE) => E
      | AssignArr (n,i,e) =>
        (case eval E i of
           IntV i =>
           let val v = eval E e
           in case lookup E n of
                SOME(ArrV vec) =>
                let val r = Vector.sub(vec,i)
                in r := SOME v; E
                end
              | _ => die "eval.AssignArr.couldn't find vector in env"
           end
         | _ => die "eval.AssignArr.expecting int as index")
      | Free n => die "Free.unimplemented"
      | Printf(s,nil) => (print s; E)
      | Printf(s,es) => die "eval.Printf not implemented"
      | Nop => E

  and evalSS E ss rn =
      List.foldl (fn (s,E) => evalS E s rn) E ss

  val emptyEnv = []

  datatype rope = % of string
                | %% of rope * rope
                | %> of rope
                | %$
  fun repeat s 0 = ""
    | repeat s n = s ^ repeat s (n-1)
  infix %%
  fun ropeToString n r =
      let fun loop n a = fn
              % s => s :: a
            | %$ => ("\n" ^ repeat "  " n) :: a
            | %> r => loop (n+1) a r
            | r1 %% r2 => loop n (loop n a r1) r2
      in (String.concat o rev o (loop n nil)) r
      end

  fun par e = %"(" %% e %% %")"
  fun spar e = %"[" %% e %% %"]"
  fun cpar e = %"{" %% e %% %"}"

  fun infi x = List.exists (fn y => x = y) [Add,Sub,Mul,Divv,Modv,Lt,Lteq,Eq,Orb,Andb,Xorb]

  fun ppU Neg = "-"
    | ppU I2D = "i2d"
    | ppU D2I = "d2i"
    | ppU B2I = "b2i"
    | ppU Not = "!"

  fun pp_t t = %(Type.prType t)

  fun pp e =
      case e of
        Var n => %(Name.pr n)
      | I i => %(pp_int i)
      | D d => %(pp_double d)
      | Binop(binop,e1,e2) => 
        if infi binop then par (pp e1 %% % (ppB binop) %% pp e2)
        else % (ppB binop) %% par(pp e1 %% %"," %% pp e2)
      | Unop(Neg,e1) => %(ppU Neg) %% (pp e1)
      | Unop(unop,e1) => %(ppU unop) %% par(pp e1)
      | Alloc (t,e1) => 
        let val t' = Type.vecElem t
        in %"(" %% pp_t t %% %")malloc(sizeof(" %% pp_t t' %% %")*" %% pp e1 %% %")"
        end
      | Vect (t,es) => 
        let val t' = Type.vecElem t
        in %"{" %% pp_es ", " es %% %"}"
        end
      | Subs(n,e1) => %(Name.pr n) %% spar(pp e1)
      | T => %(Bool.toString true)
      | F => %(Bool.toString false)
      | If(e0,e1,e2) => par(pp e0 %%  %" ? " %% pp e1 %% %" : " %% pp e2)
  and pp_es s nil = % ""
    | pp_es s [e] = pp e
    | pp_es s (e::es) = pp e %% %s %% pp_es s es 
  fun ppSS0 ss =
      case ss of
        nil => %""
      | Nop :: ss => ppSS0 ss
      | s :: ss => %$ %% ppS s %% ppSS0 ss
 
  and ppS s =
      case s of
        For (e, f) =>
        let val n = Name.new Type.Int
            val ns = Name.pr n 
        in %("for (int " ^ ns ^ " = 0; " ^ ns ^ " < ") %%
             pp e %% %("; " ^ ns ^ "++) {") %% 
               %>(ppSS0(f (Var n))) %%
             %$ %% %"}"
        end
      | Ifs(e,ss1,nil) => %"if (" %% pp e %% %") {" %%
                             %> (ppSS0 ss1) %% %$ %% 
                          %"}"
      | Ifs(e,ss1,ss2) => %"if (" %% pp e %% %") {" %%
                             %> (ppSS0 ss1) %% %$ %% 
                          %"} else {" %% 
                             %> (ppSS0 ss2) %% %$ %%
                          %"}"
      | Assign (n,e) => %(Name.pr n) %% %" = " %% pp e %% %";"
      | Decl (n,SOME(e as Vect(t,es))) =>
        let val t = Type.vecElem t
        in pp_t t %% %" " %% %(Name.pr n) %% %"[] = " %% pp e %% %";"
        end
      | Decl (n,SOME e) => pp_t (Name.typeOf n) %% %" " %% %(Name.pr n) %% %" = " %% pp e %% %";"
      | Decl (n,NONE) => pp_t (Name.typeOf n) %% %" " %% %(Name.pr n) %% %";"
      | AssignArr (n,i,e) => %(Name.pr n) %% spar(pp i) %% %" = " %% pp e %% %";"
      | Nop => %"/*nop*/"
      | Free n => die "Free.unimplemented"
      | Ret e => %"return " %% pp e %% %";"
      | Halt s => %"halt(\"" %% %s %% %"\");"
      | Printf(s,nil) => %("printf(\"" ^ String.toCString s ^ "\");")
      | Printf("%DOUBLE",[e]) => %"prDouble(" %% pp e %% %");" 
      | Printf(s,es) => %("printf(\"" ^ String.toCString s ^ "\",") %% pp_es "," es %% %");" 

  fun ppSS n ss = ropeToString n (%$ %% ppSS0 ss)
  fun ppExp e = ropeToString 0 (pp e)

  fun ppFunction name (ta,tb) argname ss =
      let val r =
              %(Type.prType tb) %% %" " %%
              %name %% par(%(Type.prType ta) %% %" " %% %(Name.pr argname)) %% %" " %% cpar(
              %>(ppSS0 ss) %% %$) %% %$
      in ropeToString 0 r
      end

  fun resTypeBinop binop =
      case binop of
        Add => Type.Int
      | Sub => Type.Int
      | Mul => Type.Int
      | Divv => Type.Int
      | Modv => Type.Int
      | Resi => Type.Int
      | Min => Type.Int
      | Max => Type.Int
      | Lt => Type.Bool
      | Lteq => Type.Bool
      | Eq => Type.Bool
      | Andb => Type.Bool
      | Orb => Type.Bool
      | Xorb => Type.Bool

  fun resTypeUnop Neg = Type.Int
    | resTypeUnop I2D = Type.Double
    | resTypeUnop D2I = Type.Int
    | resTypeUnop B2I = Type.Int
    | resTypeUnop Not = Type.Bool

  fun typeExp e =
      case e of
        Var n => Name.typeOf n
      | I n => Type.Int
      | D d => Type.Double
      | T => Type.Bool
      | F => Type.Bool
      | If (e,e1,e2) =>
        let val b = typeExp e
            val t1 = typeExp e1
            val t2 = typeExp e2
        in if b <> Type.Bool then
             die "TypeExp.Error.If: Expecting conditional of type bool"
           else if t1 <> t2 then
             die "TypeExp.Error.If: Expecting branches of equal type"
           else t1
        end
      | Subs(n,e) =>
        let val t = typeExp e
            val tv = Name.typeOf n
        in if t <> Type.Int then
             die ("TypeExp.Error.Subs: Expecting index expression of type int; got type "
                  ^ Type.prType t)
           else Type.vecElem tv
        end
      | Alloc(t,e0) =>
        let val t0 = typeExp e0
        in if t0 <> Type.Int then
             die "TypeExp.Error.Alloc: Expecting count expression of type int"
           else t
        end
      | Vect(t,es) =>
        let val ts = List.map typeExp es
        in if List.all (fn t => t = Type.Int) ts then        t
           else die "TypeExp.Error.Vect: Expecting expressions of type int"
        end
      | Binop(binop,e1,e2) => resTypeBinop binop
      | Unop(unop,e) => resTypeUnop unop
end
