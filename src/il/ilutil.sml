structure ILUtil : ILUTIL = struct
  open IL

  type e = Program.e
  type s = Program.s
  type ss = Program.s list
  type value = IL.value

  fun die s = raise Fail ("ILUtil." ^ s)

  type Env = (Name.t, value) Util.alist
  val emptyEnv : Env = Util.emptyAlist()
  val lookup = Util.lookupAlist
  val add = Util.extendAlist

  (* Simple pretty printing *)
  fun ppB Add = "+"
    | ppB Sub = "-"
    | ppB Mul = "*"
    | ppB Divv = "/"
    | ppB Modv = "%"
    | ppB Mini = "mini"
    | ppB Mind = "mind"
    | ppB Powd = "pow"
    | ppB Maxi = "maxi"
    | ppB Maxd = "maxd"
    | ppB Lt = "<"
    | ppB Lteq = "<="
    | ppB Eq = "=="
    | ppB Resi = "resi"
    | ppB Andb = "&&"
    | ppB Orb = "||"
    | ppB Xorb = "^"
    | ppB Ori = "ori"
    | ppB Andi = "andi"
    | ppB Xori = "xori"
    | ppB Shli = "shli"
    | ppB Shri = "shri"
    | ppB Shari = "shari"

  fun pp_char w =
      "'" ^ (Char.toCString o Char.chr o Word.toInt) w ^ "'"

  fun ppValue v =
      case v of
        IntV i => Util.intToCString i
      | DoubleV d => Util.realToCString d
      | CharV c => pp_char c
      | BoolV b => Bool.toString b
      | ArrV v => "vec"

  fun w2i w = Int32.fromLarge (Word32.toLargeIntX w)
  fun i2w i = Word32.fromLargeInt(Int32.toLarge i)

  fun conv opr i1 i2 = w2i(opr(i2w i1, i2w i2))

  fun conv' opr i1 i2 = w2i(opr(i2w i1, Word.fromInt(Int32.toInt i2)))

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
    | evalBinOp Ori (IntV i1,IntV i2) = IntV(conv Word32.orb i1 i2)
    | evalBinOp Andi (IntV i1,IntV i2) = IntV(conv Word32.andb i1 i2)
    | evalBinOp Xori (IntV i1,IntV i2) = IntV(conv Word32.xorb i1 i2)
    | evalBinOp Shli (IntV i1,IntV i2) = IntV(conv' Word32.<< i1 i2)
    | evalBinOp Shri (IntV i1,IntV i2) = IntV(conv' Word32.>> i1 i2)
    | evalBinOp Shari (IntV i1,IntV i2) = IntV(conv' Word32.~>> i1 i2)
    | evalBinOp Modv (DoubleV i1,DoubleV i2) = die "evalBinOp.mod double not implemented"
    | evalBinOp Mini (IntV i1,IntV i2) = IntV(if i1 < i2 then i1 else i2)
    | evalBinOp Mind (DoubleV i1,DoubleV i2) = DoubleV(if i1 < i2 then i1 else i2)
    | evalBinOp Maxi (IntV i1,IntV i2) = IntV(if i1 > i2 then i1 else i2)
    | evalBinOp Maxd (DoubleV i1,DoubleV i2) = DoubleV(if i1 > i2 then i1 else i2)
    | evalBinOp Powd (DoubleV i1,DoubleV i2) = DoubleV(Math.pow(i1,i2))
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

  val rgen = ref (Random.newgen ())

  val processStartTime = Time.now()
  fun nowMilliseconds() =
      let val t = Time.now()
          val delta = Time.-(t,processStartTime)
      in Int32.fromLarge(Time.toMilliseconds delta)
      end

  fun evalUnOp Neg (IntV i) = IntV(~i)
    | evalUnOp Neg (DoubleV d) = DoubleV(~d)
    | evalUnOp I2D (IntV i) = DoubleV(real(Int32.toInt i))
    | evalUnOp D2I (DoubleV d) = (IntV(Int32.fromInt(Real.trunc d)))
    | evalUnOp Ceil (DoubleV d) = (IntV(Int32.fromInt(Real.ceil d)))
    | evalUnOp Floor (DoubleV d) = (IntV(Int32.fromInt(Real.floor d)))
    | evalUnOp Ln (DoubleV d) = (DoubleV(Math.ln d))
    | evalUnOp Sin (DoubleV d) = (DoubleV(Math.sin d))
    | evalUnOp Cos (DoubleV d) = (DoubleV(Math.cos d))
    | evalUnOp Tan (DoubleV d) = (DoubleV(Math.tan d))
    | evalUnOp Expd (DoubleV d) = (DoubleV(Math.exp d))
    | evalUnOp Sqrt (DoubleV d) = (DoubleV(Math.sqrt d))
    | evalUnOp Roll (IntV 0) = (DoubleV(Random.random (!rgen)))
    | evalUnOp Roll (IntV i) = (DoubleV(real (Random.range (0,Int32.toInt i) (!rgen))))
    | evalUnOp B2I (BoolV b) = (IntV(if b then 1 else 0))
    | evalUnOp Now (IntV 0) = (IntV(nowMilliseconds()))
    | evalUnOp _ _ = die "evalUnOp"

  fun eval (E:Env) (e:Exp) : value =
      case e of
        Var n => (case lookup E n of
                    SOME v => v
                  | NONE => die("eval.Var: " ^ Name.pr n))
      | I i => IntV i
      | D d => DoubleV d
      | C c => CharV c
      | T => BoolV true
      | F => BoolV false
      | Binop(binop,e1,e2) => evalBinOp binop (eval E e1, eval E e2)
      | Unop(unop,e1) => evalUnOp unop (eval E e1)
      | Subs(n,e1) =>
        (case eval E e1 of
           IntV i => (case lookup E n of
                        SOME(ArrV v) =>
                        (case ! (Vector.sub(v,Int32.toInt i)) of
                           SOME v => v
                         | NONE => die "eval.Subs.array value not initialized")
                      | _ => die("eval.Subs.lookup: " ^ Name.pr n))
         | _ => die "eval.Subs.expecting integer")
      | Alloc (t,e1) =>
        (case eval E e1 of
           IntV n => ArrV(Vector.tabulate(Int32.toInt n,fn _ => ref NONE))
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
        For (e, name, body) =>
        (case eval E e of
           IntV n =>
           Util.iter (fn (i,E) =>
                         let val E = add E (name,IntV (Int32.fromInt i))
                         in evalSS E body rn
                         end) E (0,Int32.toInt n - 1)
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
                let val r = Vector.sub(vec,Int32.toInt i)
                in r := SOME v; E
                end
              | _ => die "eval.AssignArr.couldn't find vector in env"
           end
         | _ => die "eval.AssignArr.expecting int as index")
      | Free n => die "Free.unimplemented"
      | Printf(s,nil) => (print s; E)
      | Printf(s,es) => die "eval.Printf not implemented"
      | Sprintf(n,s,es) => die "eval.Sprintf not implemented"
      | ReadIntVecFile(n1,n2,e) => die "eval.ReadIntVecFile not implemented"
      | ReadDoubleVecFile(n1,n2,e) => die "eval.ReadDoubleVecFile not implemented"
      | Comment _ => E
      | Nop => E

  and evalSS E ss rn =
      List.foldl (fn (s,E) => evalS E s rn) E ss

  datatype rope = % of string
                | %% of rope * rope
                | %> of rope
                | %$
                | Par of rope  (* avoid nested pars *)
  fun repeat s 0 = ""
    | repeat s n = s ^ repeat s (n-1)
  infix %%
  fun ropeToString n r =
      let fun loop n a = fn
              % s => s :: a
            | %$ => ("\n" ^ repeat "  " n) :: a
            | %> r => loop (n+1) a r
            | r1 %% r2 => loop n (loop n a r1) r2
            | Par e => loop n a (%"(" %% e %% %")")
      in (String.concat o rev o (loop n nil)) r
      end

  fun par (e as Par _) = e
    | par e = Par e
  fun unpar (Par e) = e
    | unpar e = e
  fun spar e = %"[" %% e %% %"]"
  fun cpar e = %"{" %% e %% %"}"

  fun infi x = List.exists (fn y => x = y) [Add,Sub,Mul,Divv,Modv,Lt,Lteq,Eq,Orb,Andb,Xorb]

  fun ppU Neg = "-"
    | ppU I2D = "i2d"
    | ppU D2I = "d2i"
    | ppU Ceil = "ceili"
    | ppU Floor = "floori"
    | ppU Ln = "ln"
    | ppU Sin = "sin"
    | ppU Cos = "cos"
    | ppU Tan = "tan"
    | ppU Expd = "exp"
    | ppU Sqrt = "sqrt"
    | ppU Roll = "roll"
    | ppU Now = "now"
    | ppU B2I = "b2i"
    | ppU Not = "!"
    | ppU Strlen = "strlen"

  fun pp_t t = %(Type.prType t)

  fun pp e =
      case e of
        Var n => %(Name.pr n)
      | I i => if i < 0 then par(%(Util.intToCString i)) else %(Util.intToCString i)
      | D d => if d < 0.0 then par(%(Util.realToCString d)) else %(Util.realToCString d)
      | C c => %(pp_char c)
      | Binop(binop,e1,e2) =>
        if infi binop then par (pp e1 %% % (ppB binop) %% pp e2)
        else % (ppB binop) %% par(pp e1 %% %"," %% pp e2)
      | Unop(Neg,e1) => %(ppU Neg) %% (pp e1)
      | Unop(unop,e1) => %(ppU unop) %% par(pp e1)
      | Alloc (t,e1) =>
        let val t' = Type.vecElem t
        in par(pp_t t) %% %"malloc(sizeof" %% par (pp_t t') %% %"*" %% pp e1 %% %")"
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
        For (e, n, body) =>
        let val ns = Name.pr n
        in %("for (int " ^ ns ^ " = 0; " ^ ns ^ " < ") %%
             pp e %% %("; " ^ ns ^ "++) {") %%
               %>(ppSS0 body) %%
             %$ %% %"}"
        end
      | Ifs(e,ss1,nil) => %"if " %% par(pp e) %% %" {" %%
                             %> (ppSS0 ss1) %% %$ %%
                          %"}"
      | Ifs(e,ss1,ss2) => %"if " %% par(pp e) %% %" {" %%
                             %> (ppSS0 ss1) %% %$ %%
                          %"} else {" %%
                             %> (ppSS0 ss2) %% %$ %%
                          %"}"
      | Assign (n,e) => %(Name.pr n) %% %" = " %% unpar(pp e) %% %";"
      | Decl (n,SOME(e as Vect(t,es))) =>
        let val t = Type.vecElem t
        in pp_t t %% %" " %% %(Name.pr n) %% %"[] = " %% unpar(pp e) %% %";"
        end
      | Decl (n,SOME e) => pp_t (Name.typeOf n) %% %" " %% %(Name.pr n) %% %" = " %% unpar(pp e) %% %";"
      | Decl (n,NONE) => pp_t (Name.typeOf n) %% %" " %% %(Name.pr n) %% %";"
      | AssignArr (n,i,e) => %(Name.pr n) %% spar(pp i) %% %" = " %% unpar(pp e) %% %";"
      | Nop => %"/*nop*/"
      | Free n => %("free(" ^ Name.pr n ^ ");")
      | Ret e => %"return " %% pp e %% %";"
      | Halt s => %"halt(\"" %% %s %% %"\");" %% %$ %% %"return 0;"
      | Printf(s,nil) => %("printf(\"" ^ String.toCString s ^ "\");")
      | Printf("%DOUBLE",[e]) => %"prDouble" %% par(pp e) %% %";"
      | Printf(s,es) => %("printf(\"" ^ String.toCString s ^ "\",") %% pp_es "," es %% %");"
      | Sprintf(n,"%DOUBLE",[e]) => %"formatD" %% par(pp_es "," [Var n,e]) %% %";"
      | Sprintf(n,s,nil) => %("sprintf(" ^ Name.pr n ^ ",\"" ^ String.toCString s ^ "\"") %% %");"
      | Sprintf(n,s,es) => %("sprintf(" ^ Name.pr n ^ ",\"" ^ String.toCString s ^ "\",") %% pp_es "," es %% %");"
      | ReadIntVecFile(n1,n2,e) => %(Name.pr n1 ^ " = readIntVecFile(" ^ Name.pr n2 ^ ",") %% pp e %% %");"
      | ReadDoubleVecFile(n1,n2,e) => %(Name.pr n1 ^ " = readDoubleVecFile(" ^ Name.pr n2 ^ ",") %% pp e %% %");"
      | Comment s => %("// " ^ s)

  fun ppSS n ss = ropeToString n (%$ %% ppSS0 ss)
  fun ppExp e = ropeToString 0 (pp e)

  fun ppFunction name (ta,tb) argname ss =
      let val r =
              %(Type.prType tb) %% %" " %%
              %name %% par(%(Type.prType ta) %% %" " %% %(Name.pr argname)) %% %" " %% cpar(
              %>(ppSS0 ss) %% %$) %% %$
      in ropeToString 0 r
      end

  fun assertEqT s t1 t2 =
      if t1 = t2 then ()
      else die ("assertEqT: " ^ s ^ "; t1=" ^ Type.prType t1 ^ "; t2=" ^ Type.prType t2)

  fun assertIntOrDouble s t =
      if t = Type.Int orelse t = Type.Double then ()
      else die ("assertIntOrDouble: " ^ s)

  fun assertIIorDD s t1 t2 =
      (assertEqT s t1 t2;
       assertIntOrDouble s t1)

  fun assertBB s t1 t2 =
      if t1 = t2 andalso t2 = Type.Bool then ()
      else die ("assertBB: " ^ s)

  fun assertII s t1 t2 =
      if t1 = t2 andalso t2 = Type.Int then ()
      else die ("assertII: " ^ s)

  fun assertDD s t1 t2 =
      if t1 = t2 andalso t2 = Type.Double then ()
      else die ("assertDD: " ^ s)

  fun assertIorD s t = if t = Type.Int orelse t = Type.Double then () else die ("assertIorD: " ^ s)
  fun assertI s t = if t = Type.Int then () else die ("assertI: " ^ s)
  fun assertD s t = if t = Type.Double then () else die ("assertD: " ^ s)
  fun assertB s t = if t = Type.Bool then () else die ("assertB: " ^ s)
  fun assertC s t = if t = Type.Char then () else die ("assertC: " ^ s)

  fun typeBinop binop t1 t2 =
      case binop of
        Add => (assertIIorDD "Add" t1 t2; t1)
      | Sub => (assertIIorDD "Sub" t1 t2; t1)
      | Mul => (assertIIorDD "Mul" t1 t2; t1)
      | Divv => (assertIIorDD "Divv" t1 t2; t1)
      | Modv => (assertIIorDD "Modv" t1 t2; t1)
      | Resi => (assertIIorDD "Resi" t1 t2; t1)
      | Mini => (assertII "Mini" t1 t2; t1)
      | Maxi => (assertII "Maxi" t1 t2; t1)
      | Mind => (assertDD "Mind" t1 t2; t1)
      | Maxd => (assertDD "Maxd" t1 t2; t1)
      | Powd => (assertDD "Powd" t1 t2; Type.Double)
      | Lt => (assertIIorDD "Lt" t1 t2; Type.Bool)
      | Lteq => (assertIIorDD "Lteq" t1 t2; Type.Bool)
      | Eq => (assertEqT "Eq" t1 t2; Type.Bool)
      | Andb => (assertBB "Andb" t1 t2; Type.Bool)
      | Orb => (assertBB "Orb" t1 t2; Type.Bool)
      | Xorb => (assertBB "Xorb" t1 t2; Type.Bool)
      | Ori => (assertII "Ori" t1 t2; Type.Int)
      | Andi => (assertII "Andi" t1 t2; Type.Int)
      | Xori => (assertII "Xori" t1 t2; Type.Int)
      | Shli => (assertII "Shli" t1 t2; Type.Int)
      | Shri => (assertII "Shri" t1 t2; Type.Int)
      | Shari => (assertII "Shari" t1 t2; Type.Int)

  fun typeUnop Neg t = (assertIorD "Neg" t; t)
    | typeUnop I2D t = (assertI "I2D" t; Type.Double)
    | typeUnop D2I t = (assertD "D2I" t; Type.Int)
    | typeUnop Ceil t = (assertD "Ceil" t; Type.Int)
    | typeUnop Floor t = (assertD "Floor" t; Type.Int)
    | typeUnop Ln t = (assertD "Ln" t; Type.Double)
    | typeUnop Sin t = (assertD "Sin" t; Type.Double)
    | typeUnop Cos t = (assertD "Cos" t; Type.Double)
    | typeUnop Tan t = (assertD "Tan" t; Type.Double)
    | typeUnop Expd t = (assertD "Expd" t; Type.Double)
    | typeUnop Sqrt t = (assertD "Sqrt" t; Type.Double)
    | typeUnop Roll t = (assertI "Roll" t; Type.Double)
    | typeUnop B2I t = (assertB "B2I" t; Type.Int)
    | typeUnop Now t = (assertI "Now" t; Type.Int)
    | typeUnop Not t = (assertB "Not" t; Type.Bool)
    | typeUnop Strlen t = (assertC "Strlen" (Type.vecElem t); Type.Int)

  fun typeExp e =
      case e of
        Var n => Name.typeOf n
      | I n => Type.Int
      | D d => Type.Double
      | C c => Type.Char
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
        in if List.all (fn t => t = Type.Int) ts then t
           else die "TypeExp.Error.Vect: Expecting expressions of type int"
        end
      | Binop(binop,e1,e2) => typeBinop binop (typeExp e1) (typeExp e2)
      | Unop(unop,e) => typeUnop unop (typeExp e)

  (* Common Subexpression Elimination *)

  (* [E : exp -> var * var list] maps each side-effect-free expression e to the pair of
   * the variable that defines e and the variable uses in e. *)

  structure FM = StringMap
  structure N = NameSet

  fun disjoint (s1,s2) = N.isEmpty(N.intersect(s1,s2))

  type cse_env = (Name.t * N.set) FM.map

  fun cuts (E:cse_env) ns : cse_env = FM.filter (fn (e,(n',ns')) => disjoint(ns,N.insert(n',ns'))) E
  fun cut (E:cse_env) n : cse_env = cuts E (N.singleton n)

  fun isPtrName n =
      case Name.typeOf n of
          Vec _ => true
        | _ => false

  val uses = Program.uses
  val uses_s = Program.uses_s
  val defs_s = Program.defs_s
  val defs_ss = Program.defs_ss

  fun cse (E: cse_env) ss =
      case ss of
          nil => nil
        | (s as Decl(n,SOME e)) :: ss =>
          if Program.side_effects e orelse isPtrName n orelse Program.simpleExp e then s :: cse E ss
          else let val eS = ppExp e
               in case FM.lookup E eS of
                      SOME(n',_) => Decl(n, SOME(Var n')) :: cse E ss
                    | NONE => let val E' = FM.add(eS,(n,uses e N.empty),E)
                              in s :: cse E' ss
                              end
               end
        | (s as Decl(_,NONE)) :: ss => s :: cse E ss
        | Assign(n,e) :: ss => Assign(n,repl E e) :: cse (cut E n) ss
        | (s as Halt _) :: _ => [s]
        | Nop :: ss => cse E ss
        | (s as Free n) :: ss => s :: cse E ss
        | Ret e :: _ => [Ret (repl E e)]
        | Printf(s,es) :: ss => Printf(s,List.map (repl E) es) :: cse E ss
        | Sprintf(n,s,es) :: ss => Sprintf(n,s,List.map (repl E) es) :: cse (cut E n) ss
        | ReadIntVecFile(n1,n2,e) :: ss => ReadIntVecFile(n1,n2,repl E e) :: cse (cuts E (N.fromList[n1,n2])) ss
        | ReadDoubleVecFile(n1,n2,e) :: ss => ReadDoubleVecFile(n1,n2,repl E e) :: cse (cuts E (N.fromList[n1,n2])) ss
        | (s as Comment _) :: ss => s :: cse E ss
        | AssignArr (n,e1,e2) :: ss => AssignArr (n,repl E e1,repl E e2) :: cse (cut E n) ss
        | Ifs(e,ss1,ss2) :: ss =>
          let val ns1 = defs_ss ss1
              val ns2 = defs_ss ss2
              val ns = N.union (ns1, ns2)
          in Ifs(repl E e, cse E ss1, cse E ss2) :: cse (cuts E ns) ss
          end
        | For (e,n,ss0) :: ss =>
          let val E' = cuts E (defs_ss ss0)
          in For (repl E e,n,cse E' ss0) :: cse E' ss
          end

  and repl E e =
      let val eS = ppExp e
      in case FM.lookup E eS of
             SOME (n,_) =>
             ((*print ("Substituting " ^ Name.pr n ^ " for " ^ eS ^ "\n");*) Var n)
           | NONE => e
      end

  val cse = fn ss => cse FM.empty ss

  (* Hoisting variable declarations outside of loops *)

  (* invariant:
       hoist U ss = (ssh, ssi)  ==>
         1) uses(ssh) \cap U = 0
         2) uses(ssh) \cap decl(ssi) = 0
         3) ssh contains declarations only.
  *)

  fun hoist U nil = (nil,nil)
    | hoist U (s::ss) =
      let val ds = defs_s s
          val us = uses_s s
          val (ssh,ss) = hoist (N.union(U,ds)) ss
      in case s of
             Decl(n,SOME e) =>
             if disjoint(us,U) andalso not(Program.side_effects e) then ((n,e)::ssh,ss)
             else let val (ssh,ss) = dehoist (N.singleton n) (ssh,ss)
                  in (ssh,s::ss)
                  end
           | For(e,n,ss0) =>
             let val (ssh0,ss0) = hoist (N.singleton n) ss0
                 val ds = defs_ss ss0
                 val (ssh0,ss0) = dehoist (N.insert(n,ds)) (ssh0,ss0)
             in (ssh0@ssh,For(e,n,ss0)::ss)
             end
           | Ifs(e,ss1,ss2) => (ssh,Ifs(e,hoistCat U ss1, hoistCat U ss2)::ss)
           | _ => (ssh,s::ss)
     end
  and hoistCat U ss =
      let val (ssh,ss) = hoist U ss
      in List.map (fn (n,e) => Decl(n,SOME e)) ssh @ ss
      end
  and dehoist U (nil,ss) = (nil, ss)
    | dehoist U ((n,e)::ssh,ss) =
      if disjoint(U, uses e (N.singleton n)) then
        let val (ssh,ss) = dehoist U (ssh,ss)
        in ((n,e)::ssh,ss)
        end
      else
        let val (ssh,ss) = dehoist (N.insert(n,U)) (ssh,ss)
        in (ssh,Decl(n,SOME e)::ss)
        end

  val hoist = fn ss => hoistCat N.empty ss

  (* ---------------------------------------
   * Loop splitting
   * --------------------------------------- *)

  (* we match only some simple cases *)
  fun splits_t N n e acc =
      case e of
          Binop(Lt,Var n',e') =>
          if n=n' andalso not(Program.side_effects e') andalso
             let val ns = uses e' N.empty
             in N.isEmpty(N.intersect(N,ns))
             end
          then e'::acc
          else acc
        | _ => acc

  fun splits_e N n e acc = acc  (* for now, only split on statement-level conditionals *)

  (* Find safe split-expressions in sequences of statements. *)
  fun splits N n nil acc = acc
    | splits N n (s::ss) acc =
      case s of
          Nop => splits N n ss acc
        | Ret e => splits_e N n e (splits N n ss acc)
        | Halt _ => splits N n ss acc
        | Free _ => splits N n ss acc
        | Decl(n',SOME e) => splits_e N n e (splits (N.insert(n',N)) n ss acc)
        | Decl(n',NONE) => splits (N.insert(n',N)) n ss acc
        | Assign(n',e) => splits_e N n e acc
        | AssignArr(n',e1,e2) => splits_e N n 1 (splits_e N n 2 acc)
        | For(e,n',body) =>
          splits N n ss (splits_e N n e (splits (N.insert(n',N)) n body acc))
        | Ifs(e,ss1,ss2) =>
          splits N n ss (splits N n ss1 (splits N n ss2 (splits_t N n e acc)))
        | Printf(_,es) => splits N n ss acc
        | Sprintf(n',_,es) => splits N n ss acc
        | ReadIntVecFile (n1,n2,e) => splits N n ss acc
        | ReadDoubleVecFile (n1,n2,e) => splits N n ss acc
        | Comment _ => splits N n ss acc

  fun loopSplit nil = nil
    | loopSplit (s::ss) =
      case s of
          Nop => loopSplit ss
        | For(e,n,body) =>
          let val N = N.insert(n,defs_ss body)
              val es = splits N n body []
          in case es of
                 nil => For(e,n,loopSplit body)::loopSplit ss
               | e'::_ =>
                 let val ne = Name.new Int
                     val ne' = Name.new Int
                     val n1 = Name.new Int
                     val n2 = Name.new Int
                     val n3 = Name.new Int
                     val n' = Name.new Int
                     val body = loopSplit body
                     val body' = Decl(n,SOME(Program.addi(Var n1,Var n3))) :: body
                 in Decl(ne,SOME e) ::
                    Decl(ne',SOME e') ::
                    Decl(n1,SOME(Program.maxi(I 0,Program.mini(Var ne,Var ne')))) ::
                    For(Var n1,n,body) ::
                    Decl(n2,SOME(Program.subi(Var ne,Var n1))) ::
                    For(Var n2,n3,Program.rename body') ::
                    loopSplit ss
                 end
          end
        | Ifs(e,ss1,ss2) => Ifs(e,loopSplit ss1,loopSplit ss2)::loopSplit ss
        | Ret _ => s::loopSplit ss
        | Halt _ => s::loopSplit ss
        | Free _ => s::loopSplit ss
        | Decl _ => s::loopSplit ss
        | Assign _ => s::loopSplit ss
        | AssignArr _ => s::loopSplit ss
        | Printf _ => s::loopSplit ss
        | Sprintf _ => s::loopSplit ss
        | ReadIntVecFile _ => s::loopSplit ss
        | ReadDoubleVecFile _ => s::loopSplit ss
        | Comment _ => s::loopSplit ss

end
