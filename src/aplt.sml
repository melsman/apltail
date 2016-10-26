structure Apl2Tail = Apl2Tail(Tail)

type res = (unit, Tail.Double Tail.Num) Tail.prog option

fun die s = raise Fail ("Apl2Tail." ^ s)

(* Parsing *)
fun parseFile (flags : Flags.flags) (pe : AplParse.env) (f : string) : AplAst.exp * AplParse.env =
    let val verbose_p = Flags.flag_p flags "-v"
        val silent_p = Flags.flag_p flags "-silent"
        val _ = Util.log (not silent_p) (fn _ => "[Reading file: " ^ f ^ "]")
        val s = Util.readFile f
        val ts = AplLex.lex f s
        val _ = Util.log verbose_p (fn _ => "File lexed:")
        val _ = Util.log verbose_p (fn _ => " " ^ AplLex.pr_tokens (map #1 ts))
        val _ = Util.log verbose_p (fn _ => "Parsing tokens...")
        val (e,pe') = AplParse.parse pe ts
        val _ = Util.log verbose_p (fn _ => "Parse success:\n " ^ AplAst.pr_exp e)
    in (e,pe')
    end

fun parseFiles (flags : Flags.flags) (pe0 : AplParse.env) (fs: string list) : AplAst.exp =
    let fun mergeExps (NONE,e) = SOME e
          | mergeExps (SOME e0,e) = SOME(AplParse.seq(e0,e))
        fun parseFs pe = 
            fn (nil, NONE) => raise Fail "Expecting at least one file"
             | (nil, SOME e) => e
             | (f::fs, acc) =>
                let val (e,pe') = parseFile flags pe f
                in parseFs (AplParse.plus(pe,pe')) (fs,mergeExps (acc,e))
                end 
    in parseFs pe0 (fs,NONE)
    end

(* Compilation *)
(* compileExp : flag list -> AplAst.exp -> (unit, Tail.Double Tail.Num) Tail.prog option *)
fun compileExp (flags : Flags.flags) (e : AplAst.exp) : res =
    let (* val compile_only_p = flag_p flags "-c" *)
        val verbose_p = Flags.flag_p flags "-v"
        val silent_p = Flags.flag_p flags "-silent"
        val p_tail = Flags.flag_p flags "-p_tail"
        val p_types = Flags.flag_p flags "-p_types"
        val optlevel = if Flags.flag_p flags "-noopt" then 0 else 1
        val materialize = Flags.flag_p flags "-materialize"
        val outfile = Flags.flag flags "-o"
        val p = Apl2Tail.compile {verbose=verbose_p, optlevel=optlevel, prtype=p_types, materialize=materialize} e
        val () =
            case outfile of
                SOME ofile => Tail.outprog p_types ofile p
              | NONE =>
                if p_tail andalso not verbose_p then
                  (if not silent_p then print "TAIL program:\n" else ();
                   print (Tail.pp_prog p_types p);
                   print "\n")
                else ()  (* program already printed! *)
    in SOME p
    end

fun errHandler (e : exn) : 'a option =
    case e of
        AplParse.ParseErr (l,msg) => (Util.prln ("Parse Error at " ^ 
                                                 Region.ppLoc l ^ ": \n  " ^ 
                                                 msg); NONE)
      | Fail s => (Util.prln s; NONE)
      | _ => raise e

fun compile flags (fs : string list) : res =
    let val s_parse = Flags.flag_p flags "-s_parse"   (* stop after parsing *)
        val e = parseFiles flags Apl2Tail.initialParseEnv fs
    in if s_parse
       then (Util.prln "Stopping after parsing."; NONE)
       else compileExp flags e
    end handle ? => errHandler ?


fun compileAndRun (flags,files) =
    let val compile_only_p = Flags.flag_p flags "-c"
        val verbose_p = Flags.flag_p flags "-v"
        val stop_after_tail_p = Flags.flag_p flags "-s_tail"
        val print_laila_p = Flags.flag_p flags "-p_laila"
        val silent_p = Flags.flag_p flags "-silent"
        val () = Laila.unsafeAsserts := Flags.flag_p flags "-unsafe"
        val () = Laila.enableComments := Flags.flag_p flags "-comments"
        val () = Laila.statistics_p := Flags.flag_p flags "-stat_laila"
        val () = Laila.hoist_p := Flags.flag_p flags "-opt_hoist"
        val () = Laila.loopsplit_p := Flags.flag_p flags "-opt_loopsplit"
        val () = case Flags.flag flags "-O" of
                     NONE => ()
                   | SOME n =>
                     case Int.fromString n of
                         SOME n => Tail.optimisationLevel := n
                       | NONE => die "expecting integer as argument to -O flag"
    in if silent_p andalso verbose_p then
         Util.prln "Inconsistent use of -silent and -v flags"
       else
         case compile flags files of
             SOME p =>
             let val () = if compile_only_p then ()
                          else let val () = if not silent_p then Util.prln("Evaluating")
                                            else ()
                                   val v = Tail.eval p Tail.Uv
                               in if silent_p then Util.prln(Tail.ppValue v)
                                  else Util.prln("Result is " ^ Tail.ppValue v)
                               end
             in if stop_after_tail_p then ()
                else let val lp = Tail2Laila.compile flags p 
                         val ocfile = Flags.flag flags "-oc"
                     in case ocfile of
                            SOME ocfile => Laila.outprog ocfile lp
                          | NONE => 
                            if print_laila_p orelse verbose_p then
                              (print "LAILA program:\n";
                               print (Laila.pp_prog lp);
                               print "\n")
                            else ()  (* program already printed! *)
                     end
             end
           | NONE => OS.Process.exit OS.Process.failure
    end handle exn as IO.Io {name,function,cause} =>
               (print ("Uncaught IO exception: Io{name=" ^ name ^ ", function=" ^ function ^ ", cause=" ^ General.exnMessage cause ^ "}\n");
                raise exn)
               

val name = CommandLine.name()

fun version() =
    String.concatWith "\n"
                      ["APLtail version: " ^ Version.version,
                       "Version date: " ^ Version.date,
                       "Platform: " ^ Version.platform]
                           
fun usage() =
    version() ^ "\n\n" ^
    "Usage: " ^ name ^ " [OPTIONS]... file.apl...\n" ^
    " -o file        : write TAIL program to file\n" ^
    " -oc file       : write LAILA program to file\n" ^
    " -c             : compile only (no evaluation)\n" ^
    " -noopt         : disable optimizations\n" ^
    " -materialize   : disable materialization of arrays\n" ^
    " -p_tail        : print TAIL program\n" ^
    " -p_types       : print types in TAIL code\n" ^
    " -p_laila       : print LAILA code\n" ^
    " -s_parse       : stop after parsing\n" ^
    " -s_tail        : stop after TAIL generation\n" ^
    " -silent        : evaluation output only (unless there are errors)\n" ^
    " -v             : verbose\n" ^
    " -O n           : optimisation level (n>0 optimises double operations aggresively)\n" ^
    " -comments      : write comments in generated C code\n" ^
    " -unsafe        : don't include assert code in generated C code for array indexing\n" ^
    " -stat_laila    : print statistics for LAILA code generation\n" ^
    " -opt_hoist     : enable hoist optimization in LAILA code generation\n" ^
    " -opt_loopsplit : enable loop split optimization in LAILA code generation\n"

(* Parse command line arguments and pass to compileAndRun *)
val () = Flags.runargs {usage = usage,
                        run = compileAndRun,
                        unaries = ["-o","-oc","-O"]}
