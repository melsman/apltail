structure Apl2Tail = Apl2Tail(Tail)

fun die s = raise Fail ("Apl2Tail." ^ s)

fun compileAndRun (flags,files) =
    let val compile_only_p = Flags.flag_p flags "-c"
        val verbose_p = Flags.flag_p flags "-v"
        val stop_after_tail_p = Flags.flag_p flags "-s_tail"
        val print_laila_p = Flags.flag_p flags "-p_laila"
        val silent_p = Flags.flag_p flags "-silent"
        val () = Laila.unsafeAsserts := Flags.flag_p flags "-unsafe"
        val () = Laila.enableComments := Flags.flag_p flags "-comments"
        val () = Laila.statistics := Flags.flag_p flags "-stat_laila"
        val () = case Flags.flag flags "-O" of
                     NONE => ()
                   | SOME n =>
                     case Int.fromString n of
                         SOME n => Tail.optimisationLevel := n
                       | NONE => die "expecting integer as argument to -O flag"
    in if silent_p andalso verbose_p then
         Util.prln "Inconsistent use of -silent and -v flags"
       else
         case Apl2Tail.compile flags files of
             SOME p =>
             let val () = if compile_only_p then ()
                          else let val () = if not silent_p then Util.prln("Evaluating")
                                            else ()
                                   val v = Tail.eval p Tail.Uv
                               in if silent_p then Util.prln(Tail.ppV v)
                                  else Util.prln("Result is " ^ Tail.ppV v)
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
           | NONE => ()
    end
        
val name = CommandLine.name()

fun usage() =
    "Usage: " ^ name ^ " [OPTIONS]... file.apl...\n" ^
    " -o file     : write TAIL program to file\n" ^
    " -oc file    : write LAILA program to file\n" ^
    " -c          : compile only (no evaluation)\n" ^
    " -noopt      : disable optimizations\n" ^
    " -p_tail     : print TAIL program\n" ^
    " -p_types    : print types in TAIL code\n" ^
    " -p_laila    : print LAILA code\n" ^
    " -s_parse    : stop after parsing\n" ^
    " -s_tail     : stop after TAIL generation\n" ^
    " -silent     : evaluation output only (unless there are errors)\n" ^
    " -v          : verbose\n" ^
    " -O n        : optimisation level (n>0 optimises double operations aggresively)\n" ^
    " -comments   : write comments in generated C code\n" ^
    " -unsafe     : don't include assert code in generated C code for array indexing\n" ^
    " -stat_laila : print statistics for LAILA code generation\n"

val () = Flags.runargs {usage=usage,run=compileAndRun,unaries=["-o","-oc","-O"]}
