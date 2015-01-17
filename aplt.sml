structure Apl2Tail = Apl2Tail(Tail)

fun prln s = print(s ^ "\n")

fun compileAndRun (flags,files) =
    case Apl2Tail.compile flags files of
        SOME p =>
        let val compile_only_p = Flags.flag_p flags "-c"
            val verbose_p = Flags.flag_p flags "-v"
            val stop_after_tail = Flags.flag_p flags "-s_tail"
            val print_laila = Flags.flag_p flags "-p_laila"
            val () = if compile_only_p then ()
                     else let val () = prln("Evaluating")
                              val v = Tail.eval p Tail.Uv
                          in prln("Result is " ^ Tail.ppV v)
                          end
        in if stop_after_tail then ()
           else let val lp = Tail2Laila.compile flags p 
                    val ocfile = Flags.flag flags "-oc"
                    val print_laila = Flags.flag_p flags "-p_laila"
                in case ocfile of
                       SOME ocfile => Laila.outprog ocfile lp
                     | NONE => 
                       if print_laila orelse verbose_p then
                         (print "LAILA program:\n";
                          print (Laila.pp_prog lp);
                          print "\n")
                       else ()  (* program already printed! *)
                end
        end
      | NONE => ()

val name = CommandLine.name()

fun usage() =
    "Usage: " ^ name ^ " [-o ofile] [-c] [-v] [-noopt] [-p_types] file.apl...\n" ^
    "   -o file  : write TAIL program to file\n" ^
    "   -oc file : write LAILA program to file\n" ^
    "   -c       : compile only (no evaluation)\n" ^
    "   -noopt   : disable optimizations\n" ^
    "   -p_tail  : print TAIL program\n" ^
    "   -p_types : print types in TAIL code\n" ^
    "   -p_laila : print LAILA code\n" ^
    "   -s_parse : stop after parsing\n" ^
    "   -s_tail  : stop after TAIL generation\n" ^
    "   -v       : verbose\n"

val () = Flags.runargs {usage=usage,run=compileAndRun,unaries=["-o","-oc"]}
