structure Apl2Tail = Apl2Tail(Tail)

val name = CommandLine.name()

fun usage() =
    (print ("Usage: " ^ name ^ " [-o ofile] [-c] [-v] [-noopt] [-p_types] file.apl...\n" ^
            "   -o file : specify output file\n" ^
            "   -c : compile only (no evaluation)\n" ^
            "   -noopt : disable optimizations\n" ^
            "   -p_types : print types in TAIL code\n" ^
            "   -v : verbose\n");
     OS.Process.exit OS.Process.success)

fun isFlag s =
    case String.explode s of
        #"-" :: _ => true
      | _ => false

fun runargs args flags =
    case args of
        "-o" :: ofile :: rest => runargs rest (("-o",SOME ofile)::flags)
      | f :: rest => 
        if isFlag f then runargs rest ((f,NONE)::flags)
        else Apl2Tail.compileAndRunFiles flags args
      | nil => usage ()

val () = runargs (CommandLine.arguments()) nil
