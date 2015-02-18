(** Module for parsing commandline argument flags and invoking a general
    run function.
*)

signature FLAGS = sig
  type flags = (string * string option) list
                                        
  val flag_p : flags -> string -> bool
  val flag   : flags -> string -> string option
                                         
  val runargs : {unaries: string list,
                 usage: unit -> string, 
                 run: flags * string list -> unit} -> unit
end

(**

  [type flags] Flags are either nullary or unary.

  [flag_p flags f] returns true if f appears in flags as a nullary
  flag. Returns false, otherwise.

  [flag flags f] returns SOME v if f appears in flags as a unary flag
  with value v. Returns NONE, otherwise.

  [runargs {unaries,usage,run}] parses the commandline parameters
  given to the process. Flags are divided into unary flags (specified
  by the unaries argument), each of which take a single argument
  (consecutive sequence of non-space printable ascii characters) and
  nullary (boolean) flags that take no arguments. Flags are assumed to
  start with "-". If an empty sequence of arguments are provided to
  the process or if "-h" or "-help" is given as a flag to the process,
  the usage function is invoked and the process is
  terminated. Otherwise, if parsing of the commandline arguments
  succeeds, the runargs function is invoke with the provided flags and
  the remaining arguments (non-flags) provided on the commandline.

*)
