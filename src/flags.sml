structure Flags :> FLAGS = 
struct

type flags = (string * string option) list

fun flag_p flags s =
    List.exists (fn p => p = (s,NONE)) flags

fun flag flags s =
    case List.find (fn (s',_) => s' = s) flags of
        SOME (_,SOME v) => SOME v
      | _ => NONE

fun isFlag s =
    case String.explode s of
        #"-" :: _ :: _ => true
      | _ => false

fun runargs {unaries: string list,
             usage : unit -> string,
             run: flags * string list -> unit} : unit =
    let fun loop args acc : unit =
            case args of
                opt :: arg :: rest =>
                if List.exists (fn x => x = opt) unaries then
                  loop rest ((opt,SOME arg)::acc)
                else if isFlag opt then loop (arg::rest) ((opt,NONE)::acc)
                else run(rev acc, args) 
              | f :: rest => 
                if isFlag f then loop rest ((f,NONE)::acc)
                else run(rev acc, args)
              | nil => (print(usage () ^ "\n"); OS.Process.exit OS.Process.success)
    in loop (CommandLine.arguments()) nil
    end

end
