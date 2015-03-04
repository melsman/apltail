signature ILUTIL = sig
  type e
  type ss
  type Env
  type value = IL.value

  exception Halted of string  (* evaluation may Halt *) 

  val emptyEnv    : Env
  val add         : Env -> Name.t * value -> Env
  val lookup      : Env -> Name.t -> value option
  val eval        : Env -> e -> value
  val evalSS      : Env -> ss -> Name.t -> Env

  val ppSS        : int -> ss -> string  (* int is indent level *)
  val ppExp       : e -> string
  val ppFunction  : string -> Type.T * Type.T -> Name.t -> ss -> string
  val ppValue     : value -> string
  val typeExp     : e -> Type.T

  val cse         : ss -> ss
  val hoist       : ss -> ss
end
