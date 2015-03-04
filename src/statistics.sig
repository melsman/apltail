signature STATISTICS = sig
  type stat
  val new    : string -> bool ref -> stat
  val incr   : stat -> string -> unit
  val report : stat -> unit
end
