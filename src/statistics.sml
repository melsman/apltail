structure Statistics : STATISTICS = struct

  type stat = {name: string, enabled: bool ref, table: (string, int ref) Util.alist ref}

  fun new s r = {name=s,enabled=r,table=ref (Util.emptyAlist())}

  fun incr {name,table,enabled} s = 
      if !enabled then
        case Util.lookupAlist (!table) s of
            SOME r => r := !r + 1
          | NONE => table := Util.extendAlist (!table) (s,ref 1)
      else ()

  fun report {name,enabled,table} =
      if !enabled then
        let val sz = List.foldl (fn ((s,_),a) => (Int.max(size s,a))) 0 (!table)
            val () = Util.prln ("[" ^ name ^ " Statistics Report]")
            val () = List.app (fn (s,r) => 
                                  let val line = StringCvt.padRight #" " sz s ^ " -> " ^ Int.toString(!r)
                                  in Util.prln (" " ^ line)
                                  end) (!table)
        in ()
        end
      else ()
end
