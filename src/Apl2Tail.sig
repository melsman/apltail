signature APL2TAIL = sig

  structure T : TAIL

  (* supported flags: [-o f, -tl, -c, -v, -noopt, -p_types, -s_tail, -s_parse] *)
  type flags = {verbose : bool, optlevel : int, prtype : bool}

  (* APL extensions. The parser needs this to disambiguate whether the
     functions are dyadic/monadic or both *)
  val initialParseEnv : AplParse.env

  (* Compile the APL-expression into a TAIL-expression *)
  val compile : flags -> AplAst.exp -> (unit, T.Double T.Num) T.prog
end
