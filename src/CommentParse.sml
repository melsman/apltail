(* 

Examples
--------

  ⍝○ x : [int]0
  x ←5

  ⍝○ a : <int>42
  a ←⍳42

  ⍝○ f : [int]1 →[int]0
  f ←{⊃⍵}

  ⍝○ y : SV(int,17)
  y ← [17]

  ⍝○ z : S(int,13)
  z ← 13

  ⍝ or a polymorph type:

  ⍝○ g : [⍺]1 →[⍺]0
  g ←{⊃⍵}



 Comment [Circ ' ' Letter(x) ' ' Colon ' ' Lsqbra Letter(i) Letter(n) Letter(t) Rsqbra Digit(0)] Newline
 Id(x) Larrow 5 Newline Newline

 Comment [Circ ' ' Letter(a) ' ' Colon ' ' Lt Letter(i) Letter(n) Letter(t) Gt Digit(4) Digit(2)] Newline
 Id(a) Larrow Iota 42 Newline Newline

 Comment [Circ ' ' Letter(f) ' ' Colon ' ' Lsqbra Letter(i) Letter(n) Letter(t) Rsqbra Digit(1) ' ' Rarrow Lsqbra Letter(i) Letter(n) Letter(t) Rsqbra Digit(0)] Newline
 Id(f) Larrow Lbra Disclose Omega Rbra Newline Newline

 Comment [' ' Letter(e) Letter(l) Letter(l) Letter(e) Letter(r) ' ' Letter(p) Letter(o) Letter(l) Letter(y) Letter(m) Letter(o) Letter(r) Letter(f) Letter(t) Colon] Newline Newline

 Comment [Circ ' ' Letter(g) ' ' Colon ' ' Lsqbra Alpha Rsqbra Digit(1) ' ' Rarrow Lsqbra Alpha Rsqbra Digit(0)] Newline
 Id(g) Larrow Lbra Disclose Omega Rbra Newline Newline

 Comment [Circ ' ' Letter(y) ' ' Colon ' ' Letter(S) Letter(V) Lpar Letter(i) Letter(n) Letter(t) Cat Digit(1) Digit(7) Rpar] Newline
 Id(y) Larrow 17 Newline Newline

 Comment [Circ ' ' Letter(z) ' ' Colon ' ' Letter(S) Lpar Letter(i) Letter(n) Letter(t) Cat Digit(1) Digit(3) Rpar] Newline
 Id(z) Larrow 13 Newline

*)

functor CommentParse (T : TAIL) =
struct
structure L = AplAst.L
structure TT = T.Exp.T

(* Comment Lex *)
datatype comment_token = Colon
                       | Comma
                       | LParen
                       | RParen
                       | LBracket
                       | RBracket
                       | LAngle
                       | RAngle
                       | RArrow
                       | IdT of string (* L.token list *)
                       | SingletonVec
                       | Singleton
                       | Integer of int

fun pp_comment_token x =
   case x of
      Colon        => ":"
    | Comma        => ","
    | LParen       => "("
    | RParen       => ")"
    | LBracket     => "["
    | RBracket     => "]"
    | LAngle       => "<"
    | RAngle       => ">"
    | RArrow       => "->"
    | IdT s       => s (* AplLex.pp_tokens ts *)
    | SingletonVec => "SV"
    | Singleton    => "S"
    | Integer d    => Int.toString d

(* fun splitAtWhiteSpace ws = *)
(*       let fun split ([], [], r) = rev r *)
(*             | split ([], c, r) = L.Chars (rev c) :: r *)
(*             | split (x :: xs, [], r) = if AplLex.isWhiteSpace x *)
(*                                        then split (xs, [], r) *)
(*                                        else split (xs, [x], r) *)
(*             | split (x :: xs, c, r)  = if AplLex.isWhiteSpace x *)
(*                                        then split (xs, [], (L.Chars (rev c) :: r)) *)
(*                                        else split (xs, [x], r) *)
(*       in *)
(*           split (ws, [], []) *)
(*       end *)

local

(* TODO: Handle whitespace, to get regions right *)
fun lex []                    _   = []
  | lex (L.Letter #"S" :: ts) loc = lexSingleton ts (loc, loc)
  | lex (L.Letter x    :: ts) loc = lexId ts (loc, loc) [x]
  | lex (L.Digit x     :: ts) loc = lexInt ts (loc, loc) [x]
  | lex (L.Colon       :: ts) loc = (Colon,    (loc, loc)) :: lex ts (Region.next loc)
  | lex (L.Cat         :: ts) loc = (Comma,    (loc, loc)) :: lex ts (Region.next loc)
  | lex (L.Lpar        :: ts) loc = (LParen,   (loc, loc)) :: lex ts (Region.next loc)
  | lex (L.Rpar        :: ts) loc = (RParen,   (loc, loc)) :: lex ts (Region.next loc)
  | lex (L.Lsqbra      :: ts) loc = (LBracket, (loc, loc)) :: lex ts (Region.next loc)
  | lex (L.Rsqbra      :: ts) loc = (RBracket, (loc, loc)) :: lex ts (Region.next loc)
  | lex (L.Lt          :: ts) loc = (LAngle,   (loc, loc)) :: lex ts (Region.next loc)
  | lex (L.Gt          :: ts) loc = (RAngle,   (loc, loc)) :: lex ts (Region.next loc)
  | lex (L.Rarrow      :: ts) loc = (RArrow,   (loc, loc)) :: lex ts (Region.next loc)
  | lex (L.Chars ws    :: ts) (line,ch,file) = lex ts (line, ch+length ws, file)
  | lex (t :: _)              _   = raise Fail ("unexpected token "
                                                ^ L.pr_token t
                                                ^ " when lexing annotation")

and lexSingleton (L.Letter #"V" :: ts) (l0,l1)     = lexSingletonVec ts (l0, Region.next l1)
  | lexSingleton (L.Letter x :: ts)    r           = lexId ts r [x, #"S"]
  | lexSingleton (L.Digit x  :: ts)    r           = lexId ts r [x, #"S"]
  | lexSingleton ts                    (r as (l0,l1)) = (Singleton, r) :: lex ts (Region.next l1)

and lexSingletonVec (L.Letter x :: ts) r           = lexId ts r [x, #"V", #"S"]
  | lexSingletonVec (L.Digit x  :: ts) r           = lexId ts r [x, #"V", #"S"]
  | lexSingletonVec ts                 (r as (l0,l1)) = (SingletonVec, r) :: lex ts (Region.next l1)

and lexId (L.Letter x :: ts) (l0,l1) s = lexId ts (l0, Region.next l1) (x :: s)
  | lexId (L.Digit  x :: ts) (l0,l1) s = lexId ts (l0, Region.next l1) (x :: s)
  | lexId ts (r as (l0,l1)) s          = (IdT (implode (rev s)), r) :: lex ts (Region.next l1)

and lexInt (L.Digit x :: ts) (l0,l1) s        = lexInt ts (l0, Region.next l1) (x :: s)
  | lexInt ts                (r as (l0,l1)) s = case Int.fromString (implode (rev s)) of
                                               NONE => raise Fail "this should not happen"
                                             | SOME d => (Integer d, r) :: lex ts (Region.next l1)

in
(* commentLex : token list * Region.reg -> (comment_token * Region.reg) list *)
fun commentLex (L.Circ :: ts) (r as (l0,l1)) = SOME (lex ts l0)
  | commentLex ts r = NONE
end


structure PComb = ParseComb(type token=comment_token
                            val pr_token = pp_comment_token)
open PComb infix >>> ->> >>- ?? ??? || oo oor

type annotation = string * TT.typ

fun printAnnotation (str, ty) = str ^ " : " ^ TT.prType ty

(* Currently we parse the following BNF, but support for type
  variables should be added:

  <annotation> ::= CIRC <annoexp>
  <annoexp>    ::= ID COLON <type>
  <rank>       ::= INTEGER
  <basetype>   ::= "int" | "double" | "bool" | "char"
  <type_t> ::= "[" <basetype> "]" <rank>
            | "<" <basetype> ">" <rank>
            | "S(" <basetype> "," <rank> ")"
            | "SV(" <basetype> "," <rank> ")"
            | <basetype>
  <type> ::= <type_t> <arrow>
  <arrow> ::= RARROW <type_t> [<arrow>]
 *)

(* 
  Complication
  ------------
  The type "a -> a"

  can be parsed with either representing a typevariable for basetypes
  (\kappa) or representing an ordinary typevariable (\alpha). When
  possible, we will parse this as ordinary typevariable (most
  general). (Still TODO!)
 *)

(* p_id : string p *)
fun p_id nil              = NO (Region.botloc,fn () => "expecting identifier but found end-of-file")
  | p_id ((IdT id,r)::ts) = OK(id,r,ts)
  | p_id ((t,r)::_)       = NO (#1 r,fn() => ("expecting identifier but found token " ^ pp_comment_token t))

(* p_int : int p *)
fun p_int nil                 = NO (Region.botloc,fn () => "expecting integer but found end-of-file")
  | p_int ((Integer i,r)::ts) = OK(i,r,ts)
  | p_int ((t,r)::_)          = NO (#1 r, fn() => ("expecting integer but found token " ^ pp_comment_token t))

(* Parse basetype (these occur as identifiers in token list from the lexer) *)
fun p_basetype nil                    = NO (Region.botloc,fn () => "expecting basetype but found end-of-file")
  | p_basetype ((IdT "int",r)::ts)    = OK(TT.IntB,r,ts)
  | p_basetype ((IdT "double",r)::ts) = OK(TT.DoubleB,r,ts)
  | p_basetype ((IdT "bool",r)::ts)   = OK(TT.BoolB,r,ts)
  | p_basetype ((IdT "char",r)::ts)   = OK(TT.CharB,r,ts)
  | p_basetype ((t,r)::_)             = NO (#1 r,fn() => ("expecting basetype (int,double,bool,char) but found token " ^ pp_comment_token t))

(* Parse rank expression - currently rank variables aren't supported *)
fun p_rank ts = ((p_int oo TT.rnk)) ts
                 (* || (p_id oo TT.RnkVar)) *) 

(* fun p_annotation ts = (eat Circle ->> p_annoexp) ts *)

fun p_annotation ts = (p_id >>> (eat Colon ->> p_type)) ts

and p_type_t ts = ((((eat LBracket ->> p_basetype >>- eat RBracket) >>> p_rank) oo (Util.uncurry TT.Arr))
                  || (((eat LAngle ->> p_basetype >>- eat RAngle) >>> p_rank) oo (Util.uncurry TT.Vcc))
                  || p_singletonvec
                  || p_singleton
                  (* || (p_id oo TyVar) *)
                  || (p_basetype oo (fn a => TT.Arr a (TT.rnk 0)))) ts

and p_type ts = ((p_type_t ?? p_arrow) TT.Fun) ts

and p_arrow ts = ((eat RArrow ->> p_type_t ?? p_arrow) TT.Fun) ts

and p_singleton ts = ((eat Singleton ->> eat LParen ->> p_basetype >>- eat Comma >>> p_rank >>- eat RParen) oo (Util.uncurry TT.S)) ts
and p_singletonvec ts = ((eat SingletonVec ->>
                          eat LParen ->>
                          p_basetype >>-
                          eat Comma >>>
                          p_rank >>-
                          eat RParen) oo (Util.uncurry TT.SV)) ts

fun parseAnnotation tokens (l0,l1) = 
  (case commentLex tokens (Region.next l0,l1) of (* next to skip Nabla *)
      SOME ts => SOME (p_annotation ts)
    | NONE    => NONE)

fun parseAndPrintAnnotation tokens r =
 case parseAnnotation tokens r of
    SOME (OK (x, r, rest)) => "Annotation parsed OK: " ^ printAnnotation x
  | SOME (NO (_, e))       => "Annotation could not be parsed: " ^ e ()
  | NONE                   => "Not an annotation: " ^ AplLex.pp_tokens tokens

end
