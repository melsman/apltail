
structure Util :> UTIL = struct

fun curry f a b = f (a,b)
fun uncurry f (a,b) = f a b

infixr $
fun f $ e = f e

fun iter f a (i,j) =
    let fun loop a n = if n > j then a
                       else loop (f (n,a)) (n+1)
    in loop a i
    end

(* Association lists *)
type (''a,'b) alist = (''a * 'b) list
fun emptyAlist () = []
fun extendAlist e (n,v) = (n,v)::e
fun lookupAlist E n =
    case List.find (fn (x,_) => x=n) E of
        SOME(_,v) => SOME v
      | NONE => NONE
fun plusAlist (a1,a2) = a2@a1

(* Printing basic values *)
fun intToCString i =
    if i = ~2147483648 then "-2147483648"
    else if i < 0 then "-" ^ intToCString (~i)
    else Int32.toString i

fun realToCString d =
    if d < 0.0 then "-" ^ realToCString (~d)
    else
      if Real.==(d,Real.posInf) then "HUGE_VAL"
      else 
        let val s = Real.toString d
            val s = String.translate (fn #"~" => "-" 
                                     | #"E" => "e" 
                                     | c => String.str c) s
        in if CharVector.exists (fn c => c = #".") s then s
           else if CharVector.exists (fn c => c = #"e") s then s
           else s ^ ".0"
        end

fun prln s = print(s ^ "\n")
fun log verbose f = if verbose then prln(f()) else ()

(* Minimum and maximum values *)
val minInt = case Int.minInt of
                 SOME i => i
               | NONE => raise Fail "Util.no minInt"
val maxInt = case Int.maxInt of
                 SOME i => i
               | NONE => raise Fail "Util.no maxInt"


(* File manipulation *)
fun readFile f =
    let val is = case f of
                    "-" => TextIO.stdIn
                  | _   => TextIO.openIn f
    in let val s = TextIO.inputAll is
       in TextIO.closeIn is;
          s
       end handle ? => (TextIO.closeIn is; raise ?)
    end

end
