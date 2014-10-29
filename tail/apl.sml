structure Apl :> APL = struct

structure V = Vector

type 'a t = int vector   (* shape vector *)
          * 'a vector    (* elements *)
          * 'a           (* default element *)

fun scl (v:'a) : 'a t = 
    (V.fromList [], V.fromList [v], v)

fun vec v (vs: 'a list) : 'a t =
    (V.fromList [length vs], V.fromList vs, v)

fun zilde v = vec v []

fun unScl s (v: 'a t) : 'a =
    let val len = V.length (#1 v)
    in if len = 0 then V.sub(#2 v,0)
       else raise Fail ("expecting scalar argument for " ^ s ^ " - got array of rank " ^ Int.toString len)
    end
fun liftU f v =
    scl(f (unScl "liftU" v))
fun liftB f (v1,v2) =
    scl(f (unScl "liftB:1" v1, unScl "liftB:1" v2))

fun unVec s (v: 'a t) : 'a vector =
    if V.length (#1 v) = 1 then #2 v
    else raise Fail ("expecting vector argument for " ^ s)

fun shape (a : 'a t) : int t =
    let val sh = #1 a
    in (V.fromList[V.length sh], sh, 0)
    end

fun product (v: int vector) : int =
    V.foldl (op * ) 1 v

fun resize (n:int) (x:'a) (v: 'a vector) : 'a vector =
    let val n0 = V.length v
    in V.tabulate (n,
                   if n0 = 0 then fn _ => x
                   else fn i => V.sub(v,i mod n0))
    end

fun reshape (sh:int t, a : 'a t) : 'a t =
    let val sh = unVec "reshape" sh
        val x = #3 a
        val vs = resize (product sh) x (#2 a)
    in (sh, vs, x)
    end

fun ravel (a : 'a t) : 'a t =
    let val vs = #2 a
    in (V.fromList[V.length vs], vs, #3 a)
    end

fun first (a : 'a t) : 'a t =
    let val vs = #2 a
        val v = if V.length vs > 0 then V.sub(vs,0) else #3 a
    in scl v
    end

fun iota (n : int t) : int t =
    let val n = unScl "iota" n
    in (V.fromList[n], V.tabulate(n, fn x => x + 1), 0)
    end

fun unliftU s (f : 'a t -> 'b t) : 'a -> 'b =
    unScl s o f o scl

fun unliftB s (f : 'a t * 'b t -> 'c t) : 'a * 'b -> 'c =
    fn (x,y) => unScl s (f (scl x, scl y))

fun each (x:'b) (f: 'a t -> 'b t) (a : 'a t) : 'b t =
    (#1 a, V.map (unliftU "each" f) (#2 a), x)

fun map (f: 'a -> 'b) (a : 'a t) : 'b t =
    (#1 a, V.map f (#2 a), f (#3 a))
    
fun list (v: 'a vector) : 'a list = V.foldr (op ::) nil v
fun reduce (f: 'a t * 'a t -> 'a t) (n:'a t) (a:'a t) : 'a t =
    case rev (list (#1 a)) of
        nil => a  (* scalar value: reduce is the identity! *)
      | [0] => n  (* empty vector: return the neutral element *)
      | m::rns => 
        let val ns = V.fromList (rev rns)
            val k = product ns
            val vs0 = #2 a
            fun loop i =
                let fun run j a =
                        if j >= m then a
                        else run (j+1) (f(scl(V.sub(vs0,i+j)),a))
                in unScl "loop" (run 1 (scl(V.sub(vs0,i))))
                end
            val vs = V.tabulate(k, fn i => loop (i*m))
        in (ns, vs, #3 a)
        end
            
fun scan _ = raise Fail "scan not implemented"

fun replicate0 s toI (is,vs) =
    let val (sh_is,vs_is,_) = is
        val (sh_vs,vs_vs,v0) = vs
        fun toList v = Vector.foldr (op ::) nil v
        fun loop (0::cs,_::vs,acc) = loop(cs,vs,acc)
          | loop (c::cs,v::vs,acc) =
            if c < 0 then loop(c+1::cs,v::vs,v0::acc)
            else loop(c-1::cs,v::vs,v::acc)
          | loop (nil,nil,acc) = rev acc
          | loop _ = raise Fail "replicate length error"
    in if Vector.length sh_is <> 1 then raise Fail "replicate expects a vector as its first argument"
       else if Vector.length sh_vs <> 1 then raise Fail "replicate expects a vector as its second argument"
       else vec v0 (loop(List.map toI (toList vs_is),toList vs_vs,nil))
    end

fun replicate a = replicate0 "replicate" (fn x => x) a
fun compress a = replicate0 "compress" (fn true => 1 | false => 0) a

fun prod nil = 1
  | prod (x::xs) = x * prod xs

fun toSh sh (i:int) : int list =
    case sh of
        nil => nil
      | x::xs => 
        let val p = prod xs
        in i div p :: toSh xs (i mod p)
        end
fun fromSh sh (idx:int list) : int =
    case (sh, idx) of
        (nil, nil) => 0
      | (x::sh, i::idx) => i * prod sh + fromSh sh idx
      | _ => raise Fail "fromIdx: dimension mismatch"

fun transpose (a: 'a t) : 'a t =
    let val sh = list(#1 a)
        val sh' = rev sh
        val vs = #2 a
    in (V.fromList sh',
        V.tabulate(V.length vs, fn i => V.sub(vs, fromSh sh (rev (toSh sh' i)))),
        #3 a)
    end

fun exchange nil xs = nil
  | exchange (i::I) xs = List.nth (xs,i-1) :: exchange I xs

(*
fun exchange I xs =
    let fun loop nil = nil
          | loop (i::I) =
            List.nth (xs,i-1)::loop I
    in loop I
    end
*)

fun transpose2 (I: int t, a: 'a t) : 'a t =
    let val I = list(#2 I)
        val sh = list(#1 a)
        val () = if length sh <> length I then
                   raise Fail "transpose2: wrong index vector length"
                 else let fun check n =
                              if n = 0 then ()
                              else if List.exists (fn x => x = n) I then
                                check (n-1)
                              else raise Fail "transpose2: index vector not a permutation"
                      in check (length I)
                      end
        val sh' = exchange I sh
        val vs = #2 a
    in (V.fromList sh',
        V.tabulate(V.length vs, fn i => V.sub(vs, fromSh sh (exchange I (toSh sh' i)))),
        #3 a)
    end

fun vcat (a1: 'a t, a2: 'a t) : 'a t =
    case (list (#1 a1), list (#1 a2)) of
        (nil, nil) => (V.fromList[2], V.fromList[V.sub(#2 a1,0),V.sub(#2 a2,0)], #3 a1)
      | (x::xs,y::ys) =>
        if xs <> ys then raise Fail "vcat dimension mismatch"
        else let val sh = (x+y)::xs
             in (V.fromList sh, V.concat[#2 a1,#2 a2], #3 a1)
             end
      | _ => raise Fail "vcat rank mismatch"

fun catenate (a1: 'a t, a2: 'a t) : 'a t =
    transpose(vcat(transpose a1, transpose a2))

fun ext (a: 'a t) : 'a t =
    (V.fromList(list(#1 a) @ [1]), #2 a, #3 a)

fun cons (v,a) = catenate (ext v, a)
fun snoc (a,v) = catenate (a, ext v)

fun zipWith (x:'c) (f: 'a t * 'b t -> 'c t) (a : 'a t) (b : 'b t) : 'c t =
    (#1 a, V.fromList(ListPair.map (unliftB "zipWith" f) (list(#2 a),list(#2 b))), x)

fun rot 0 a = a
  | rot n nil = nil
  | rot n (x::xs) = rot (n-1) (xs@[x])
    
fun iot n = List.tabulate(n, fn i => i+1)
        
fun desnoc A = case rev A of
                    nil => raise Fail "desnoc"
                  | y::ys => (rev ys,y)

fun front A = #1 (desnoc A)
fun last A = #2 (desnoc A)

fun dot f g n A B =
    let val (shA,_,_) = A
        val (shB,_,_) = B
        val (shA,shB) = (list shA,list shB)
        val WA = tl shB @ shA
        val KA = length shA - 1
        val VA = iot (length WA)
        val ZA = rot KA (front VA) @ [last VA]
        val TA = transpose2(vec 0 ZA,reshape(vec 0 WA,A))
        val WB = front shA @ shB
        val KB = length shA
        val VB = iot (length WB)
        val shVB = length VB
        val ZB = iot (KB-1) @ List.map (fn x => KB+x) (iot (shVB-KB)) @ [KB]
        val TB = transpose2(vec 0 ZB,reshape(vec 0 WB,B))
        val R1 = zipWith (unScl "dot" n) g TA TB
    in reduce f n R1
    end

fun reverse (a: 'a t) : 'a t =
    let val sh = #1 a    
    in if V.length sh > 1 then
         raise Fail "apl.reverse: supported only for vectors and scalars"
       else (sh,V.fromList (rev (list(#2 a))),#3 a)
    end

fun rotate (i : int t, a: 'a t) : 'a t =
    let val i = unScl "rotate" i
        val sh = #1 a
        val p = prod (list sh)
        fun find i = if i > 0 then i
                     else find (p + i)
        val i = find i
    in if V.length sh > 1 then
         raise Fail "apl.rotate: supported only for vectors and scalars"
       else (sh,V.fromList (rot i (list(#2 a))),#3 a)
    end

structure Vs = VectorSlice
fun drop (i : int t, a: 'a t) : 'a t =
    let val i = unScl "drop" i
        val sh = list(#1 a)
    in if i < 0 then take (scl(hd sh + i),a)
       else case sh of
                nil => a
              | [0] => a
              | _ => 
                let val d = i * prod(tl sh)
                    val t = hd sh - i
                in if t < 1 then zilde (#3 a)
                   else let val sh' = t :: tl sh
                        in (V.fromList sh', Vs.vector(Vs.slice(#2 a, d, NONE)), #3 a)
                        end
                end
    end
and take (i : int t, a: 'a t) : 'a t =
    let val i = unScl "take" i
        val sh = list(#1 a)
    in if i < 0 then drop (scl(hd sh + i),a)
       else case sh of
                nil => let val v = V.sub(#2 a,0)
                       in (V.fromList[i],V.tabulate(i,fn _ => v),#3 a)
                       end
              | [0] => (V.fromList[i],V.tabulate(i,fn _ => #3 a),#3 a)
              | _ => 
                let val sh' = i :: tl sh
                    val sz = prod sh'
                in (V.fromList sh', resize sz (#3 a) (#2 a), #3 a)
                end
    end

fun iff (b : bool t, f1,f2) =
    if unScl "iff" b then f1() else f2()

fun pr p (a: 'a t) : string =
    let fun prv p s e v =
            s ^ String.concatWith "," (List.map p (list v)) ^ e
    in prv Int.toString "[" "]" (#1 a) ^
       prv p "(" ")" (#2 a)
    end
       
end
