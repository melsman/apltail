structure Apl :> APL = struct

structure V = Vector

type 'a t = int vector   (* shape vector *)
          * 'a vector    (* elements *)
          * 'a           (* default element *)

fun list (v: 'a vector) : 'a list = 
    V.foldr (op ::) nil v

fun pp_sh v =
    "[" ^ String.concatWith "," (List.map Int.toString (list v)) ^ "]"

fun scl (def:'a) (v:'a) : 'a t = 
    (V.fromList [], V.fromList [v], def)

fun vec v (vs: 'a list) : 'a t =
    (V.fromList [length vs], V.fromList vs, v)

fun zilde v = vec v []

fun unScl s (v: 'a t) : 'a =
    let val len = V.length (#1 v)
    in if len = 0 then V.sub(#2 v,0)
       else raise Fail ("expecting scalar argument for " ^ s ^ " - got array of rank " ^ Int.toString len)
    end
fun liftU def f v =
    scl def (f (unScl "liftU" v))
fun liftB def f (v1,v2) =
    scl def (f (unScl "liftB:1" v1, unScl "liftB:1" v2))

fun unVec s (v: 'a t) : 'a vector =
    if V.length (#1 v) = 1 then #2 v
    else raise Fail ("expecting vector argument for " ^ s)

fun shape (a : 'a t) : int t =
    let val sh = #1 a
    in (V.fromList[V.length sh], sh, 0)
    end

fun product (v: int vector) : int =
    V.foldl (op * ) 1 v

(* resize: for reshape operation *)
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
    in scl (#3 a) v
    end

fun iota (n : int t) : int t =
    let val n = unScl "iota" n
    in (V.fromList[n], V.tabulate(n, fn x => x + 1), 0)
    end

fun unliftU s (def: 'a) (f : 'a t -> 'b t) : 'a -> 'b =
    unScl s o f o (scl def)

fun unliftB s (defa: 'a) (defb: 'b) (f : 'a t * 'b t -> 'c t) : 'a * 'b -> 'c =
    fn (x,y) => unScl s (f (scl defa x, scl defb y))

fun each (x:'b) (f: 'a t -> 'b t) (a : 'a t) : 'b t =
    (#1 a, V.map (unliftU "each" (#3 a) f) (#2 a), x)

fun power (f: 'a t -> 'a t) (n : int t) (a : 'a t) : 'a t =
    let val n = unScl "power" n
    in if n < 0 then raise Fail "power: negative number of iterations not supported"
       else if n = 0 then a
       else power f (scl 0 (n-1)) (f a)
    end
                
fun map (def: 'b) (f: 'a -> 'b) (a : 'a t) : 'b t =
    (#1 a, V.map f (#2 a), def)
    
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
                        else run (j+1) (f(scl(#3 a)(V.sub(vs0,i+j)),a))
                in unScl "loop" (run 1 (scl(#3 a)(V.sub(vs0,i))))
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

fun appi0 _ f nil = ()
  | appi0 n f (x::xs) = (f (x,n); appi0 (n+1) f xs)
  
fun appi f xs = appi0 0 f xs

fun exchange' ctrl xs =
    let val sz = length ctrl
        val a = Array.tabulate (sz,fn _ => 0)
    in appi (fn (c,i) => Array.update(a,c-1,List.nth(xs,i))) ctrl
     ; Array.foldr(op::) nil a
    end

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
        val sh' = exchange' I sh
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
    let val sha = #1 a
        val shb = #1 b
    in if sha <> shb then
         raise Fail ("incompatible shapes in zipWith operation: shape " ^ pp_sh sha ^ " is incompatible with " ^ pp_sh shb)
       else 
         (#1 a, V.fromList(ListPair.map (unliftB "zipWith" (#3 a) (#3 b) f) (list(#2 a),list(#2 b))), x)
    end

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

fun vrotate (n : int t, (sh,src,default): 'a t) : 'a t =
    let val n = unScl "vrotate" n
        val shl = list sh
    in case shl of
           nil => (sh,src,default) (* scalar *) 
         | 0::_ => (sh,src,default) (* empty vector *)
         | s::sh' =>
           let val sz = prod shl
               val n = n mod s
               val n = if n > 0 then n else s - n 
               val offset = n * prod sh'
           in (sh, 
               V.tabulate(sz, fn i => V.sub(src, (i+offset) mod sz)),
               default)
           end
    end

fun vreverse ((sh,src,default): 'a t) : 'a t =
    let val shl = list sh
        val sz = prod shl
    in case shl of
           nil => (sh,src,default)
         | n :: subsh =>
          let val subsz = prod subsh
          in (sh,
              V.tabulate(sz, fn i => 
                                let val y = n - (i div subsz) - 1
                                    val x = i mod subsz
                                in V.sub(src,y*subsz+x)
                                end),
              default)
          end
    end

fun rotate (i : int t, a: 'a t) : 'a t =
    let val i = unScl "rotate" i
        val sh = #1 a
        val p = prod (list sh)
        fun find i = if i >= 0 then i
                     else find (p + i)
        val i = find i
    in if V.length sh > 1 then
         raise Fail "apl.rotate: supported only for vectors and scalars"
       else (sh,V.fromList (rot i (list(#2 a))),#3 a)
    end

fun drop (i : int t, (sh,src,default): 'a t) : 'a t =
    let val i = unScl "drop" i
        val x = Int.abs i
    in case list sh of
           nil => (sh,src,default)
         | n::subsh =>
           let val subsz = prod subsh
               val sh' = Int.max(0,n-x) :: subsh
               val sz = prod sh'
               val offset = Int.max(0,i * subsz)
           in (V.fromList sh',
               V.tabulate(sz, fn i => V.sub(src,i+offset)),
               default)
           end
    end

fun take (n : int t, (sh,src,default): 'a t) : 'a t =
    let val n = unScl "take" n
    in if n < 0 then
         let val (sh',subsz,b) = 
                 case list sh of nil => ([~n], 1, 1)
                               | b :: sh => (~n :: sh, prod sh, b)
             val offset = (~n - b)*subsz
         in (V.fromList sh',
             V.tabulate (prod sh', fn i => if i < offset then default
                                           else V.sub(src,i-offset)),
             default)
         end
       else
         let val sh' = case list sh of nil => [n]
                                     | _ :: sh => n :: sh
             val sz = prod sh'
             val srcsz = V.length src
         in (V.fromList sh', 
             V.tabulate (sz, fn i => if i >= srcsz then default
                                     else V.sub(src,i)),
             default)
         end
    end

fun iff (b : bool t, f1,f2) =
    if unScl "iff" b then f1() else f2()

fun foreach n f = 
    let fun for i = if i >= n then nil
                    else f i :: for (i+1)
    in for 0
    end

fun idx (I : int t option list) (a: 'a t) : 'a t =
    let val sh = list (#1 a)
        val vs = list (#2 a)
        fun tk n l = List.take(l,n)
        fun dr n l = List.drop(l,n)
        fun indx I sh vs acc =
            case (I,sh) of
                (nil,nil) => vs @ acc
              | (NONE::I, s::sh) =>
                let val vss =
                        foreach s (fn i => indx I sh
                                                (tk (prod sh) (dr (i*prod sh) vs)) [])
                in List.concat vss @ acc
                end
              | (SOME n::I, s::sh) =>
                let val n = unScl "idx:n" n
                in indx I sh (tk (prod sh) (dr ((n-1)*prod sh) vs)) acc
                end
              | _ => raise Fail "idx.indx: indexes do not match array rank"
        val vs' = indx I sh vs []
        fun compShape nil nil = nil
          | compShape (NONE::I) (s::sh) = s::compShape I sh
          | compShape (SOME _::I) (s::sh) = compShape I sh
          | compShape _ _ = raise Fail "idx.compShape: indexes do not match array rank"
        val sh' = compShape I sh
    in (V.fromList sh',V.fromList vs', #3 a)
    end

fun idxS  (x: int t, n: int t, a: 'a t) : 'a t =
    let val x = unScl "idxS:x" x
        val r = length(list (#1 a))
        fun comp j = if j > r then nil
                     else (if j = x then SOME n
                           else NONE) :: comp (j+1)
    in idx (comp 1) a
    end

fun pr (p,sep) (a: 'a t) : string =
    let fun prv sep p s e v =
            s ^ String.concatWith sep (List.map p v) ^ e
        val shape = list (#1 a)
        val values = list (#2 a)
        fun flat () =
            (prv "," Int.toString "[" "]" shape ^
             prv sep p "(" ")" values)
    in case shape of
           [_] => if sep="" then prv sep p "" "" values else flat()
         | [X,Y] => 
           if prod shape = 0 then flat() else
           let val sep = if sep="" then sep else " "
               val values = List.map p values
               val sz = List.foldl Int.max 0 (List.map size values)
               fun padn 0 = ""
                 | padn n = " " ^ padn (n-1)
               fun pad v = padn (sz-size v) ^ v
               fun loop [] = nil
                 | loop vs =
                   let val vs' = List.take(vs,Y)
                       val v = " " ^ String.concatWith sep (List.map pad vs')
                   in v :: loop (List.drop(vs,Y))
                   end
           in "\n" ^ String.concatWith "\n" (loop values) ^ "\n"
           end
         | _ => flat()
    end
       
end
