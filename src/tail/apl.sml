structure ArrayInt32 = struct
  open Array
  val length = fn x => Int32.fromInt (length x)
  val sub = fn (a,i) => sub(a,Int32.toInt i)
  val update = fn (a,i,v) => update(a,Int32.toInt i,v)
  val foldli = fn f => foldli (fn (i,a,b) => f(Int32.fromInt i,a,b))
  val tabulate = fn (n,f) => tabulate(Int32.toInt n, f o Int32.fromInt)
end

structure VectorInt32 = struct
  open Vector
  val length = fn x => Int32.fromInt (length x)
end

structure ListInt32 = struct
  open List
  val nth = fn (l,i) => nth (l,Int32.toInt i)
  val length = fn x => Int32.fromInt (length x)
  val take = fn (l,n) => take (l,Int32.toInt n)
  val drop = fn (l,n) => drop (l,Int32.toInt n)
  val tabulate = fn (n,f) => tabulate(Int32.toInt n, f o Int32.fromInt)
end

structure Apl :> APL = struct

structure V = VectorInt32
structure A = ArrayInt32
structure L = ListInt32

fun die s = raise Fail ("Apl: " ^ s)

type 'a APLArray = Int32.int vector   (* shape vector *)
          * 'a array     (* elements *)
          * 'a           (* default element *)

local
    fun list (v: 'a vector) : 'a list =
        V.foldr (op ::) nil v

    fun alist (v: 'a array) : 'a list =
        A.foldr (op ::) nil v

    fun amap f a = A.fromList(L.map f (alist a))
    fun aconcat (a1,a2) = A.fromList (alist a1 @ alist a2)

    fun pp_sh v =
        "[" ^ String.concatWith "," (L.map Int32.toString (list v)) ^ "]"

    fun product (v: Int32.int vector) : Int32.int =
        V.foldl (op * ) 1 v

in
fun scl (def:'a) (v:'a) : 'a APLArray =
    (V.fromList [], A.fromList [v], def)

fun vec v (vs: 'a list) : 'a APLArray =
    (V.fromList [Int32.fromInt(length vs)], A.fromList vs, v)

fun zilde v = vec v []

fun unScl s (v: 'a APLArray) : 'a =
    let val len = V.length (#1 v)
    in if len = 0 then A.sub(#2 v,0)
       else die ("expecting scalar argument for " ^ s ^ " - got array of rank " ^ Int32.toString len)
    end
fun liftU def f v =
    scl def (f (unScl "liftU" v))
fun liftB def f (v1,v2) =
    scl def (f (unScl "liftB:1" v1, unScl "liftB:1" v2))

fun unVec s (v: 'a APLArray) : 'a vector =
    if V.length (#1 v) = 1 then A.vector(#2 v)
    else die ("expecting vector argument for " ^ s)

fun shape (a : 'a APLArray) : Int32.int APLArray =
    let val sh = #1 a
    in (V.fromList[V.length sh], A.fromList(list sh), 0)
    end

(* resize: for reshape operation *)
fun resize (n:Int32.int) (x:'a) (v: 'a array) : 'a array =
    let val n0 = A.length v
    in A.tabulate (n,
                   if n0 = 0 then fn _ => x
                   else fn i => A.sub(v,i mod n0))
    end

fun reshape (sh:Int32.int APLArray, a : 'a APLArray) : 'a APLArray =
    let val sh = unVec "reshape" sh
        val x = #3 a
        val vs = resize (product sh) x (#2 a)
    in (sh, vs, x)
    end

fun ravel (a : 'a APLArray) : 'a APLArray =
    let val vs = #2 a
    in (V.fromList[A.length vs], vs, #3 a)
    end

fun grade (lt: 'a * 'a -> bool) (a : 'a APLArray) : Int32.int APLArray =
    let val vs : 'a list = alist (#2 a)
        fun mapi i f nil = nil
          | mapi i f (x::xs) = f (x,i)::mapi (Int32.+(i,1)) f xs
        val vs : ('a*Int32.int) list = mapi 1 (fn p => p) vs
        fun lti (x,y) = Int32.compare
        fun order (x:'a*Int32.int,y:'a*Int32.int) : order =
            if lt(#1 x,#1 y) then LESS
            else if lt(#1 y,#1 x) then GREATER
            else Int32.compare(#2 x,#2 y)
        val vs_sorted : ('a*Int32.int) list = ListSort.sort order vs
        val is = List.map #2 vs_sorted
    in (V.fromList[ListInt32.length is], A.fromList is, Int32.fromInt 0)
    end

fun gradeUp lt a = grade lt a
fun gradeDown gt a = grade gt a

fun first (a : 'a APLArray) : 'a APLArray =
    let val vs = #2 a
        val v = if A.length vs > 0 then A.sub(vs,0) else #3 a
    in scl (#3 a) v
    end

fun iota (n : Int32.int APLArray) : Int32.int APLArray =
    let val n = unScl "iota" n
    in (V.fromList[n], A.tabulate(n, fn x => x + 1), 0)
    end

fun unliftU s (def: 'a) (f : 'a APLArray -> 'b APLArray) : 'a -> 'b =
    unScl s o f o (scl def)

fun unliftB s (defa: 'a) (defb: 'b) (f : 'a APLArray * 'b APLArray -> 'c APLArray) : 'a * 'b -> 'c =
    fn (x,y) => unScl s (f (scl defa x, scl defb y))

fun each (x:'b) (f: 'a APLArray -> 'b APLArray) (a : 'a APLArray) : 'b APLArray =
    (#1 a, amap (unliftU "each" (#3 a) f) (#2 a), x)

fun power (f: 'a APLArray -> 'a APLArray) (n : Int32.int APLArray) (a : 'a APLArray) : 'a APLArray =
    let val n = unScl "power" n
    in if n < 0 then die "power: negative number of iterations not supported"
       else if n = 0 then a
       else power f (scl 0 (n-1)) (f a)
    end

fun map (def: 'b) (f: 'a -> 'b) (a : 'a APLArray) : 'b APLArray =
    (#1 a, amap f (#2 a), def)

fun reduce (f: 'a APLArray * 'a APLArray -> 'a APLArray) (n:'a APLArray) (a:'a APLArray) : 'a APLArray =
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
                        else run (j+1) (f(scl(#3 a)(A.sub(vs0,i+j)),a))
                in unScl "loop" (run 1 (scl(#3 a)(A.sub(vs0,i))))
                end
            val vs = A.tabulate(k, fn i => loop (i * m))
        in (ns, vs, #3 a)
        end

local
fun scanlChunked defaultElem (f: 'a APLArray * 'a APLArray -> 'a APLArray) chunkSize (vec : 'a array) =
  let
      fun next (i, x, y::ys) = if i mod chunkSize = 0
                                then scl defaultElem x::y::ys
                                else f(scl defaultElem x, y)::y::ys
        | next (_, x, []) = [scl defaultElem x]

      val xs : 'a APLArray list = (rev (A.foldli next [] vec))
  in
      A.fromList (L.map (unScl "scanl") xs)
  end

in
(* TODO: do we really need the neutral element? *)
fun scan (f: 'a APLArray * 'a APLArray -> 'a APLArray) ((ns,vs,def):'a APLArray) : 'a APLArray =
  case rev (list ns) of
      nil => (ns,vs,def)     (* scalar: scan is identity *)
    | [0] => zilde def       (* empty: scan is identity *)
    | m::rns => (ns, scanlChunked def f m vs, def)
end

fun replicate0 s toI (is,vs) =
    let val (sh_is,vs_is,_) = is
        val (sh_vs,vs_vs,v0) = vs
        fun toList v = A.foldr (op ::) nil v
        fun loop (0::cs,_::vs,acc) = loop(cs,vs,acc)
          | loop (c::cs,v::vs,acc) =
            if c < 0 then loop(c+1::cs,v::vs,v0::acc)
            else loop(c-1::cs,v::vs,v::acc)
          | loop (nil,nil,acc) = rev acc
          | loop _ = die "replicate length error"
    in if Vector.length sh_is <> 1 then die "replicate expects a vector as its first argument"
       else if Vector.length sh_vs <> 1 then die "replicate expects a vector as its second argument"
       else vec v0 (loop(L.map toI (toList vs_is),toList vs_vs,nil))
    end

fun replicate a = replicate0 "replicate" Int32.toInt a
fun compress a = replicate0 "compress" (fn true => 1 | false => 0) a

fun prod nil : Int32.int = 1
  | prod (x::xs) = x * prod xs

fun toSh sh (i:Int32.int) : Int32.int list =
    case sh of
        nil => nil
      | x::xs =>
        let val p = prod xs
        in i div p :: toSh xs (i mod p)
        end
fun fromSh msg sh (idx:Int32.int list) : Int32.int =
    case (sh, idx) of
        (nil, nil) => 0
      | (s::sh, i::idx) => if i >= s then die (msg ^ ": INDEX ERROR")
                           else i * prod sh + fromSh msg sh idx
      | _ => die "fromSh: dimension mismatch"

fun transpose (a: 'a APLArray) : 'a APLArray =
    let val sh = list(#1 a)
        val sh' = rev sh
        val vs = #2 a
    in (V.fromList sh',
        A.tabulate(A.length vs, fn i => A.sub(vs, fromSh "transpose" sh (rev (toSh sh' i)))),
        #3 a)
    end

fun exchange nil xs = nil
  | exchange (i::I) xs = L.nth (xs,i-1) :: exchange I xs

fun appi0 _ f nil = ()
  | appi0 (n:Int32.int) f (x::xs) = (f (x,n); appi0 (n+1) f xs)

fun appi f xs = appi0 0 f xs

fun exchange' ctrl xs =
    let val sz = L.length ctrl
        val a = A.tabulate (sz,fn _ => (0:Int32.int))
    in appi (fn (c,i) => A.update(a,c-1,L.nth(xs,i))) ctrl
     ; A.foldr(op::) nil a
    end


fun transpose2 (I: Int32.int APLArray, a: 'a APLArray) : 'a APLArray =
    let val I = alist(#2 I)
        val sh = list(#1 a)
        val () = if L.length sh <> L.length I then
                   die "transpose2: wrong index vector length"
                 else let fun check n =
                              if n = 0 then ()
                              else if L.exists (fn x => x = n) I then
                                check (n-1)
                              else die "transpose2: index vector not a permutation"
                      in check (L.length I)
                      end
        val sh' = exchange' I sh
        val vs = #2 a
    in (V.fromList sh',
        A.tabulate(A.length vs, fn i => A.sub(vs, fromSh "transpose2" sh (exchange I (toSh sh' i)))),
        #3 a)
    end

fun vcat (a1: 'a APLArray, a2: 'a APLArray) : 'a APLArray =
    case (list (#1 a1), list (#1 a2)) of
        (nil, nil) => (V.fromList[2], A.fromList[A.sub(#2 a1,0),A.sub(#2 a2,0)], #3 a1)
      | (x::xs,y::ys) =>
        if xs <> ys then die "vcat dimension mismatch"
        else let val sh = (x+y)::xs
             in (V.fromList sh, aconcat(#2 a1,#2 a2), #3 a1)
             end
      | _ => die "vcat rank mismatch"

fun catenate (a1: 'a APLArray, a2: 'a APLArray) : 'a APLArray =
    transpose(vcat(transpose a1, transpose a2))

fun ext (a: 'a APLArray) : 'a APLArray =
    (V.fromList(list(#1 a) @ [1]), #2 a, #3 a)

fun cons (v,a) = catenate (ext v, a)
fun snoc (a,v) = catenate (a, ext v)

fun zipWith (x:'c) (f: 'a APLArray * 'b APLArray -> 'c APLArray) (a : 'a APLArray) (b : 'b APLArray) : 'c APLArray =
    let val sha = #1 a
        val shb = #1 b
    in if sha <> shb then
         die ("incompatible shapes in zipWith operation: shape " ^ pp_sh sha ^ " is incompatible with " ^ pp_sh shb)
       else
         (#1 a, A.fromList(ListPair.map (unliftB "zipWith" (#3 a) (#3 b) f) (alist(#2 a),alist(#2 b))), x)
    end

fun rot (0:Int32.int) a = a
  | rot n nil = nil
  | rot n (x::xs) = rot (n-1) (xs@[x])

fun iot n = L.tabulate(n, fn i => i+1)

fun desnoc A = case rev A of
                    nil => die "desnoc"
                  | y::ys => (rev ys,y)

fun front A = #1 (desnoc A)
fun last A = #2 (desnoc A)

(*
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
*)

fun reverse (a: 'a APLArray) : 'a APLArray =
    let val sh = #1 a
    in if V.length sh > 1 then
         die "reverse: supported only for vectors and scalars"
       else (sh,A.fromList (rev (alist(#2 a))),#3 a)
    end

fun vrotate (n : Int32.int APLArray, (sh,src,default): 'a APLArray) : 'a APLArray =
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
               A.tabulate(sz, fn i => A.sub(src, (i+offset) mod sz)),
               default)
           end
    end

fun vreverse ((sh,src,default): 'a APLArray) : 'a APLArray =
    let val shl = list sh
        val sz = prod shl
    in case shl of
           nil => (sh,src,default)
         | n :: subsh =>
          let val subsz = prod subsh
          in (sh,
              A.tabulate(sz, fn i =>
                                let val y = n - (i div subsz) - 1
                                    val x = i mod subsz
                                in A.sub(src,y*subsz+x)
                                end),
              default)
          end
    end

fun rotate (i : Int32.int APLArray, a: 'a APLArray) : 'a APLArray =
    let val i = unScl "rotate" i
        val sh = #1 a
        val p = prod (list sh)
        fun find i = if i >= 0 then i
                     else find (p + i)
        val i = find i
    in if V.length sh > 1 then
         die "rotate: supported only for vectors and scalars"
       else (sh,A.fromList (rot i (alist(#2 a))),#3 a)
    end

(*
fun drop (i : int APLArray, (sh,src,default): 'a APLArray) : 'a APLArray =
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
*)

fun drop (i : Int32.int APLArray, (sh,src,default): 'a APLArray) : 'a APLArray =
    let val i = unScl "drop" i
        val x = Int32.abs i
        val sh' =
            case list sh of
                nil => nil
              | n :: subsh => Int32.max(0,n-x) :: subsh
        val sz' = prod sh'
        val offset =
            case list sh of
                nil => 0
              | _ :: subsh => Int32.max(0,i * prod subsh)
    in (V.fromList sh',
        A.tabulate(sz', fn i => A.sub(src,i+offset)),
        default)
    end

fun take (n : Int32.int APLArray, (sh,src,default): 'a APLArray) : 'a APLArray =
    let val n = unScl "take" n
        val sz = A.length src
        val sh' = case list sh of nil => [Int32.abs n]
                                | _ :: subsh => Int32.abs n :: subsh
        val sz' = prod sh'
        val offset = sz' - sz
    in (V.fromList sh',
        A.tabulate (sz', fn i => if (n < 0 andalso i < offset) then default
                                 else if (n >= 0 andalso i >= sz) then default
                                 else A.sub(src,if n<0 then i-offset else i)),
        default)
    end

fun iff (b : bool APLArray, f1,f2) =
    if unScl "iff" b then f1() else f2()

(*
fun foreach n f =
    let fun for i = if i >= n then nil
                    else f i :: for (i+1)
    in for 0
    end
*)
(*
fun idx (I : int t option list) (a: 'a t) : 'a t =
    let val sh = list (#1 a)
        val vs = alist (#2 a)
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
    in (V.fromList sh',A.fromList vs', #3 a)
    end
*)
(*
fun idxS  (d: int APLArray, n: int APLArray, a: 'a APLArray) : 'a APLArray =
    let fun tk n l = List.take(l,n)
        fun dr n l = List.drop(l,n)
        val d = unScl "idxS:d" d
        val n = unScl "idxS:n" n
        val sh = list (#1 a)
        val vs = alist (#2 a)
        fun indx di nil vs = vs
          | indx di (s::sh) vs =
            let fun extract x = indx (di-1) sh (tk (prod sh) (dr (x*prod sh) vs))
            in if di = 1 then extract (n-1)
               else if di < 1 then vs
               else List.concat(foreach s extract)
            end
        val vs' = indx d sh vs
        val sh' = tk (d-1) sh @ dr d sh
    in (V.fromList sh',A.fromList vs', #3 a)
    end
*)

fun indexFirst (n : Int32.int APLArray, a: 'a APLArray) : 'a APLArray =
    let fun tk n l = L.take(l,n)
        fun dr n l = L.drop(l,n)
    in case list (#1 a) of
           s::sh' =>
           let val n = unScl "idxS:n" n
               val () = if n < 1 orelse n > s then die "indexFirst.index error"
                        else ()
               val vs = alist (#2 a)
               val bulksz = prod sh'
               val vs' = tk bulksz (dr ((n-1)*bulksz) vs)
           in (V.fromList sh',A.fromList vs', #3 a)
           end
         | nil => die "indexFirst.assumes non-scalar array as argument"
    end

fun idxS  (d: Int32.int APLArray, n: Int32.int APLArray, a: 'a APLArray) : 'a APLArray =
    let fun tk n l = L.take(l,n)
        fun dr n l = L.drop(l,n)
        val d = unScl "idxS:d" d
        val sh = list (#1 a)
        val r = L.length sh
        val () = if d < 1 orelse d > r then die "idxS.dimension index error"
                 else ()
        val iotar = L.tabulate (r, fn i => i+1)
        val iotar = dr 1 iotar
        val I = tk (d-1) iotar @ [1] @ dr (d-1) iotar  (* squeze in a 1 in position d *)
        val a2 = transpose2 (vec 0 I, a)
    in indexFirst (n,a2)
    end

(*
fun idxS  (x: int APLArray, n: int APLArray, a: 'a APLArray) : 'a APLArray =
    let val x = unScl "idxS:x" x
        val r = length(list (#1 a))
        fun comp j = if j > r then nil
                     else (if j = x then SOME n
                           else NONE) :: comp (j+1)
    in idx (comp 1) a
    end
*)

fun idxassign (is: Int32.int APLArray, a : 'a APLArray, v : 'a) : unit =
    let val is = alist (#2 is)
        val is = L.map (fn x => x - 1) is
        val sh = list (#1 a)
        val i = fromSh "idxassign" sh is
    in A.update (#2 a, i, v)
    end

fun pr (p,sep) (a: 'a APLArray) : string =
    let fun prv sep p s e v =
            s ^ String.concatWith sep (L.map p v) ^ e
        val shape = list (#1 a)
        val values = alist (#2 a)
        fun flat () =
            (prv "," Int32.toString "[" "]" shape ^
             prv sep p "(" ")" values)
    in case shape of
           [_] => if sep="" then prv sep p "" "" values else flat()
         | [X,Y] =>
           if prod shape = 0 then flat() else
           let val sep = if sep="" then sep else " "
               val values = L.map p values
               val sz = L.foldl Int.max 0 (L.map size values)
               fun padn 0 = ""
                 | padn n = " " ^ padn (n-1)
               fun pad v = padn (sz-size v) ^ v
               fun loop [] = nil
                 | loop vs =
                   let val vs' = L.take(vs,Y)
                       val v = " " ^ String.concatWith sep (L.map pad vs')
                   in v :: loop (L.drop(vs,Y))
                   end
           in "\n" ^ String.concatWith "\n" (loop values) ^ "\n"
           end
         | _ => flat()
    end
end
end
