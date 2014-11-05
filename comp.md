## Compiling APL

Source language

    e ::= i | e1;e2 | e1+e2 | x<-e | x | lam e | x(e)  

Dynamically typed __terms__:

```sml
  type mi = Int Num m   (* Multidimensional integer array *)

  datatype s =          (* Terms *)
      Is of INT         (*   integer *)
    | Ais of mi         (*   integer array *)
    | Fs of s -> s M    (*   function in-lining *)
```

Environments (G) map identifiers (variables and symbols) to terms:

    G \in ENV = ID -> s

Compilation:

    [ _ ] _ _ : AST -> ENV -> (s * ENV -> s M) -> s M

    [i] G k = k (Is(I i),{})

    [e1; e2] G k =
      [e1] G (fn (s1,G1) =>
      [e2] (G1@G) (fn (s2,G2) =>
      k (s2,G2@G1)))

    [e1 + e2] G k =
      [e2] G (fn (s2,G2) =>
      [e1] (G2@G) (fn (s1,G1) =>
      case (s1, s2) of
           (Is i1, Is i2) => k(Is(i1+i2),G2@G1)
         | (Ais a1, Ais a2) => mmap2 (op +) a1 a2 >>= (fn x => k(Ais x,G2@G1))
         | (Ais a1, Is i2) => k(Ais(mmap(fn x => x+i2)a1),G2@G1)
         | (Is i1, Ais a2) => k(Ais(mmap(fn x => i1+x)a2),G2@G1)
         | _ => err))

    [v <- e] G k =
       [e] G (fn (s,_) => k(s,{v->s}))

    [a] G k = k (G(a),{})

    [lam e] G k =
      let f x =
        let G' = {w -> x}
        in [e] (G'@G) (fn (s,_) => ret s)
      in k(Fs f,{})
      end

    [a(e)] G k =
      case G(a) of
         Fs f =>
           [e] G (fn (s,G') =>
           f s >>= (fn s' => k(s',G')))
       | _ => err

## Compiling into TAIL (simplified)

    opr ::= reduce[1,2] | map[1,1] | transpose[0,1] | out[2,2] 
          | add[0,2] | mul[0,2] | max[0,2] | min[0,2]

    a ::= <i1,...,in>

    e ::= a | x | let x = e in e | fun f (x1,...,xn) = e in e 
        | f(e1,...,en) | opr [f1,...,fn] (e1,...,em)

    [ _ ] _ : AST -> E -> (ML -> E -> ML) -> ML

    [x] E k = k E x

    [a] E k = k E a

    [x<-e] E k = let x = [e] E (fn _ x => x) in k (E+{x->x}) x

    [e1;e2] E k = [e1] E (fn E x => [e2] E k)

    [e1+e2] k = [e2] E (fn E x => [e1] E (fn E y => k E (add[](y,x))))

    [lam e] E k = 
      fun f (w) = [e] (E+{w->w}) (fn _ x => x)
      in k E f

    [x(e)] E k = [e] E (fn E y => k E (E(x)(y)))

