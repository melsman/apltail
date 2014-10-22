## apltail: An APL Compiler targeting a Typed Array Intermediate Language

This software implements an APL compiler in Standard ML. The compiler
targets a [typed array intermediate
language](http://www.elsman.com/pdf/array14_final.pdf) [1].

See [the compilation scheme](comp.md).

See also the [coverage page](coverage.md).

See also [AplCompile](https://github.com/melsman/aplcompile).

## An example

Consider the following program:

```apl
f ← {5+⍵}    ⍝ Function adding 5 to its argument (⍵)
+/ f ⍳ 30    ⍝ Apply f to the vector 1..30 and
             ⍝  sum up the elements in the resulting vector
```

Here is what happens when the program is compiled and executed:

    bash-3.2$ ./aplt -p_tail tests/test.apl
    [Reading file: tests/test.apl]
    Resulting program:
    let v0:Sh(30) = iotaSh(30) in
    i2d(reduce(addi,0,each(fn v1:[int]0 => addi(5,v1),v0)))
    Evaluating
    Result is [](615.0)

## Another example

Consider the program

```apl
diff ← {1↓⍵−¯1⌽⍵}
signal ← {¯50⌈50⌊50×(diff 0,⍵)÷0.01+⍵}
+/ signal ⍳ 100
```

Here is the result of compiling and evaluating it:

    bash-3.2$ ./aplt -p_tail tests/signal.apl
    [Reading file: tests/signal.apl]
    Resulting program:
    let v0:Sh(100) = iotaSh(100) in
    let v3:Sh(101) = consSh(0,v0) in
    reduce(addd,0.00,each(fn v11:[double]0 => maxd(~50.00,v11),each(fn v10:[double]0 => mind(50.00,v10),each(fn v9:[double]0 => muld(50.00,v9),zipWith(divd,each(i2d,drop(1,zipWith(subi,v3,rotateSh(~1,v3)))),each(fn v2:[double]0 => addd(0.01,v2),each(i2d,v0)))))))
    Evaluating
    Result is [](258.557340366)

## Example demonstrating transpose and a double-reduce

Consider the following APL program:

```apl
a ← 3 2 ⍴ ⍳ 5
a2 ← 3 2 ⍴ ⍳ 4
b ← ⍉ a
c ← b, ⍉ a2
×/ +/ c

⍝ 1 2    1 3 5  1 3 1  -+-> 14  
⍝ 3 4    2 4 1  2 4 2  -+-> 15
⍝ 5 1 
⍝                          ---
⍝                          210
```

Here is the result of compiling and evaluating it:

    bash-3.2$ ./aplt -p_tail tests/test15.apl
    [Reading file: tests/test15.apl]
    Resulting program:
    let v0:[int]2 = reshape([3,2],iotaSh(5)) in
    let v1:[int]2 = reshape([3,2],iotaSh(4)) in
    let v2:[int]2 = transp(v0) in
    let v3:[int]2 = cat(v2,transp(v1)) in
    i2d(reduce(muli,1,reduce(addi,0,v3)))
    Evaluating
    Result is [](210.0)

## Example demonstrating matrix-multiplication

```apl
a ← 3 2 ⍴ ⍳ 5
b ← ⍉ a
c ← a +.× b
×/ +/ c

⍝       1  3  5
⍝       2  4  1
⍝
⍝ 1 2   5 11  7  -+->    23
⍝ 3 4  11 25 19  -+->    55
⍝ 5 1   7 19 26  -+->    52
⍝                     65780
```

Here is the result of compiling and evaluating the example using the
[prelude](/prelude.apl) definition of inner product:

    bash-3.2$ ./aplt -p_tail prelude.apl tests/test13.apl
    [Reading file: prelude.apl]
    [Reading file: tests/test13.apl]
    Resulting program:
    let v0:[int]2 = reshape([3,2],iotaSh(5)) in
    let v1:[int]2 = transp(v0) in
    let v6:[int]3 = transp2([2,1,3],reshape([3,3,2],v0)) in
    let v12:[int]3 = transp2([1,3,2],reshape([3,2,3],v1)) in
    let v17:[int]2 = reduce(addi,0,zipWith(muli,v6,v12)) in
    i2d(reduce(muli,1,reduce(addi,0,v17)))
    Evaluating
    Result is [](65780.0)

Without optimizations, the compilation results in a slightly larger output:

    bash-3.2$ ./aplt -p_tail -noopt prelude.apl tests/test13.apl
    [Reading file: prelude.apl]
    [Reading file: tests/test13.apl]
    Resulting program:
    let v0:[int]2 = reshape([3,2],iotaSh(5)) in
    let v1:[int]2 = transp(v0) in
    let v2:Sh(3) = catSh(dropSh(1,shape(v1)),shape(v0)) in
    let v3:[int]0 = subi(firstSh(shapeSh(shape(v0))),1) in
    let v4:Sh(3) = iotaSh(firstSh(shapeSh(v2))) in
    let v5:Sh(3) = catSh(rotateSh(v3,dropSh(~1,v4)),takeSh(~1,v4)) in
    let v6:[int]3 = transp2(v5,reshape(v2,v0)) in
    let v7:Sh(3) = catSh(dropSh(~1,shape(v0)),shape(v1)) in
    let v8:Si(2) = firstSh(shapeSh(shape(v0))) in
    let v9:Sh(3) = iotaSh(firstSh(shapeSh(v7))) in
    let v10:Sh(1) = dropSh(negi(v8),rotateSh(v8,iotaSh(firstSh(shapeSh(v9))))) in
    let v11:Sh(3) = catSh(dropSh(~1,iotaSh(v8)),snocSh(v10,v8)) in
    let v12:[int]3 = transp2(v11,reshape(v7,v1)) in
    let v17:[int]2 = reduce(addi,0,zipWith(muli,v6,v12)) in
    i2d(reduce(muli,1,reduce(addi,0,v17)))
    Evaluating
    Result is [](65780.0)

## Try it!

The software makes use of the SML [unicode
library](https://github.com/melsman/unicode) library for lexing and
the [aplparse](https://github.com/melsman/aplparse) project for
parsing. It also uses parts of the
[kitlib](https://github.com/melsman/kitlib) library. The software is
setup to make use of
[Smackage](https://github.com/standardml/smackage). This means that
you need to [get Smackage working on your
system](http://www.elsman.com/lessons/2014/10/02/getting-started-with-smackage/)
and add the following lines to your `sources.local` file in your
`$(HOME)/.smackage` directory:

    kitlib git git://github.com/melsman/kitlib.git
    aplparse git git://github.com/melsman/aplparse.git
    apltail git git://github.com/melsman/apltail.git

You also need a Standard ML compiler (e.g., [Mlton](http://www.mlton.org/)).

Then simply write

    $ smackage refresh
    $ smackage get apltail
    $ smackage make apltail
    $ smackage make apltail install

To run a series of tests, execute `smackage make apltail test` in your shell.

To get `aplt` to output type instantiation list for the polymorphic
functions, such as `reduce`, `each`, and `take`, you may pass the
option `-p_types` to `aplt`.

See also the [coverage page](coverage.md).

## License

This software is published under the [MIT License](MIT_LICENSE.md).

## References 

[1] Martin Elsman and Martin Dybdal. __Compiling a Subset of APL Into
a Typed Intermediate Language__. In _ACM SIGPLAN International
Workshop on Libraries, Languages and Compilers for Array Programming
(ARRAY'14)_. Edinburgh, UK. June,
2014. [pdf](http://www.elsman.com/pdf/array14_final.pdf),
[bibtex](http://www.elsman.com//pdf/array14_final.bibtex.txt).