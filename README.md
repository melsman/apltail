## apltail: An APL Compiler targeting a Typed Array Intermediate Language

This software implements an APL compiler in Standard ML. The compiler
targets a [typed array intermediate language](http://www.elsman.com/pdf/array14_final.pdf) [1]. The
executable also contains an interpreter for TAIL and a compiler from
TAIL into C.

See [the compilation scheme](doc/comp.md).

See also the [coverage page](doc/coverage.md).

## Status

[![Build Status](https://travis-ci.org/melsman/apltail.svg?branch=master)](https://travis-ci.org/melsman/apltail)

## An example

Consider the following program:

```apl
f ← {5+⍵}    ⍝ Function adding 5 to its argument (⍵)
+/ f ⍳ 30    ⍝ Apply f to the vector 1..30 and
             ⍝  sum up the elements in the resulting vector
```

Here is what happens when the program is compiled and executed:

    bash-3.2$ ./aplt -p_tail lib/prelude.apl tests/test.apl
    [Reading file: lib/prelude.apl]
    [Reading file: tests/test.apl]
    Resulting program:
    let v0:<int>30 = iotaV(30) in
    i2d(reduce(addi,0,eachV(fn v1:[int]0 => addi(5,v1),v0)))
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

    bash-3.2$ ./aplt -p_tail lib/prelude.apl tests/signal.apl
    [Reading file: lib/prelude.apl]
    [Reading file: tests/signal.apl]
    Resulting program:
    let v0:<int>100 = iotaV(100) in
    let v3:<int>101 = consV(0,v0) in
    reduce(addd,0.00,each(fn v11:[double]0 => maxd(~50.00,v11),each(fn v10:[double]0 => mind(50.00,v10),each(fn v9:[double]0 => muld(50.00,v9),zipWith(divd,each(i2d,drop(1,zipWith(subi,v3,rotateV(~1,v3)))),eachV(fn v2:[double]0 => addd(0.01,v2),eachV(i2d,v0)))))))
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

    bash-3.2$ ./aplt -p_tail lib/prelude.apl tests/test15.apl
    [Reading file: lib/prelude.apl]
    [Reading file: tests/test15.apl]
    Resulting program:
    let v0:[int]2 = reshape([3,2],iotaV(5)) in
    let v1:[int]2 = reshape([3,2],iotaV(4)) in
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

    bash-3.2$ ./aplt -p_tail lib/prelude.apl tests/test13.apl
    [Reading file: lib/prelude.apl]
    [Reading file: tests/test13.apl]
    Resulting program:
    let v0:[int]2 = reshape([3,2],iotaV(5)) in
    let v1:[int]2 = transp(v0) in
    let v6:[int]3 = transp2([2,1,3],reshape([3,3,2],v0)) in
    let v12:[int]3 = transp2([1,3,2],reshape([3,2,3],v1)) in
    let v17:[int]2 = reduce(addi,0,zipWith(muli,v6,v12)) in
    i2d(reduce(muli,1,reduce(addi,0,v17)))
    Evaluating
    Result is [](65780.0)

Without optimizations, the compilation results in a slightly larger output:

    bash-3.2$ ./aplt -p_tail -noopt lib/prelude.apl tests/test13.apl
    [Reading file: lib/prelude.apl]
    [Reading file: tests/test13.apl]
    Resulting program:
    let v0:[int]2 = reshape([3,2],iotaV(5)) in
    let v1:[int]2 = transp(v0) in
    let v2:<int>3 = catV(dropV(b2iV(tt),shape(v1)),shape(v0)) in
    let v3:[int]0 = subi(firstV(shapeV(shape(v0))),b2iV(tt)) in
    let v4:<int>3 = iotaV(firstV(shapeV(v2))) in
    let v5:<int>3 = catV(rotateV(v3,dropV(~1,v4)),takeV(~1,v4)) in
    let v6:[int]3 = transp2(v5,reshape(v2,v0)) in
    let v7:<int>3 = catV(dropV(~1,shape(v0)),shape(v1)) in
    let v8:S(int,2) = firstV(shapeV(shape(v0))) in
    let v9:<int>3 = iotaV(firstV(shapeV(v7))) in
    let v10:<int>1 = dropV(negi(v8),rotateV(v8,iotaV(firstV(shapeV(v9))))) in
    let v11:<int>3 = catV(dropV(~1,iotaV(v8)),snocV(v10,v8)) in
    let v12:[int]3 = transp2(v11,reshape(v7,v1)) in
    let v17:[int]2 = reduce(addi,0,zipWith(muli,v6,v12)) in
    i2d(reduce(muli,1,reduce(addi,0,v17)))
    Evaluating
    Result is [](65780.0)

## Try it!

The software makes use of the SML [unicode library](https://github.com/diku-dk/sml-unicode) library for lexing and
the [aplparse](https://github.com/diku-dk/sml-aplparse) project for
parsing. It also uses various other packages that can be installed with [smlpkg](https://github.com/diku-dk/smlpkg), which itself needs to be available on the system for pulling down the library sources.

You also need a Standard ML compiler (e.g., [Mlton](http://www.mlton.org/) or [MLKit](http://melsman.github.io/mlkit)).

To pull down the dependent libraries and to compile the source, execute the following commands in a shell:

    $ make prepare
    $ make all

These commands will leave an executable `aplt` in the root directory of the repository.

To run a series of tests, execute `make test` in your shell.

To get `aplt` to output type instantiation list for the polymorphic
functions, such as `reduce`, `each`, and `take`, you may pass the
option `-p_types` to `aplt`.

See also the [coverage page](doc/coverage.md).

To compile with MLKit instead of with MLton, which takes quite some
time, instead of typing `make all` above, type instead `MLCOMP=mlkit make all`.

## License

This software is published under the [MIT License](MIT_LICENSE.md).

## References

[1] Martin Elsman and Martin Dybdal. __Compiling a Subset of APL Into
a Typed Intermediate Language__. In _ACM SIGPLAN International
Workshop on Libraries, Languages and Compilers for Array Programming
(ARRAY'14)_. Edinburgh, UK. June,
2014. [pdf](http://www.elsman.com/pdf/array14_final.pdf),
[bibtex](http://www.elsman.com//pdf/array14_final.bibtex.txt).
