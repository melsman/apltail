## Coverage of the APLT Compiler

This page documents the functionality of the APLT compiler.

### Datatypes

Supported datatypes include booleans, integers, doubles, and
characters. Strings are represented as character arrays. Nested arrays
are currently not supported.

### Monadic Functions

| Expression    | Name             | Meaning              | Notes         |
|---------------|------------------|----------------------|---------------|
| ⍴ A           | Shape            | Returns the shape of the array A as a vector of integers. | |
| ⍳ N           | Iota             | Returns a vector of length N with values [1,...,N].  | |
| , A           | Ravel            | Returns the vector of values appearing in A in row-major order.  | |
| + A           | Identity         | Returns A.           |               |
| - A           | Negation         | Returns the array A with all elements negated.  | |
| ÷ A           | Reciprocal       | Returns the array A with elements being the reciprocal values of the values in A.  | |
| × A           | Sign             | Returns the array A with elements being -1 for negative values in A and 1 for positive values in A.  | |
| ⌈ A           | Ceil             | Returns A with elements ceiled. | The function returns an integer array. |
| ⌊ A           | Floor            | Returns A with elements floored. | The function returns an integer array. |
| ⍉ A           | Transpose        | Returns the transposed version of A.  | |
| ~ A           | Logical negation | Returns A with boolean elements negated.  |  Assumes A to be of boolean type. |
| ○ A           | Pi times         | Returns A with all elements multiplied by pi. |
| ⍟ A           | Log              | Returns A with the natural logarithm applied to all elements. | 
| ? A           | Random           | Returns random values between 0 and 1 (if the argument is 0) and otherwise between 0 and the argument (whole number). | 
| ≢ A           | Tally            | Returns the size of the outer dimension of A. | 

### Dyadic Functions

| Expression    | Name             | Meaning              | Notes         |
|---------------|------------------|----------------------|---------------|
| B ⍴ A         | Reshape          | Returns an array with shape B and values taken from ,A (rotated if there are not sufficiently many values in A). | The length of the vector B must be statically decided. |
| B ⍉ A         | Dyadic transpose | Returns the generalized transposed version of A, according to the dimension index vector B. | B must be an integer vector of the same length as A. |
| B + A         | Addition         | Pair-wise addition of elements in B and A. | Scalar expansion supported: Arrays A and B must be of the same shape unless one of the arrays is a scalar value in which case the array is extended to match the shape of the other array. |
| B - A         | Subtraction      | Pair-wise subtraction of elements in B and A. | Scalar expansion supported. |
| B ÷ A         | Division         | Pair-wise division of elements in B and A. | Scalar expansion supported. |
| B × A         | Multiplication   | Pair-wise multiplication of elements in B and A. | Scalar expansion supported. |
| B ⌈ A         | Maximum          | Pair-wise maximum of elements in B and A. | Scalar expansion supported. |
| B ⌊ A         | Minimum          | Pair-wise minimum of elements in B and A. | Scalar expansion supported. |
| N ↑ A         | Take             | Returns the vector resulting from taking N elements from A (row-major order). | When N is negative, values are taken from the right. |
| N ↓ A         | Drop             | Returns the vector resulting from dropping N elements from A (row-major order). | When N is negative, values are dropped from the right. |
| ```N ⌽ A```         | Rotate           | Returns the vector resulting from rotating values in A to the left N steps (row-major order). | When N is negative, values are right-rotated. |
| ```N ⊖ A```         | Rotate last      | Returns the vector resulting from rotating values (in A) N steps along the last axis (column-major order). | When N is negative, values are rotated in the opposite directions. |
| `N ⊖ A`         | Rotate last      | Returns the vector resulting from rotating values (in A) N steps along the last axis (column-major order). | When N is negative, values are rotated in the opposite directions. |
| B , A         | Catenate         | Returns the vector resulting from catenating elements in B with elements in A (row-major order). | |
| B ⍪ A         | Catenate last    | Returns the vector resulting from catenating elements in B with elements in A (column-major order). | |
| B ∨ A         | Boolean or       | Returns the logical disjunction of B and A. | Scalar expansion supported. |
| B ∧ A         | Boolean and      | Returns the logical conjunction of B and A. | Scalar expansion supported. |
| B ⍱ A         | Boolean nor      | Returns the negation of the logical disjunction of B and A. | Scalar expansion supported. |
| B ⍲ A         | Boolean nand     | Returns the negation of the logical conjunction of B and A. | Scalar expansion supported. |
| B = A         | Equality         | Pair-wise equality of elements in B and A. | Scalar expansion supported. |
| B ≠ A         | Not equal        | Pair-wise inequality of elements in B and A. | Scalar expansion supported. |
| B < A         | Less than        | Pair-wise less than comparison of elements in B and A. | Scalar expansion supported. |
| B ≤ A         | Less than or equal| Pair-wise less than or equal comparison of elements in B and A. | Scalar expansion supported. |
| B > A         | Greater than     | Pair-wise greater than comparison of elements in B and A. | Scalar expansion supported. |
| B ≥ A         | Greater than or equal|Pair-wise greater that or equal comparison of elements in B and A. | Scalar expansion supported. |
| B / A         | Compress         | Returns the elements in A (as a vector) for which the corresponding elements in the boolean vector B are true. | Assumes A to be a vector of the same length as the boolean vector B. |
| B / A         | Replicate        | Returns repeated elements from A (as a vector) with the repetition controlled by the corresponding elements in the integer vector B. | Assumes A to be a vector of the same length as the integer vector B. |
| B ○ A         | Circ             | Returns the trigonometric function on A dependent on the integer value B; 1:sin, 2:cos, 3:tan. | The value of B has to be statically determined. | 

### Monadic Operators

| Expression    | Name             | Meaning              | Notes         |
|---------------|------------------|----------------------|---------------|
| f /           | Reduce           | Returns a monadic function that reduces (using f) its argument array along the first axis. The argument to the operator must be a dyadic function. | Certain limitations apply. |
| f ⌿           | Reduce last axis | Returns a monadic function that reduces (using f) its argument array along the last axis. The argument to the operator must be a dyadic function. | Certain limitations apply. |
| f ¨           | Each             | Returns a monadic function that maps (using f) over its argument array. The argument to the operator must be a monadic function. |  |
| ∘. f          | Outer product    | Returns a dyadic function that applies f to all elements (pairs) from the cartesian product of the array arguments. |  |

### Dyadic Operators

| Expression    | Name             | Meaning              | Notes         |
|---------------|------------------|----------------------|---------------|
| f . g         | Inner product    | Returns a dyadic function that performs the inner product of its arguments (using f and g). The arguments to the operator must be a dyadic function. | Certain limitations apply. |
| f ⍣ n         | Power            | Returns a monadic function that iteratively applies f to its argument n times. | Parentheses may be needed to separate n from the argument to the resulting function. The function f may take (and produce) a vector of arrays, which may be indexed in the body of f using array indexing. |

### Scalar extensions

Scalar extensions are supported.


### Identity items
For certain built-in functions, identity items are supported:

| Function      | Left-identity    | Right-identity |
|---------------|------------------|----------------|
| + | 0 | 0 |
| - |  | 0 |
| × | 1 | 1 |
| ÷ |  | 1 |
| | | 0 |  |
| =,≥,≤ | 1 | 1 |
| ≠,>,< | 0 | 0 |
| ∨ |  | 0 |
| ∧ |  | 1 |
| ⌈ | -inf | -inf |
| ⌊ | inf | inf |

### Quad Assignment (output)

Intermediate arrays may be printed using assignment to the quad-character:

```apl
⎕ ← A
```

Arrays of rank 2 are printed as matrices. Other arrays are printed
using a flat representation, specifying both the shape vector and the
underlying values.

### Trains (points-free notation)

APLT supports writing expressions in points-free (also called tacit) notation. For instance, the APL code

```apl
mean ← +/÷≢
⎕ ← mean 2 37 4 1
```
results in the output `11`. APLT support both monadic and dyadic _Agh_ trains and monadic and dyadic _fgh_ trains.

### Tuples of arrays

APLT supports having tuples of arrays of different ranks and
types. For instance, a tuple `c` of a vector, a pair (of two vectors), and
a scalar, may be constructed as

```apl
a ← 1 2 4 3
b ← 'hello'
c ← a (b a) 4
```

Tuples, such as `c`, may be passed to functions and deconstructed using index notation:

```apl
f ← {
  a1 ← +/⍵[1]
  a2 ← ⊃⍴(⍵[2])[1]
  a1 + a2 + ⍵[3]
}
⎕ ← f c   ⍝ --> [](19)
```

A particular use of this feature is in conjunction with the power
operator. See the [powtup.apl](https://github.com/melsman/apltail/blob/master/tests/powtup.apl) test example.

### System Functions

| Expression    | Name             | Meaning              | Notes         |
|---------------|------------------|----------------------|---------------|
| ⎕ReadFile file| READ FILE        | Takes a filepath as parameter and returns a vector of characters | Fails in case the file cannot be read. |
| ⎕ReadIntVecFile file| READ INT VECTOR FROM FILE | Takes a filepath as parameter and returns a vector of integers | Fails in case the input file cannot be read or does not contain a sequence of integers separated by space characters. |


### Using APLT

Here is the output from executing `aplt` on the command-line:

    bash-3.2$ aplt 
    Usage: ../apltail/aplt [OPTIONS]... file.apl...
     -o file        : write TAIL program to file
     -oc file       : write LAILA program to file
     -c             : compile only (no evaluation)
     -noopt         : disable optimizations
     -materialize   : disable materialization of arrays
     -p_tail        : print TAIL program
     -p_types       : print types in TAIL code
     -p_laila       : print LAILA code
     -s_parse       : stop after parsing
     -s_tail        : stop after TAIL generation
     -silent        : evaluation output only (unless there are errors)
     -v             : verbose
     -O n           : optimisation level (n>0 optimises double operations aggresively)
     -comments      : write comments in generated C code
     -unsafe        : don't include assert code in generated C code for array indexing
     -stat_laila    : print statistics for LAILA code generation
     -opt_hoist     : enable hoist optimization in LAILA code generation
     -opt_loopsplit : enable loop split optimization in LAILA code generation
