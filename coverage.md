## Coverage of the APL Compiler `aplt`

This page documents the functionality of the APL2Tail compiler `aplt`.

### Datatypes

Currently, only booleans, integers, and doubles are supported. There
is currently no support for characters and strings.

### Monadic Functions

| Expression    | Name             | Meaning              | Notes         |
|---------------|------------------|----------------------|---------------|
| ⍴ A           | Shape            | Returns the shape of the array A as a vector of integers. | |
| ⍳ N           | Iota             | Returns a vector of length N with values [1,...,N].  | |
| , A           | Ravel            | Returns the vector of values appearing in A in row-major order.  | |
| + A           | Identity         | Returns A.           |               |
| - A           | Negation         | Returns the array A with all values negated.  | |
| ÷ A           | Reciprocal       | Returns the array A with elements being the reciprocal values of the values in A.  | |
| × A           | Sign             | Returns the array A with elements being -1 for negative values in A and 1 for positive values in A.  | |
| ⍉ A           | Transpose        | Returns the transposed version of A.  | |
| ~ A           | Logical negation | Returns A with boolean elements negated.  |  Assumes A to be of boolean type. |

### Dyadic Functions

| Expression    | Name             | Meaning              | Notes         |
|---------------|------------------|----------------------|---------------|
| B ⍴ A         | Reshape          | Returns an array with shape B and values taken from ,A (rotated if there are not sufficiently many values in A).         | |
| B ⍉ A         | Dyadic transpose | Returns the generalized transposed version of A, according to the dimension index vector B. | B must be an integer vector of the same length as A. |
| B + A         | Addition         | Pair-wise addition of elements in B and A. | Scalar expansion supported: Arrays A and B must be of the same shape unless one of the arrays is a scalar value in which case the array is extended to match the shape of the other array. |
| B - A         | Subtraction      | Pair-wise subtraction of elements in B and A. | Scalar expansion supported. |
| B ÷ A         | Division         | Pair-wise division of elements in B and A. | Scalar expansion supported. |
| B × A         | Multiplication   | Pair-wise multiplication of elements in B and A. | Scalar expansion supported. |
| B ⌈ A         | Maximum          | Pair-wise maximum of elements in B and A. | Scalar expansion supported. |
| B ⌊ A         | Minimum          | Pair-wise minimum of elements in B and A. | Scalar expansion supported. |
| N ↑ A         | Take             | Returns the vector resulting from taking N elements from A (row-major order). | When N is negative, values are taken from the right. |
| N ↓ A         | Drop             | Returns the vector resulting from dropping N elements from A (row-major order). | When N is negative, values are dropped from the right. |
| N ⌽ A         | Rotate           | Returns the vector resulting from rotating values in A to the left N steps (row-major order). | When N is negative, values are right-rotated. |
| N ⊖ A         | Rotate last      | Returns the vector resulting from rotating values (in A) N steps along the last axis (column-major order). | When N is negative, values are rotated in the opposite directions. |
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
| f ⍣ n         | Power            | Returns a monadic function that iteratively applies f to its argument n times. | Parentheses may be needed to separate n from the argument to the resulting function. |

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

### System Functions

| Expression    | Name             | Meaning              | Notes         |
|---------------|------------------|----------------------|---------------|
| ⎕ReadFile file| READ FILE        | Takes a filepath as parameter and returns a vector of characters | Fails in case the file cannot be read. |
| ⎕ReadIntVecFile file| READ INT VECTOR FROM FILE | Takes a filepath as parameter and returns a vector of integers | Fails in case the input file cannot be read or does not contain a sequence of integers separated by space characters. |
