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

<table>
<tr><th>Expression</th><th>Name</th><th>Meaning</th><th>Notes</th></tr>
<tr><td>B ⍴ A</td><td>Reshape</td><td>Returns an array with shape B and values taken from ,A (rotated if there are not sufficiently many values in A).</td><td></td></tr>
<tr><td>B ⍉ A</td><td>Dyadic transpose</td><td>Returns the generalized transposed version of A, according to the dimension index vector B.</td><td>B must be an integer vector of the same length as A.</td></tr>
<tr><td>B + A</td><td>Addition</td><td>Pair-wise addition of elements in B and A.</td><td>Scalar expansion supported: Arrays A and B must be of the same shape unless one of the arrays is a scalar value in which case the array is extended to match the shape of the other array.</td></tr>
<tr><td>B - A</td><td>Subtraction</td><td>Pair-wise subtraction of elements in B and A.</td><td>Scalar expansion supported.</td></tr>
<tr><td>B ÷ A</td><td>Division</td><td>Pair-wise division of elements in B and A.</td><td>Scalar expansion supported.</td></tr>
<tr><td>B × A</td><td>Multiplication</td><td>Pair-wise multiplication of elements in B and A.</td><td>Scalar expansion supported.</td></tr>
<tr><td>B ⌈ A</td><td>Maximum</td><td>Pair-wise maximum of elements in B and A.</td><td>Scalar expansion supported.</td></tr>
<tr><td>B ⌊ A</td><td>Minimum</td><td>Pair-wise minimum of elements in B and A.</td><td>Scalar expansion supported.</td></tr>
<tr><td>N ↑ A</td><td>Take</td><td>Returns the vector resulting from taking N elements from A (row-major order).</td><td>When N is negative, values are taken from the right.</td></tr>
<tr><td>N ↓ A</td><td>Drop</td><td>Returns the vector resulting from dropping N elements from A (row-major order).</td><td>When N is negative, values are dropped from the right.</td></tr>
<tr><td>N ⌽ A</td><td>Rotate</td><td>Returns the vector resulting from rotating values in A to the left N steps (row-major order).</td><td>When N is negative, values are right-rotated.</td></tr>
<tr><td>N ⊖ A</td><td>Rotate last</td><td>Returns the vector resulting from rotating values (in A) N steps along the last axis (column-major order).</td><td>When N is negative, values are rotated in the opposite directions.</td></tr>
<tr><td>B , A</td><td>Catenate</td><td>Returns the vector resulting from catenating elements in B with elements in A (row-major order).</td><td></td></tr>
<tr><td>B ⍪ A</td><td>Catenate last</td><td>Returns the vector resulting from catenating elements in B with elements in A (column-major order).</td><td></td></tr>
<tr><td>B ∨ A</td><td>Boolean or</td><td>Returns the logical disjunction of B and A.</td><td>Scalar expansion supported.</td></tr>
<tr><td>B ∧ A</td><td>Boolean and</td><td>Returns the logical conjunction of B and A.</td><td>Scalar expansion supported.</td></tr>
<tr><td>B ⍱ A</td><td>Boolean nor</td><td>Returns the negation of the logical disjunction of B and A.</td><td>Scalar expansion supported.</td></tr>
<tr><td>B ⍲ A</td><td>Boolean nand</td><td>Returns the negation of the logical conjunction of B and A.</td><td>Scalar expansion supported.</td></tr>
<tr><td>B = A</td><td>Equality</td><td>Pair-wise equality of elements in B and A.</td><td>Scalar expansion supported.</td></tr>
<tr><td>B ≠ A</td><td>Not equal</td><td>Pair-wise inequality of elements in B and A.</td><td>Scalar expansion supported.</td></tr>
<tr><td>B < A</td><td>Less than</td><td>Pair-wise less than comparison of elements in B and A.</td><td>Scalar expansion supported.</td></tr>
<tr><td>B ≤ A</td><td>Less than or equal</td><td>Pair-wise less than or equal comparison of elements in B and A.</td><td>Scalar expansion supported.</td></tr>
<tr><td>B > A</td><td>Greater than</td><td>Pair-wise greater than comparison of elements in B and A.</td><td>Scalar expansion supported.</td></tr>
<tr><td>B ≥ A</td><td>Greater than or equal</td><td>Pair-wise greater that or equal comparison of elements in B and A.</td><td>Scalar expansion supported.</td></tr>
<tr><td>B / A</td><td>Compress</td><td>Returns the elements in A (as a vector) for which the corresponding elements in the boolean vector B are true.</td><td>Assumes A to be a vector of the same length as the boolean vector B.</td></tr>
<tr><td>B / A</td><td>Replicate</td><td>Returns repeated elements from A (as a vector) with the repetition controlled by the corresponding elements in the integer vector B.</td><td>Assumes A to be a vector of the same length as the integer vector B.</td></tr>
</table>

### Monadic Operators

<table>
<tr><th>Expression</th><th>Name</th><th>Meaning</th><th>Notes</th></tr>
<tr><td>f /</td><td>Reduce</td><td>Returns a monadic function that reduces (using f) its argument array along the first axis. The argument to the operator must be a dyadic function.</td><td>Certain limitations apply.</td></tr>
<tr><td>f ⌿</td><td>Reduce last axis</td><td>Returns a monadic function that reduces (using f) its argument array along the last axis. The argument to the operator must be a dyadic function.</td><td>Certain limitations apply.</td></tr>
<tr><td>f ¨</td><td>Each</td><td>Returns a monadic function that maps (using f) over its argument array. The argument to the operator must be a monadic function.</td><td></td></tr>
<tr><td>∘. f</td><td>Outer product</td><td>Returns a dyadic function that applies f to all elements (pairs) from the cartesian product of the array arguments.</td><td></td></tr>
</table>

### Dyadic Operators

<table>
<tr><th>Expression</th><th>Name</th><th>Meaning</th><th>Notes</th></tr>
<tr><td>f . g</td><td>Inner product</td><td>Returns a dyadic function that performs the inner product of its arguments (using f and g). The arguments to the operator must be a dyadic function.</td><td>Certain limitations apply.</td></tr>
</table>

### Scalar extensions

Scalar extensions are supported.


### Identity items
For certain built-in functions, identity items are supported:

<table>
<tr><th>Function</th><th>Left-identity</th><th>Right-identity</th></tr>
<tr><td>+</td><td>0</td><td>0</td></tr>
<tr><td>-</td><td></td><td>0</td></tr>
<tr><td>×</td><td>1</td><td>1</td></tr>
<tr><td>÷</td><td></td><td>1</td></tr>
<tr><td>|</td><td>0</td><td></td></tr>
<tr><td>=,≥,≤</td><td>1</td><td>1</td></tr>
<tr><td>≠,>,<</td><td>0</td><td>0</td></tr>
<tr><td>∨</td><td></td><td>0</td></tr>
<tr><td>∧</td><td></td><td>1</td></tr>
<tr><td>⌈</td><td>-inf</td><td>-inf</td></tr>
<tr><td>⌊</td><td>inf</td><td>inf</td></tr>
</table>

### Quad Assignment (output)

Intermediate arrays may be printed using assignment to the quad-character:

```apl
⎕ ← A
```

Arrays of rank 2 are printed as matrices. Other arrays are printed
using a flat representation, specifying both the shape vector and the
underlying values.
