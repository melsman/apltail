⍝ Multiplication table
⍝  a × b    scalar multiplication, "a times b"
⍝  ∘.       is the "outer product" operator
⍝  A ∘.× B  every item in A times every item in B
b ← (⍳ 10) ∘.× ⍳ 10
+/+/b