mean ← +/÷≢ 

a ← 2 3 4 2 4 6 3 5

⎕ ← mean a

meandev ← ⊢ - +/ ÷ ≢ 

⎕ ← meandev a

var ← mean ({⍵×⍵}⊢-+/÷≢)

stddev ← {⍵*0.5} var

all ← mean, var, stddev

⎕ ← all a       ⍝ -> 

⎕ ← (⍳{⍺/⍵}⍳)3  ⍝ -> 1 2 2 3 3 3

⍝ ⎕ ← (⍳(/∘⊢)⍳)3
⍝ ⎕ ← (2/⍳)3

0
