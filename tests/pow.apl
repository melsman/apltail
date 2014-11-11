
⍝ Power Operator example from Legrands "Mastering Dyalog APL", page 415 (first edition).

mat ← 3 3 ⍴ 1 2 3 8 0 4 7 6 5

Spin ← {⊖⍉ ⍵}

⎕ ← mat

⎕ ← Spin Spin mat

x ← Spin ⍣ 14 mat

y ← x = Spin Spin mat

⎕ ← ∧/,y

⍝ Power allows us to calculate a Fibonacci series 
fibo ← {(,⍵),+/¯2↑,⍵}

fib ← {(fibo⍣⍵) 0 1}

⎕ ← fib 10

⍝ (⌊ ⍣ 1) 23.73 42.25

0