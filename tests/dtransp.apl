A ← 2 3 4 ⍴ ⍳ 24
⎕ ← 'A:'
⎕ ← A

B ← 2 1 3 ⍉ A           ⍝  ⍴B = 3 2 4
⎕ ← 'B:'
⎕ ← B
⎕ ← +/B                 ⍝ -->  10 58
                        ⍝      26 74
                        ⍝      42 90

⍝ The following example requires a correct definition of exchange - see ARRAY'14
⍝ paper is wrong wrt the specification of exchange - here is a correct
⍝ specification:
⍝    exchange_p(q) = r where r(p(i)) = q(i)

C ← 3 1 2 ⍉ A           ⍝  ⍴C = 3 4 2
⎕ ← 'C:'
⎕ ← C
⎕ ← +/C                 ⍝ -->  14 16 18 20
                        ⍝      22 24 26 28
                        ⍝      30 32 34 36

0

