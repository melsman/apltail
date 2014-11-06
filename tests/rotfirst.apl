⍝ Rotate along the first axis as in (1 ⊖ A)

rotfirst ← { (⍺ ↓ ⍵) ⍪ ⍺ ↑ ⍵ }

A ← 3 4 ⍴ ⍳ 12

⎕ ← 1 rotfirst A            ⍝ -->   5  6  7  8
                            ⍝       9 10 11 12
                            ⍝       1  2  3  4
0