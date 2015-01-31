⍝ Rotate along the first axis as in (1 ⊖ A)

rotfirst ← { (⍺ ↓ ⍵) ⍪ ⍺ ↑ ⍵ }

A ← 3 4 ⍴ ⍳ 12

X ← 1 rotfirst A            ⍝ -->   5  6  7  8
                            ⍝       9 10 11 12
                            ⍝       1  2  3  4

⎕ ← 9 ⌊ X - 1               ⍝ -->   4 5 6 7
                            ⍝       8 9 9 9
                            ⍝       0 1 2 3

⎕ ← 9 ⌊ (1 ⊖ A) - 1         ⍝ -->   4 5 6 7
                            ⍝       8 9 9 9
                            ⍝       0 1 2 3

0