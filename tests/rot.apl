⍝ Rotate along the last axis as in (1 ⌽ A)

rot ← { (⍉ ⍺ ↓ ⍉ ⍵) , ⍉ ⍺ ↑ ⍉ ⍵ }

A ← 3 4 ⍴ ⍳ 12

⎕ ← 1 rot A            ⍝ -->   2  3  4  1
                       ⍝       6  7  8  5
                       ⍝      10 11 12  9


⍝ 1 ⌽ A
0