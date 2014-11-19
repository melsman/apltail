
else ← {(⍺⍺⍣(~⍺))(⍵⍵⍣⍺)⍵}

f ← {⍵ + 30}
g ← {⍵ - 30}

⎕ ← (3>2) f else g 28

⎕ ← (3>3) f else g 28

A ← (2 2 ⍴ ⍳ 4)
⎕ ← (3>2) f else g A

⎕ ← (3>3) f else g A

0