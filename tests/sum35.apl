
⍝ Calculate the sum of all multiples of 3 or 5 below 1000

f ← {+/⍵×(0=3|⍵)∨0=5|⍵}

sumbelow ← {f⍳⍵-1}

⎕ ← sumbelow 10   ⍝ → 23

sumbelow 1000     ⍝ → 233168

