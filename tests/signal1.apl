f ← { diff ← {1↓⍵-¯1⌽⍵} ⋄ signal ← {¯50⌈50⌊50×(diff 0,⍵)÷0.01+⍵} ⋄ +/ signal ⍳ ⍵ }
f 1000


