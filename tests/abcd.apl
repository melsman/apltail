
elements ← 'ABCD'

samples ← 8 6 ⍴ 'CCCAACBAADABCAACDCACDDBCDDCCCBADCBCCCACCCBCCBACC'

⎕← samples

X ← elements ∘.= samples

⎕← 'Result:'
⎕← ∧⌿∨/ X            ⍝ --> [8](0,0,0,1,0,1,0,0)

⍝ {∧/⍵}¨∨/{⍵ = elements}¨samples

0
