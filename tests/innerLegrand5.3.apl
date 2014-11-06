⍝ *******************
⍝ Example 1
⍝ *******************

HMS ← 3 44 29
good ← +/ 3600 60 1 × HMS             ⍝ --> 13469
excelent ← 3600 60 1 +.× HMS          ⍝ --> 13469

ex1 ← good + excelent                 ⍝ --> 13469+13469 = 26938

⍝ *******************
⍝ Example 2
⍝ *******************

Price ←   6  4.2  1.5  8.9  31  18
Qty   ←   2  6    3    5    1   0.5

beginnerSolution ← +/ Price × Qty       ⍝ --> 126.2
innerSolution ← Price +.× Qty           ⍝ --> 126.2

ex2 ← beginnerSolution + innerSolution  ⍝ --> 252.4

⍝ *******************
⍝ Total
⍝ *******************
ex1 + ex2                               ⍝ --> 26938 + 252.4 = 27190.4