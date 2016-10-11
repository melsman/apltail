⍝ Longest streak of increasing numbers. Solution to Problem 2 of 
⍝ 2015 International APL Problem Solving Competition - Phase I

streak ← { is ← ¯1↓⍵<1⌽⍵ ⋄ s ← +\is ⋄ ⌈/0,s - ⌈\s - s × is }

⎕ ← streak 1 2 3 4 5 6 7 8 9

⎕ ← streak 1

⎕ ← streak 9 8 7 6 5 4

⎕ ← streak 1 5 3 4 2 6 7 8

0