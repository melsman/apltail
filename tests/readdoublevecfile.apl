
is ← ⎕ReadDoubleVecFile 'readdoublevecfile.txt'

⎕ ← 'Number of doubles in file:'
⎕ ← ⊃ ⍴ is                            ⍝ -> 16

⎕ ← 'File content:'
⎕ ← is

⎕ ← 'File content reversed:'
⎕ ← ⌽ is

0