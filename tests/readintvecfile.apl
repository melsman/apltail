
is ← ⎕ReadIntVecFile 'readintvecfile.txt'

⎕ ← 'Number of integers in file:'
⎕ ← ⊃ ⍴ is                            ⍝ -> 16

⎕ ← 'File content:'
⎕ ← is

⎕ ← 'File content reversed:'
⎕ ← ⌽ is

0