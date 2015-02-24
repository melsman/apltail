
start ← ⎕NOW 0

⎕ ← 'Starting computation:'

⍝ time consuming operation
res ← ⌈/ +/ 5000 5000 ⍴ ⍳ 28

⎕ ← 'Result:'
⎕ ← res

⎕ ← 'Time (ms):'
⎕ ← (⎕NOW 0) - start

0
