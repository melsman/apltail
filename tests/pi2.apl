⍝ Compute pi
n ← 10000000
pi ← 4×(+/1>+/(?n 2⍴0)*2)÷n
err ← | pi - ○ 1

⎕ ← 'Computed pi:'
⎕ ← pi

⎕ ← 'Error:'
⎕ ← err

0.001 > err
