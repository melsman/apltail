⍝ Compute pi
n ← 40000000
pi ← 4×(+/1>+/(?n 2⍴0)*2)÷n
err ← | pi - ○ 1

⎕ ← 'Computed pi:'
⎕ ← pi

⎕ ← 'Error:'
⎕ ← err

0.001 > err
