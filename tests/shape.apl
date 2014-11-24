
⎕ ← 'Shape of scalar'
shS ← ⍴ 4
⎕ ← shS

⎕ ← 'Shape of vector'
shV ← (⍴ 5 ⍴ 4) + ⍴ 'hello' 
⎕ ← shV 

⎕ ← 'Shape of array'
shA ← (⍴ 5 2 3 ⍴ 4) + ⍴ 5 2 3 ⍴ 'hello' 
⎕ ← shA

⎕ ← 'Shape of empty array'
shZ ← ⍴ ⍬
⎕ ← shZ

0 