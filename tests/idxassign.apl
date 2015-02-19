
v ← ⍳ 28

v[4] ← 34

⎕ ← v

m ← 3 4 ⍴ ⍳ 9

m[2;3] ← 0

⎕ ← m

a ← 3 4 3 ⍴ ⍳ 128

a[1;1;3] ← 1000

⎕ ← a

b ← a + 0.2

k ← 3

b[2;k;1] ← 2001

⎕ ← b

x ← 7 5 ⍴ 'hello this is a great story I think'

x[3;4] ← 'X'

⎕ ← x

0