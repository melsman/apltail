⍝ ---------------------------------------------
⍝ Drop operations on arrays of different ranks
⍝ ---------------------------------------------

⍝ Normal drop
⎕ ← 3 ↓ 1 2 3 4 5    ⍝ -->   4 5

⍝ Normal overdrop
⎕ ← 6 ↓ 1 2 3 4      ⍝ -->   [0]()

⍝ Multi-dimensional drop
⎕ ← 2 ↓  4 5 ⍴ ⍳ 20  ⍝ -->   11 12 13 14 15
                     ⍝       16 17 18 19 20

⍝ Multi-dimensional overdrop
⎕ ← 4 ↓ 2 5 ⍴ ⍳ 8    ⍝ -->   [0,5]()

⍝ Test of negative drop
⎕ ← ¯2 ↓ 1 2 3 4 5   ⍝ -->   1 2 3

⎕ ← ¯2 ↓ 4 2 ⍴ ⍳ 8   ⍝ -->   1 2
                     ⍝       3 4

⍝ Test of underdrop
⎕ ← ¯6 ↓ 1 2 3 4     ⍝ -->   [0]()
⎕ ← ¯6 ↓ 4 2 ⍴ ⍳ 8   ⍝ -->   [0,2]()
⎕ ← ¯4 ↓ 4 2 ⍴ ⍳ 8   ⍝ -->   [0,2]()

⍝ Drop from zilde
⎕ ← 3 ↓ ⍬            ⍝ -->   [0]()
⎕ ← ¯3 ↓ ⍬           ⍝ -->   [0]()

⍝ zero-drops
⎕ ← 0 ↓ 1 2 3 4      ⍝ -->   [4](1,2,3,4)
⎕ ← 0 ↓ 2 2 ⍴ ⍳ 4    ⍝ -->   1 2
                     ⍝       3 4
⎕ ← 0 ↓ ⍬            ⍝ -->   [0]()

0