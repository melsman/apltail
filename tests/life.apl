⍝ Conway's Game of Life in APL without the use of nested arrays
⍝ Martin Elsman, 2014-11-10

⍝ Function computing the next generation of a board of life
life ← {
  rowsum ← {
    (¯1⌽⍵) + ⍵ + 1⌽⍵
  }
  neighbor ← {
    (rowsum ¯1⊖⍵) + (rowsum ⍵) + rowsum 1⊖⍵       
  }
  n ← neighbor ⍵
  (n=3) ∨ (n=4) ∧ ⍵
}

lifepr ← {
  ⎕ ← life ⍵
}
 
nlife ← {
  (lifepr ⍣ ⍺) ⍵
}

glider ← 3 3⍴1 1 1 1 0 0 0 1 0

board ← ⍉ ¯10 ↑ ⍉ ¯10 ↑ glider

⍝ board ← 5 5⍴0 0 0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 1 0 0

⍝ ⎕ ← board

⍝ ⎕ ← neighbor board

a ← 20 nlife board

0