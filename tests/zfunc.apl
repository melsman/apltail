
zfunc ← {
  NP ← ⍺
  NC ← ⍵
  X1←1⌈+⌿NP
  X2←1⌈+⌿NC
  N1←(⍴NP)⍴X1
  N2←(⍴NC)⍴X2
  B1←NP÷N1
  B2←NC÷N2
  B←(NP+NC)÷N1+N2
  T←B1-B2
  N←B×(1-B)×((÷N1)+÷N2)
  N←N⋆0.5
  NA2←,N
  NA2 ← NA2+NA2=0
  N←(⍴N)⍴NA2
  Z←T÷N
  ⌈100×Z
}

X ← ⍳ 100

Y ← 100 + ⍳ 100

R ← X zfunc Y 

⎕ ← R

0