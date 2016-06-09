
f ← {
  a1 ← ⍵[1]
  a2 ← ⍵[2]
  a3 ← ⍵[3]
  r1 ← (1 1) + a1
  r2 ← a2,'hej'
  (r1 r2 3)
}

res ← (f⍣3) ((2 3) 'asbc' 8)
⍝ res ← f (2 '')
⎕ ← res[1]
⎕ ← res[2]
⎕ ← res[3]

0