⍝
⍝   * if S is the triangle of rank n, then rank n+1 would be
⍝     the two-dimensional catenation:
⍝             S 0
⍝             S S
⍝     where "0" is an all-blank matrix same size as S.

⍝ f : [b]2 -> [b]2 ← {(⍵,(⍴⍵)⍴0)⍪⍵,⍵}
f ← {(⍵,(⍴⍵)⍴0)⍪⍵,⍵}
⍝S ← {' #'[(f⍣⍵) 1 1 ⍴ 1]}
S ← {(f⍣⍵) 1 1 ⍴ 1}

⎕ ← S 5

0