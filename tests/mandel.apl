⍝ Mandelbrot one-liner:

⍝ Compared to Dyalog APL, additional parentheses are needed around
⍝ bindings (←) and around the power operator (⍣). Moreover, the
⍝ function parameter to the power operator needs to be monadic.

⎕ ← ' #'[1+9>|({m+⍵×⍵}⍣9)(m←¯3×.7j.5-⍉a∘.+0j1×(a←(1+⍳n+1)÷(n←28)))]

⍝ And one that uses trains, which does not work...:
⍝ ⎕ ← ' #'[1+9>|m∘(2*⍨+)⍣9⊢(m←¯3×.7j.5-⍉a∘.+0j1×(a←(1+⍳n+1)÷(n←98)))]

0