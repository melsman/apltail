⍝ Anagram checking. Ignore spaces.

elimSp ← {(⍵≠(≢⍵)⍴' ')/⍵}

⍝ ⎕ ← elimSp 'hi there you are'

anagram ← { a ← elimSp ⍺ ⋄ w ← elimSp ⍵ ⋄ ((⍴a)=⍴w) ∧ ∧/(+/a ∘.= w) = +/a ∘.= a }

⎕ ← 'anagram' anagram 'nag a ram'

⎕ ← 'dyalog apl' anagram 'dog pay all'

⎕ ← ' ' anagram 'dog pay all'

⎕ ← 'cat' anagram 'dog'

0