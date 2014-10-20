A ← 3 2 ⍴ ⍳ 5 ⋄ B ← ⍉ A

WA ← (1↓⍴B),⍴A
KA ← (⊃⍴⍴A)-1
VA ← ⍳ ⊃ ⍴WA
ZA ← (KA⌽¯1↓VA),¯1↑VA
TA ← ZA⍉WA⍴A

WB ← (¯1↓⍴A),⍴B        ⍝ size-3 vector with shape (3 2 3)
KB ← ⊃ ⍴⍴A             ⍝ 2
VB ← ⍳ ⊃ ⍴WB           ⍝ (1 2 3)

ZB ← (KB+⍳(⊃⍴VB)-KB),KB   ⍝  ((KB≠VB)/VB),KB     ⍝ 1 3 2
⍝ ZB ← (⍳ KB-1),ZB

+/+/+/TA

