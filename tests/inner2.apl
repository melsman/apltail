A ← 3 2 ⍴ ⍳ 5   ⍝ Example input A
B ← ⍉ A         ⍝ Example input B

WA ← (1↓⍴B),⍴A
KA ← (⊃⍴⍴A)-1
VA ← ⍳ ⊃ ⍴WA
ZA ← (KA⌽¯1↓VA),¯1↑VA
TA ← ZA⍉WA⍴A

WB ← (¯1↓⍴A),⍴B
KB ← ⊃ ⍴⍴A 
VB ← ⍳ ⊃ ⍴WB
ZB0 ← (-KB) ↓ KB ⌽ ⍳(⊃⍴VB)  
ZB ← (¯1↓(⍳ KB)),ZB0,KB
TB ← ZB⍉WB⍴B

R ← +/ 3 3 2 ⍴ TA×TB

R2 ← ×/ +/ R

⍝       1  3  5
⍝       2  4  1
⍝
⍝ 1 2   5 11  7  -+->    23
⍝ 3 4  11 25 19  -+->    55
⍝ 5 1   7 19 26  -+->    52
⍝                     65780
