inner ← { 
  WA ← (1↓⍴⍵),⍴⍺
  KA ← (⊃⍴⍴⍺)-1
  VA ← ⍳ ⊃ ⍴WA
  ZA ← (KA⌽¯1↓VA),¯1↑VA
  TA ← ZA⍉WA⍴⍺    ⍝ Replicate, transpose
  WB ← (¯1↓⍴⍺),⍴⍵
  KB ← ⊃ ⍴⍴⍺
  VB ← ⍳ ⊃ ⍴WB
  ZB0 ← (-KB) ↓ KB ⌽ ⍳(⊃⍴VB)  
  ZB ← (¯1↓(⍳ KB)),ZB0,KB
  TB ← ZB⍉WB⍴⍵    ⍝ Replicate, transpose
  + / TA × TB     ⍝ Compute final array
}

A ← 3 2 ⍴ ⍳ 5   ⍝ Example input A
B ← ⍉ A         ⍝ Example input B

R ← A inner B

R2 ← ×/ +/ R

⍝       1  3  5
⍝       2  4  1
⍝                           
⍝ 1 2   5 11  7  -+->    23  |
⍝ 3 4  11 25 19  -+->    55  ×
⍝ 5 1   7 19 26  -+->    52  |
⍝                     65780  v
