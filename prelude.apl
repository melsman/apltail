$dot ← {
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
  ⍺⍺ / TA ⍵⍵ TB   ⍝ Compute the result
}
