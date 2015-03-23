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

$out ← {
  A ← ⍺
  B ← ⍵
  T ← (⊃(⍴⍴A))⌽⍳⊃⍴(⍴B),⍴A
  x ← T ⍉ ((⍴B),(⍴A)) ⍴ A
  y ← ((⍴A),(⍴B)) ⍴ B
  x ⍺⍺ y
}

$slashbar ← {
  ⍉ ⍺⍺ / ⍉ ⍵
}

$log ← {
  ⍝ Dyadic logarithm
  (⍟⍵)÷⍟⍺
}

ReadCSVDouble ← ⎕ReadDoubleVecFile
ReadCSVInt ← ⎕ReadIntVecFile
xor ← ⎕INT32XOR
and ← ⎕INT32AND
sll ← ⎕INT32SHL
srl ← ⎕INT32SHR
testBit ← { 0≠⍵ and 1 sll (⍺-1) }

now ← ⎕NOW

bench ← {
  g ← ⍺⍺
  f ← { ⍵ ⋄ ⌷ g 0 }
  t0 ← now 0
  r ← (f ⍣ ⍵) 0
  t1 ← now 1
  r,(t1-t0)÷⍵
}
