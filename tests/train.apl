a ← (-,÷)5                                 ⍝ +/a ->   ¯4.8

b ← (+,-,÷)5                               ⍝ +/b ->    0.2

f ← { ⍵ + 1 }
t ← f,÷

x ← t 4                                    ⍝ +/x ->    5.25

h ← 4 (++-) 2                              ⍝ h ->      8

minmax ← ⌊/,⌈/

mm ← minmax 3 4 2 3 2 10 2 3 1 3 4         ⍝ +/mm ->  11

⍝ Agh ← (2{⍺/⍵}⍳)3
⍝ Agh ← (2/⍳)3

k ← (⊣ + ⊢)                                ⍝ 2 k 4 ->  6

(+/a) + (+/b) + (+/x) + h + (+/mm) + (2 k 4)  ⍝ TOT:  25.65