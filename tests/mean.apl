mean ← {      ⍝ Arithmetic mean
    sum←+/⍵
    num←⍴⍵
    sum÷⊃num
}

mean 1 2 3 4