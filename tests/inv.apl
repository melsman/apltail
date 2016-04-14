⍝ Copyright 2016, Martin Elsman
⍝ MIT License
⍝ 
⍝ Matrix inverse

⍝ [id n] returns an n×n identity matrix
id ← {(⍳⍵)∘.=⍳⍵}

else ← {(⍺⍺⍣⍺)(⍵⍵⍣(~⍺))⍵}

⍝ [n swap1 m] swaps row n in m with the first row
swap1 ← {
  n ← ⍺
  (n = 1) { ⍵ } else {
    row ← 1↑(n-1)↓⍵
    fst ← 1↑⍵
    pre ← 1↓(n-1)↑⍵
    suf ← n↓⍵
    row ⍪ pre ⍪ fst ⍪ suf
  } ⍵
}

swap ← {
  x ← ⍺[1]
  y ← ⍺[2]
  (x=y) { ⍵ } else {
    x swap1 (y swap1 (x swap1 ⍵))
  } ⍵
}

⍝ [normVec v] normalizes a vector to have a leading 1 (assumes non-zero first element)
normVec ← { ⍵ ÷ ⊃ ⍵ }

normFirst ← {
  (normVec 1↑⍵) ⍪ 1↓⍵
}

normByPivot ← {
  v ← ⊃ (⍺-1)↓⍉(⍺-1)↓⍵
  ⍝ v ← (⍵[⍺])[⍺]
  pre ← (⍺-1)↑⍵
  rest ← (⍺-1)↓⍵
  row ← 1↑rest
  suf ← 1↓rest
  pre ⍪ (row ÷ v) ⍪ suf
}

⍝ ⎕ ← 'Tests of normByPivot'
⍝ arg ← 1 ⌽ 5 5 ⍴ ⍳ 8
⍝ ⎕ ← arg
⍝ ⎕ ← 2 normByPivot arg

⍝ [n maxColIdx m] returns the index of the largest element in column n of m
maxColIdx ← {
 c ← ,1↑(⍺-1)↓⍉⍵
 m ← ⌈/c
 ⌈/(⍳⊃⍴⍵)×({⍵=m}¨c) 
}

⍝ ⎕ ← 'Tests of maxColIdx'
⍝ ⎕ ← 1 maxColIdx 2 ⊖ 3 ⌽ 5 5 ⍴ ⍳ 100   ⍝ => 3
⍝ ⎕ ← 3 maxColIdx 1 ⊖ 3 ⌽ 5 5 ⍴ ⍳ 100   ⍝ => 4

⍝ [n maxColIdxSub m] returns the index of the largest element in the
⍝ first column of the lower-right sub-matrix of m (indexed by n)
maxColIdxSub ← {
  ⍺ maxColIdx (⍺-1)↓⍵
}

⍝ ⎕ ← 'Tests of maxColIdxSub'
⍝ ⎕ ← 1 maxColIdxSub 2 ⊖ 3 ⌽ 5 5 ⍴ ⍳ 100   ⍝ => 3
⍝ ⎕ ← 3 maxColIdxSub 1 ⊖ 3 ⌽ 5 5 ⍴ ⍳ 100   ⍝ => 2
⍝ ⎕ ← 5 maxColIdxSub 1 ⊖ 3 ⌽ 5 5 ⍴ ⍳ 100   ⍝ => 1

zerofront ← {
  v ← ,1↑⍉⍵
  m ← v ∘.× ,⍺
  ⍵ - m
}

zeroPivot ← {
  i ← ⍺
  pre ← (i-1)↑⍵
  rest ← (i-1)↓⍵
  suf ← 1↓rest
  r ← 1↑rest
  onPart ← {
    v ← ,1↑(i-1)↓⍉⍵
    m ← v ∘.× ,r
    ⍵ - m
  }
  (onPart pre) ⍪ r ⍪ onPart suf
}

inv ← {
  n ← ⊃ ⍴ ⍵
  R ← ⍵ , id n
  R ← (1 ((⍴R)[2]) ⍴ 1.0) ⍪ R        ⍝ Hack to pass the pivot index around
  looper ← {
    R ← ⍵
    i ← ⌈ ⊃,R                        ⍝ Extract pivot index
    R ← 1↓R                          ⍝ Extract array
    m ← i maxColIdxSub R
    R ← (i,m+i-1) swap R
    R ← i normByPivot R
    R ← i zeroPivot R
    R ← (1 ((⍴R)[2]) ⍴ (i+1.0)) ⍪ R  ⍝ Increase pivot index
  }
  R ← (looper ⍣ n) R
  R ← 1↓R                            ⍝ Extract array
  ⍉n↓⍉R                              ⍝ Cut off identity matrix
}


⍝ Examples
A ← 3 3 ⍴ 1 ⌽ ⍉ ⍳ 9
⎕ ← A
⎕ ← inv A

B ← 2 2 ⍴ 4 7 2 6
⎕ ← B
⎕ ← inv B

⍝ ⎕ ← inv 4 4 ⍴ 4 0 0 0 0 0 2 0 0 1 2 0 1 0 0 1


0