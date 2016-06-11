⍝ Mandelbrot
⍝ grid-size in left argument (e.g., (1024 768))
⍝ X-range, Y-range in right argument

mandelbrot ← {
  X ← ⊃⍺                                 ⍝ e.g., 1024
  Y ← ⊃1↓⍺                               ⍝ e.g., 768
  xRng ← 2↑⍵ 
  yRng ← 2↓⍵
  dx ← ((xRng[2])-xRng[1]) ÷ X
  dy ← ((yRng[2])-yRng[1]) ÷ Y
  cxA ← Y X ⍴ (xRng[1]) + dx × ⍳X        ⍝ real plane
  cyA ← ⍉ X Y ⍴ (yRng[1]) + dy × ⍳Y      ⍝ img plane
  N ← 90                                 ⍝ iterations
  mandel1 ← {
    cx ← ⍺
    cy ← ⍵
    f ← {
      arg ← 3 ⍴ ⍵
      x ← arg[1]                         ⍝ real value
      y ← arg[2]                         ⍝ imaginary value
      count ← arg[3]
      zx ← cx+(x×x)-(y×y)
      zy ← cy+(x×y)+(x×y)
      conv ← 4 > (zx × zx) + zy × zy
      count2 ← count + 1 - conv
      (zx zy count2)
    }
    res ← (f ⍣ N) (0 0 0)                ⍝ perform N iteration of a single mandelbrot point
    res[3]
  }
  res ← cxA mandel1¨ cyA
  res ÷ N
}

layout ← {
  arr ← '$#Oo*=+:- '
  arr[⍵+1]
}

norm ← { layout¨ ⌈9 × ⍵ }

mandelWrap ← {
  ⎕ ← norm (⌽⍵) ⍴ ⍵ mandelbrot ¯2 0.75 ¯0.75 0.75
}

mandelWrap 20 12

0