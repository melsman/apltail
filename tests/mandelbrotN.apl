⍝ grid-size in left argument (e.g., (1024 768))
⍝ X-range, Y-range in right argument

mandelbrot ← {
  X ← ⊃⍺                                 ⍝ e.g., 1024
  Y ← ⊃1↓⍺                               ⍝ e.g., 768
  xRng ← 2↑⍵ 
  yRng ← 2↓⍵
  dx ← ((xRng[2])-xRng[1]) ÷ X
  dy ← ((yRng[2])-yRng[1]) ÷ Y
  cx ← Y X ⍴ (xRng[1]) + dx × ⍳X         ⍝ real plane
  cy ← ⍉ X Y ⍴ (yRng[1]) + dy × ⍳Y       ⍝ img plane
  mandel1 ← {                            ⍝ perform one iteration of mandelbrot - vectorized
    zx ← Y X ⍴ ⍵[1]                      ⍝ real plane
    zy ← Y X ⍴ ⍵[2]                      ⍝ imaginary plane
    count ← Y X ⍴ ⍵[3]                   ⍝ count plane
    zzx ← cx + (zx × zx) - zy × zy 
    zzy ← cy + (zx × zy) + zx × zy
    conv ← 4 > (zzx × zzx) + zzy × zzy
    count2 ← count + 1 - conv
    (zzx zzy count2)
  }
  pl ← Y X ⍴ 0                           ⍝ zero-plane
  N ← 90                                 ⍝ iterations
  res ← (mandel1 ⍣ N) (pl pl pl)
  res[3] ÷ N                             ⍝ return normalized count plane
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