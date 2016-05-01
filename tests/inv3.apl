
inv3 ← {
 x ← ,⍵
 a ← x[1] ⋄ b ← x[2] ⋄ c ← x[3]
 d ← x[4] ⋄ e ← x[5] ⋄ f ← x[6]
 g ← x[7] ⋄ h ← x[8] ⋄ i ← x[9]
 det ← ((a×((e×i)-f×h)) - (b×((d×i)-f×g))) + c×((d×h)-e×g)
 adj ← 3 3 ⍴ ((e×i)-f×h) ((c×h)-b×i) ((b×f)-c×e) ((f×g)-d×i) ((a×i)-c×g) ((c×d)-a×f) ((d×h)-e×g) ((b×g)-a×h) ((a×e)-b×d)
 adj ÷ det
}

⍝ Examples
A ← ⌷ 3 3 ⍴ 1 ⌽ ⍉ ⍳ 9
⎕ ← A

B ← ⌷ inv3 A
⎕ ← B
0