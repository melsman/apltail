easter←{                     ⍝ Easter Sunday in year ⍵
    G←1+19|⍵                 ⍝ year "golden number"
    C←1+⌊⍵÷100               ⍝ Century: eg 1984 → 20th
    X← ¯12 + ⌊ C×3÷4         ⍝ yrs in which leap yr omitted
    Z← ¯5 + ⌊ (5+8×C)÷25     ⍝ synch Easter & moon's orbit
    S←(⌊(5×⍵)÷4)-X+10        ⍝ find Sunday
    E←30|(11×G)+20+Z-X       ⍝ Epact
    F←E+(E=24)∨(E=25)∧G>11   ⍝    (when full moon occurs)
    N←(30×F>23)+44-F         ⍝ find full moon
    N←N+7-7|S+N              ⍝ advance to Sunday
    M←3+N>31                 ⍝ month: March or April
    D←N-31×N>31              ⍝ day within month
    10000 100 1+.×⍵ M D      ⍝ yyyymmdd
}

⎕ ← easter ¨ 2014+⍳ 4

0