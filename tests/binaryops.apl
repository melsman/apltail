else ← {(⍺⍺⍣⍺)(⍵⍵⍣(~⍺))⍵}

tst ← {
  s ← 'Test of ', ⍵[1]
  r ← ⍵[4] = ⍵[2] ⍺⍺ ⍵[3]
  waste ← r { ⎕ ← s,': OK' ⋄ ⍵ + 1 } else { ⎕ ← s,': ERR' ⋄ ⍵ + 2 } 3
  r
}

t1a ← ⎕INT32OR tst ('or_1' 1 2 3)
t1b ← ⎕INT32OR tst ('or_2' 4 5 5)
t2a ← ⎕INT32AND tst ('and_1' 3 4 0)
t2b ← ⎕INT32AND tst ('and_2' 3 5 1)
t3a ← ⎕INT32XOR tst ('xor_1' 3 5 6)
t3b ← ⎕INT32XOR tst ('xor_2' 0 5 5)

t5a ← ⎕INT32SHL tst ('shl_1' 1 10 1024)
t5b ← ⎕INT32SHL tst ('shl_2' 1 31 ¯2147483648)

t6a ← ⎕INT32SHR tst ('shr_1' 1024 10 1)
t6b ← ⎕INT32SHR tst ('shr_2' ¯2147483648 30 2)

t7a ← ⎕INT32SHAR tst ('shar_1' 1024 10 1)
t7b ← ⎕INT32SHAR tst ('shar_2' ¯1024 1 ¯512)

0