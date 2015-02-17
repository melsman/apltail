A←1↓⍳10000
primes ← (1=+⌿0=A∘.|A)/A

⎕← ⌈/ primes
