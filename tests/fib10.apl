⍝ Power allows us to calculate a Fibonacci series 
fibo ← {(,⍵),+/¯2↑,⍵}

fib ← {(fibo⍣⍵) 0 1}

⎕ ← fib 10

0