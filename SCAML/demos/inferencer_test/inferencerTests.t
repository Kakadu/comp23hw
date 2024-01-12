  $ ./inferencerTests.exe <<-EOF
  > let rec factorial n = if n <= 1  then 1 else factorial (n - 1) * n
  > let x5 = factorial 5 
  > EOF
  factorial : int -> int
  x5 : int
  $ ./inferencerTests.exe <<-EOF
  > let rec fibonacci n = if n <= 1 then 1 else fibonacci (n - 1) + fibonacci (n - 2)
  > let x5 = fibonacci 5 
  > EOF
  fibonacci : int -> int
  x5 : int
  $ ./inferencerTests.exe <<-EOF
  > let fack1 k n m = k (n * m)
  > let rec fack n k = if n <= 1 then k 1 else fack (n-1) (fack1 k n)
  > let id x = x
  > let fac n = fack n id
  > EOF
  fack1 : (int -> 'd) -> int -> int -> 'd
  fack : int -> (int -> 'm) -> 'm
  id : 'n -> 'n
  fac : int -> int
  $ ./inferencerTests.exe <<-EOF
  > let id x = x
  > let acc1 acc x y = acc (x + y)
  > let acc2 fib_func n acc x = fib_func (n - 2) (acc1 acc x)
  > let rec fibo_cps n acc = if n < 3 then acc 1 else fibo_cps (n - 1) (acc2 fibo_cps n acc)
  > let fibo n = fibo_cps n id
  > EOF
  id : 'a -> 'a
  acc1 : (int -> 'e) -> int -> int -> 'e
  acc2 : (int -> (int -> 'k) -> 'n) -> int -> (int -> 'k) -> int -> 'n
  fibo_cps : int -> (int -> 'y) -> 'y
  fibo : int -> int
  $ ./inferencerTests.exe <<-EOF
  > let pr = print_int 1
  > EOF
  pr : ()
