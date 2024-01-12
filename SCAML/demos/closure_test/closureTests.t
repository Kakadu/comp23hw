  $ ./closureTests.exe <<-EOF
  > let fac n =
  > let rec fack n k =
  > if n <= 1 then k else fack (n - 1) (fun m -> k (m * n))
  > in
  > fack n (fun x -> x)
  > EOF
  let  fac n  = let rec fack n k  = if (n <= 1) then k else fack (n - 1) (fun k -> (fun n -> (fun m -> k (m * n)))) k n in fack n (fun x -> x)
  $ ./closureTests.exe <<-EOF
  > let a c d =
  > let m = c + d in
  > let k l = l + m in
  > k (5 + m)
  > EOF
  let  a c d  = let  m  = (c + d) in let  k m l  = (l + m) in k m (5 + m)
  $ ./closureTests.exe <<-EOF
  > let fac n =
  > let rec fack n k =
  > if n <= 1 then k else fack (n - 1) ((fun k -> (fun n -> (fun m -> k (m * n)))) k n)
  > in
  > fack n (fun x -> x)
  > EOF
  let  fac n  = let rec fack n k  = if (n <= 1) then k else fack (n - 1) (fun k -> (fun n -> (fun m -> k (m * n)))) k n in fack n (fun x -> x)
  $ ./closureTests.exe <<-EOF
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = factorial 5
  > EOF
  let rec factorial n  = if (n <= 1) then 1 else (n * factorial (n - 1))
  let  main  = factorial 5
  $ ./closureTests.exe <<-EOF
  > let plus a =
  > let sum b = a + b in
  > sum 5
  > EOF
  let  plus a  = let  sum a b  = (a + b) in sum a 5
  $ ./closureTests.exe <<-EOF
  > let s1 x =
  > let s2 = x + 5 in
  > let s3 = s2 + 5 in
  > s3
  > EOF
  let  s1 x  = let  s2  = (x + 5) in let  s3  = (s2 + 5) in s3
  $ ./closureTests.exe <<-EOF
  > let fibo n =
  > let rec fibo_cps n acc =
  > if n < 3 then acc 1 else fibo_cps (n - 1) (fun x ->  fibo_cps (n - 2) (fun y -> acc (x + y)))
  > in
  > fibo_cps n (fun x -> x)
  > EOF
  let  fibo n  = let rec fibo_cps n acc  = if (n < 3) then acc 1 else fibo_cps (n - 1) (fun acc -> (fun fibo_cps -> (fun n -> (fun x -> fibo_cps (n - 2) (fun acc -> (fun x -> (fun y -> acc (x + y)))) acc x)))) acc fibo_cps n in fibo_cps n (fun x -> x)

