  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let fibo n =
  > let rec fibo_cps n acc =
  > if n < 3 then acc 1 else fibo_cps (n - 1) (fun x ->  fibo_cps (n - 2) (fun y -> acc (x + y)))
  > in
  > fibo_cps n (fun x -> x)
  > let main = fibo 11
  > EOF
  [89]
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let fac n =
  > let rec fack n k =
  > if n <= 1 then k 1 else fack (n - 1) (fun m -> k (m * n))
  > in 
  > fack n (fun x -> x)
  > let main = fac 5
  > EOF
  [120]
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let main = (5 + 4) - 2
  > EOF
  [7]
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let x = (5 + (4 - 3)) - 2
  > let main = x
  > EOF
  [4]
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let main = (5 + 4) + (3 + 2)
  > EOF
  [14]
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let s1 x =
  > let s2 = x + 5 in
  > let s3 = s2 + 5 in
  > s3
  > let main = s1 10
  > EOF
  [20]
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let plus a =
  > let sum b = a + b in
  > sum 5
  > let main = plus 5
  > EOF
  [10]
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = factorial 5
  > EOF
  [120]

  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let rec foo a1  a1  a1  a1  a1  a1  a1  a1  a1  a1  a1  a1  a1 = a1
  > let main = foo 1
  > EOF
  [120]
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let rec foo    a b c d e f g h i j k l m n  o p q  = a
  > let main = foo 1 2 3 4 5 6 7 8 9 0 1 2 3 4  5 6 7
  > EOF
  [120]
