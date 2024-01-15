  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/llvm/runtime.so
  > fun foo x y = x + y
  > val main = print_int (foo 1 2)
  > EOF
  3

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/llvm/runtime.so
  > fun sub x = x - 1
  > val sub2 = sub 2
  > val main = print_int sub2
  > EOF
  1

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/llvm/runtime.so
  > val main = print_int (let val x = 5 in x end)
  > EOF
  5

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/llvm/runtime.so
  > fun f x = if x < 7 then 1 else 0
  > val main = print_int (f 5)
  > EOF
  1

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/llvm/runtime.so
  > val main = print_int (8 / 2 * 3 + 6)
  > EOF
  18

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/llvm/runtime.so
  > fun max2 a b = if (a > b) then a else b
  > fun max3 a b c = max2 c (max2 a b)
  > val main = print_int (max3 4 8 2)
  > EOF
  8

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/llvm/runtime.so
  > val main = print_int (6 + 4)
  > EOF
  10

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/llvm/runtime.so
  > fun xor l r = (l andalso not r) orelse (r andalso not l)
  > val main = print_bool (xor true false)
  > EOF
  true

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/llvm/runtime.so
  > val id = fn x => x
  > val main = print_int (id 66)
  > EOF
  66

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/llvm/runtime.so
  > val d = fn x => fn y => x * y > 10
  > val main = print_bool (d 4 5)
  > EOF
  true

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/llvm/runtime.so
  > fun fac n = let fun fack n k = if n <= 1 then k 1 else fack (n-1) (fn m => k (m * n)) in fack n (fn x => x) end
  > val main = print_int (fac 6)
  > EOF
  720

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/llvm/runtime.so
  > fun y = let fun f x = x * 2 in f 5 end
  > val main = print_int y
  > EOF
  10
 
  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/llvm/runtime.so
  > fun sum a b c = a + b + c
  > val main = print_int (sum 1 2 3)
  > EOF
  6

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/llvm/runtime.so
  > fun fact n = if n <= 1 then 1 else n * fact (n - 1)
  > val main = print_int (fact 6)
  > EOF
  720
 
  $ ./demoLlvm.exe <<-EOF
  > fun fact a b = a + b + b
  > val main = fact 3
  > EOF
  int -> int

  $ ./demoLlvm.exe <<- EOF
  > fun fact a b c = a + b + c
  > val sum1 = fact 1
  > val x = sum1 2 3
  > val main = x + true
  > EOF
  Unification failed: type of the expression is bool but expected type was int

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/llvm/runtime.so
  > fun fact self n = if n <= 1 then 1 else n * self (n - 1)
  > fun fix f x = f (fix f) x 
  > val main = print_int (fix fact 6)
  > EOF
  720
