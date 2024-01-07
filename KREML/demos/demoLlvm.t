  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/runtime.so
  > fun foo x y = x + y
  > val main = foo 1 2
  > EOF
  [3]


  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/runtime.so
  > fun sub x = x - 1
  > val sub2 = sub 2
  > val main = sub2
  > EOF
  [1]


  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/runtime.so
  > val main = let val x = 5 in x end
  > EOF
  [5]


  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/runtime.so
  > fun f x = if x < 7 then 1 else 0
  > val main = f 5
  > EOF
  [1]


  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/runtime.so
  > val main = 8 / 2 * 3 + 6
  > EOF
  [18]


  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/runtime.so
  > fun max2 a b = if (a > b) then a else b
  > fun max3 a b c = max2 c (max2 a b)
  > val main = max3 4 8 2
  > EOF
  [8]


  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/runtime.so
  > val main = 6 + 4
  > EOF
  [10]


  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/runtime.so
  > fun xor l r = (l andalso not r) orelse (r andalso not l)
  > val main = xor true false
  > EOF
  [1]


  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/runtime.so
  > val id = fn x => x
  > val main = id 66
  > EOF
  [66]

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/runtime.so
  > val d = fn x => fn y => x * y > 10
  > val main = d 4 5
  > EOF
  [1]


  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/runtime.so
  > fun fac n = let fun fack n k = if n <= 1 then k 1 else fack (n-1) (fn m => k (m * n)) in fack n (fn x => x) end
  > val main = fac 3
  > EOF
  [6]


  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/runtime.so
  > fun y = let fun f x = x * 2 in f 5 end
  > val main = y
  > EOF
  [10]
 
  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/runtime.so
  > fun sum a b c = a + b + c
  > val main = sum 1 2 3
  > EOF
  [6]

 
  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/runtime.so
  > fun fact n = if n <= 1 then 1 else n * fact (n - 1)
  > val main = fact 3
  > EOF
  [6]
