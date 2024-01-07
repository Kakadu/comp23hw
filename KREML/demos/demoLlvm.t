  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > fun foo x y = x + y
  > val foo_12 = foo 1 2
  > EOF

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > fun sub x = x - 1
  > val sub2 = sub 2
  > EOF

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > val y = let val x = 5 in x end
  > EOF

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > fun f x = if x < 7 then 1 else 0
  > val f5 = f
  > EOF

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > val x = 8 / 2 * 3 + 6
  > EOF

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > val x = false
  > EOF

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > val x = ~6
  > EOF

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > val x = true andalso false
  > EOF

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > fun fact n = if n <= 1 then 1 else n * fact (n - 1)
  > EOF

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > val identity = fn x => x
  > EOF

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > val compare = fn x => fn y => x * y > 10
  > EOF

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > fun fac n = let fun fack n k = if n <= 1 then k 1 else fack (n-1) (fn m => k (m * n)) in fack n (fn x => x) end
  > val fat3 = fac 3
  > EOF

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > fun y = let fun f x = x * 2 in f 5 end
  > EOF

  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > val y = let fun f x = x * 2 in f 5 end
  > EOF
 
  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/runtime.so
  > fun sum a b c = a + b + c
  > val sum123 = sum 1 2 3
  > EOF
 
  $ ./demoLlvm.exe <<- EOF | lli-16 -load ../lib/runtime.so
  > fun fact n = if n <= 1 then 1 else n * fact (n - 1)
  > val main = fact 3
  > EOF
  