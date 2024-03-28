Show help
  $ ./jaml.exe -help
  jaml -i -d -f -ll -x86_64 <file>
  Compile program from stdin
    -i Infer only the types for the input, do not use the compiler.
    -f Read program from specified file, not from the stdin.
    -d Disable occurrence checking during type checking
    -ll Compilation with Llvm
    -x86_64 Compilation with x86_64
    -help  Display this list of options
    --help  Display this list of options
  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let main = print_int 1
  > EOF
  1

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let main = print_int (-1)
  > EOF
  -1

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let main = print_bool true
  > EOF
  true

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let main = print_bool false
  > EOF
  false

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let main = print_int (1 + 2)
  > EOF
  3

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let main = print_int ((1 + 2 * 3 + 3)/ 5)
  > EOF
  2

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let main = print_bool (10 > 9)
  > EOF
  true

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let main = print_bool (10 < 9)
  > EOF
  false


  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let main = print_bool (-10 < 9)
  > EOF
  true

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let main = print_bool (-10 < 9 && 10 > 9)
  > EOF
  true

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let main = print_bool (-10 > 9 || 10 < 9)
  > EOF
  false

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let main = print_bool (-10 > 9 ^ 10 > 9)
  > EOF
  true

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let rec range n m = 
  > let cond = n >= m in
  > if cond
  >    then (let _ = print_int n in 
  >             print_bool (cond)) 
  >    else (let _ = print_int n in 
  >             let _ = print_bool (cond) in 
  >             range (n + 1) m)
  > let main = print_int (range 1 3)
  > EOF
  1
  false
  2
  false
  3
  true
  0

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let test f x y = if f x y then x + 1 + 3 else y + 4 + 5
  > let f x y = (x > 0) && (y <> 0)
  > let main = print_int (test f 1 2)
  > EOF
  5

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let rec fib n =
  > if n < 3 then 1 else fib (n - 1) + fib (n - 2)
  > let main = print_int (fib 6)
  > EOF
  8

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let id a = a
  > let id_pa = id 
  > let main = print_int (id_pa 2)
  > EOF
  2

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = print_bool (factorial 6 = 720)
  > EOF
  true

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = print_int (factorial 6)
  > EOF
  720

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let rec factorial n acc = if n < 1 then acc else factorial (n - 1) (n * acc)
  > let fact_tailrec n = factorial n 1
  > let main = print_int (fact_tailrec 6)
  > EOF
  720

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let x = (1 + 2, 3 + 4)
  > let (a, b) = x
  > let main = print_int (a + b)
  > EOF
  10

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let fst (a, b) = a
  > let main = print_int (fst (1,2))
  > EOF
  1

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let sum a b = (fun c d e f -> a + d + e + f)
  > let partial = sum 1 2 3 4 5 6
  > let main = print_int (partial)
  > EOF
  16

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let sum6 a b c d e f = a + b + c + d + e + f
  > let sum4 a b c d = sum6 a b c d
  > let sum2 a b = sum4 a b
  > let res = sum2 1 2 3 4 5 6
  > let main = print_int (res)
  > EOF
  21

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let (a, b) = (50, 2)
  > let main = print_int (a + b)
  > EOF
  52

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let sum_cortage ((a, b), (d, e)) = a + b + d + e
  > let main = print_int (sum_cortage ((10, 15), (1337, 7331)))
  > EOF
  8693

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let fibo n =
  > let rec fibo_cps n acc =
  > if n < 3 then acc 1 else fibo_cps (n - 1) (fun x ->  fibo_cps (n - 2) (fun y -> acc (x + y)))
  > in
  > fibo_cps n (fun x -> x)
  > let main = print_int (fibo 11)
  > EOF
  89

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let fac n =
  > let rec fack n k =
  > if n <= 1 then k 1
  > else fack (n-1) ((fun k n m -> k (m * n)) k n) 
  > in
  > fack n (fun x -> x)
  > let main = print_int (fac 7)
  > EOF
  5040

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let test x y z = (if x > 1 then (if y > 0 then 1 else 2) else (if z > 0 then 3 else 4))
  > let main = print_int (test 1 2 3)
  > EOF
  3

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let sum a b = a + b
  > let minus a b = a - b
  > let f a = if a > 1 then sum else minus
  > let f_res = f 0 3 4
  > let main = print_int (f_res)
  > EOF
  -1

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let x c (a,b) =
  >   let sum (c, d) = (a + b, c + d) in  
  >   sum (c, 13)
  > let scd (_, b) = b
  > let main = print_int (scd (x 101 (10, 145)))
  > EOF
  114

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 = a1 + a2 - a3 * a4 / a5 + a6 - a7 * a8 / a9 + a10 - a11 * a12 / a13 + a14 - a15 * a16
  > let x = f 
  > let pa = x 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
  > let main = print_int (pa)
  > EOF
  -225

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 = 
  >     a1 + a2 - a3 * a4 / a5 + a6 - a7 * a8 / a9 + a10 - a11 * a12 / a13 + a14 - a15 * a16
  > let x = f 
  > let pa = x 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  > let main = print_int (pa)
  > EOF
  1

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let f _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ a16 = a16
  > let x = f 
  > let pa = x 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 143
  > let main = print_int (pa)
  > EOF
  143

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let sum_cortage ((a, b), (d, _), _, (x, y)) = a + b + d + x + y
  > let x = sum_cortage 
  > let pa = x ((10, 20), (10, 20), 10, (15, 14))
  > let main = print_int (pa)
  > EOF
  69

  $ ./jaml.exe -x86_64 <<- EOF && ./jaml
  > let apply20 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 =
  >   a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20
  > let main = print_int (apply20 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
  > EOF
  210
