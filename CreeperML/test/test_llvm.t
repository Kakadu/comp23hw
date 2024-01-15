  $ cat > input.ml <<- EOF
  > let fac n =
  >   let rec helper n acc =
  >     if n <= 1 then 
  >       acc
  >     else
  >       helper (n - 1) (n * acc)
  >     in
  >   helper n 1
  > let () = print_int (fac 5)
  > EOF
  $ ocaml input.ml
  120
  $ cat input.ml | ./test_llvm.exe
  $ llc-16 --relocation-model=pic test-opt.ll
  $ gcc test-opt.s ../lib/bindings.co -o test-opt
  $ ./test-opt
  120

  $ cat > input.ml <<- EOF
  > let (a, b, c, d) = (1, 2, 3, 4)
  > let sum = a + b + c + d
  > let () = print_int sum
  > EOF
  $ ocaml input.ml
  10
  $ cat input.ml | ./test_llvm.exe
  $ llc-16 --relocation-model=pic test-opt.ll
  $ gcc test-opt.s ../lib/bindings.co -o test-opt
  $ ./test-opt
  10

  $ cat > input.ml <<- EOF
  > let id a = a
  > let () = print_int (id (id (id (id (727)))))
  > EOF
  $ ocaml input.ml
  727
  $ cat input.ml | ./test_llvm.exe
  $ llc-16 --relocation-model=pic test-opt.ll
  $ gcc test-opt.s ../lib/bindings.co -o test-opt
  $ ./test-opt
  727

  $ cat > input.ml <<- EOF
  > let foo a b = a + b
  > let baz a = foo 1 a
  > let () = print_int (baz 9)
  > EOF
  $ ocaml input.ml
  10
  $ cat input.ml | ./test_llvm.exe
  $ llc-16 --relocation-model=pic test-opt.ll
  $ gcc test-opt.s ../lib/bindings.co -o test-opt
  $ ./test-opt
  10

  $ cat > input.ml <<- EOF
  > let rec fac n k = 
  >   if n <= 1 then 
  >     k n 
  >   else 
  >     fac (n - 1) (fun m -> k (m * n))
  > let f t = fac t (fun x -> x)
  > let () = print_int (f 7)
  > EOF
  $ ocaml input.ml
  5040
  $ cat input.ml | ./test_llvm.exe
  $ llc-16 --relocation-model=pic test-opt.ll
  $ gcc test-opt.s ../lib/bindings.co -o test-opt
  $ ./test-opt
  5040

  $ cat > input.ml <<- EOF
  > let f a b = 
  >   let g c d = 
  >     let h x y = 
  >       let j z w = 
  >         a + b + c + d + x + y + z + w 
  >       in j 
  >     in h 
  >   in 
  >   g
  > let () = print_int (f 1 2 3 4 5 6 7 8) 
  > EOF
  $ ocaml input.ml
  36
  $ cat input.ml | ./test_llvm.exe
  $ llc-16 --relocation-model=pic test-opt.ll
  $ gcc test-opt.s ../lib/bindings.co -o test-opt
  $ ./test-opt
  36

  $ cat > input.ml <<- EOF
  > let fib n = 
  >   let rec helper a b n = 
  >     if n > 0 then 
  >       helper b (a + b) (n - 1)
  >     else
  >       a
  >     in
  >   helper 0 1 n
  > let () = print_int (fib 5)
  > EOF
  $ ocaml input.ml
  5
  $ cat input.ml | ./test_llvm.exe
  $ llc-16 --relocation-model=pic test-opt.ll
  $ gcc test-opt.s ../lib/bindings.co -o test-opt
  $ ./test-opt
  5

  $ cat > input.ml <<- EOF
  > let fib n =
  >   let rec helper n a b =
  >     if n == 0 then
  >       a
  >     else 
  >       helper (n - 1) b (a + b)
  >   in 
  >   helper n 0 1
  > let () = print_int (fib 5)
  > EOF
  $ ocaml input.ml
  5
  $ cat input.ml | ./test_llvm.exe
  $ llc-16 --relocation-model=pic test-opt.ll
  $ gcc test-opt.s ../lib/bindings.co -o test-opt
  $ ./test-opt
  5

  $ cat > input.ml <<- EOF
  > let rec fix f x = f (fix f) x
  > let fib n =
  >   let helper self n a b =
  >     if n == 0 then
  >       a
  >     else 
  >       self (n - 1) b (a + b)
  >   in 
  >   fix helper n 0 1
  > let () = print_int (fib 5)
  > EOF
  $ ocaml input.ml
  5
  $ cat input.ml | ./test_llvm.exe
  $ llc-16 --relocation-model=pic test-opt.ll
  $ gcc test-opt.s ../lib/bindings.co -o test-opt
  $ ./test-opt
  5
