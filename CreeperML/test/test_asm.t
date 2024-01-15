+------------------+
|  Factorial test  |
+------------------+
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
  $ chmod +x ../build.sh
  $ cat input.ml | ./test_asm.exe > /dev/null
  $ ocaml input.ml
  120
  $ cm_build/program
  120


+-----------------------------------------------+
|  Maths test (print_int is done with newline)  |
+-----------------------------------------------+
  $ cat > input.ml <<- EOF
  > let () = print_int (5 + 5)
  > let () = print_int (1 + 5 + 3)
  > let () = print_int (1 * 5 - 2)
  > let () = print_int (3 * 9 - 2 + 4)
  > let () = print_int (5 / 3)
  > let () = print_int (5 / 3 + 6 / (2 - 1 + 7) / 2 * 3)
  > EOF
  $ chmod +x ../build.sh
  $ cat input.ml | ./test_asm.exe > /dev/null
  $ ocaml input.ml
  10932911
  $ cm_build/program
  10
  9
  3
  29
  1
  1

+-------------------------+
|  Simple function calls  |
+-------------------------+
  $ cat > input.ml <<- EOF
  > let id2 x = x + 2
  > let id x = (id2 x) + (id2 (x + 1))
  > let () = print_int (id 5)
  > EOF
  $ chmod +x ../build.sh
  $ cat input.ml | ./test_asm.exe > /dev/null
  $ ocaml input.ml
  15
  $ cm_build/program
  15


+----------------------+
|  Factorial with CPS  |
+----------------------+
  $ cat > input.ml <<- EOF
  > let rec fac n k = if n <= 1 then k n else fac (n - 1) (fun m -> k (m * n))
  > let f t = fac t (fun x -> x)
  > let () = print_int (f 7)
  > EOF
  $ chmod +x ../build.sh
  $ cat input.ml | ./test_asm.exe > /dev/null
  $ ocaml input.ml
  5040
  $ cm_build/program
  5040


+-------------------------+
|  Another test with CPS  |
+-------------------------+
  $ cat > input.ml <<- EOF
  > let plus m n k = k (n + m)
  > let mul m n k = k (n * m)
  > let () = plus 1 2 (fun x -> mul 10 x print_int)   
  > EOF
  $ chmod +x ../build.sh
  $ cat input.ml | ./test_asm.exe > /dev/null
  $ ocaml input.ml
  30
  $ cm_build/program
  30


+-----------------------------+
|  Factorial with CPS         |
|  Passing print_int directly |
+-----------------------------+
  $ cat > input.ml <<- EOF
  > let rec fac n k = if n <= 1 then k n else fac (n - 1) (fun m -> k (m * n))
  > let () = fac 6 print_int   
  > EOF
  $ chmod +x ../build.sh
  $ cat input.ml | ./test_asm.exe > /dev/null
  $ ocaml input.ml
  720
  $ cm_build/program
  720


+------------------------+
|  30 fibonacci numbers  |
+------------------------+
  $ cat > input.ml <<- EOF
  > let rec fib n = if n < 2 then 1 else (fib (n - 1)) + (fib (n - 2))
  > let rec repeat fn n =
  >  let () = fn n in  
  >  if n > 0 then (repeat fn (n - 1))
  >  else ()
  > let () = repeat (fun n -> print_int (fib n)) 30   
  > EOF
  $ chmod +x ../build.sh
  $ cat input.ml | ./test_asm.exe > /dev/null
  $ ocaml input.ml
  1346269832040514229317811196418121393750254636828657177111094667654181258415979876103772331448955342113853211
  $ cm_build/program
  1346269
  832040
  514229
  317811
  196418
  121393
  75025
  46368
  28657
  17711
  10946
  6765
  4181
  2584
  1597
  987
  610
  377
  233
  144
  89
  55
  34
  21
  13
  8
  5
  3
  2
  1
  1

+------------------------+
|  Simple tuple example  |
+------------------------+
  $ cat > input.ml <<- EOF
  > let t = (1, 2, 3)
  > let (a, b, c) = t
  > let () = print_int a
  > let () = print_int b
  > let () = print_int c  
  $ chmod +x ../build.sh
  $ cat input.ml | ./test_asm.exe > /dev/null
  $ ocaml input.ml
  123
  $ cm_build/program
  1
  2
  3


+----------------------------+
|  Adding pairs with tuples  |
+----------------------------+
  $ cat > input.ml <<- EOF
  > let add_pair (a, b) (c, d) = (a + c, b + d)
  > let (e, f) = add_pair (1, 2) (3, 4)
  > let () = print_int e
  > let () = print_int f   
  $ chmod +x ../build.sh
  $ cat input.ml | ./test_asm.exe > /dev/null
  $ ocaml input.ml
  46
  $ cm_build/program
  4
  6

+-----------------------+
|  Another CPS example  |
+-----------------------+
  $ cat > input.ml <<- EOF
  > let mul a b k = k (a * b)
  > let forward x k = k x
  > let apply f x = f x
  > let add a b k = k (a + b)
  > let id x = x
  > let () = mul 3 4 (fun x ->
  > add x 5 (fun x ->
  > forward (apply id x) print_int
  > ))
  $ chmod +x ../build.sh
  $ cat input.ml | ./test_asm.exe > /dev/null
  $ ocaml input.ml
  17
  $ cm_build/program
  17


+-----------------------+
|  Partial application  |
+-----------------------+
  $ cat > input.ml <<- EOF
  > let f a b c = let g x y z = x + a + y + z + b + c in g
  > let p1 = f 1 2
  > let p2 = p1 3 4
  > let p3 = p2 5 6
  > let () = print_int p3
  $ chmod +x ../build.sh
  $ cat input.ml | ./test_asm.exe > /dev/null
  $ ocaml input.ml
  21
  $ cm_build/program
  21


+------------------------+
|  Partial applications  |
+------------------------+
  $ cat > input.ml <<- EOF
  > let f a b = let g c d = let h x y = let j z w = a + b + c + d + x + y + z + w in j in h in g
  > let () = print_int (f 1 2 3 4 5 6 7 8) 
  $ chmod +x ../build.sh
  $ cat input.ml | ./test_asm.exe > /dev/null
  $ ocaml input.ml
  36
  $ cm_build/program
  36

+-------------------------+
|  Passing through stack  |
+-------------------------+
  $ cat > input.ml <<- EOF
  > let f rdi rsi rdx rcx r8 r9 stack1 stack2 stack3 = rdi + rsi + rdx + rcx + r8 + r9 + stack1 + stack2 + stack3
  > let () = print_int (f 1 2 3 4 5 6 7 8 9)
  $ chmod +x ../build.sh
  $ cat input.ml | ./test_asm.exe > /dev/null
  $ ocaml input.ml
  45
  $ cm_build/program
  45


+------------------+
|  Stolen CPS Fib  |
+------------------+
  $ cat > input.ml <<- EOF
  > let rec fib n k = if n <= 1 then k n else fib (n - 2) (fun a -> fib (n - 1) (fun b -> k (a + b)))
  > let () = print_int (fib 8 (fun w -> w))
  $ chmod +x ../build.sh
  $ cat input.ml | ./test_asm.exe > /dev/null
  $ ocaml input.ml
  21
  $ cm_build/program
  21


+-----------------------+
|  Test CreeperML logo  |
+-----------------------+
  $ cat > input.ml <<- EOF
  > let x = print_int
  > let () = x 123
  $ chmod +x ../build.sh
  $ cat input.ml | ./test_asm.exe
  
     ___     ___        _____                               __  __ _
    |   |   |   |      / ____|                             |  \/  | |
    |___|___|___|     | |     _ __ ___  ___ _ __   ___ _ __| \  / | |
       _|   |_        | |    | '__/ _ \/ _ \ '_ \ / _ \ '__| |\/| | |
      |  ___  |       | |____| | |  __/  __/ |_) |  __/ |  | |  | | |____
      |_|   |_|        \_____|_|  \___|\___| .__/ \___|_|  |_|  |_|______|
                                           |_|
  👌 Successful run
  $ ocaml input.ml
  123
  $ cm_build/program
  123


  $ cat > input.ml <<- EOF
  > let rec fix f x = f (fix f) x
  > let map f p = let (a,b) = p in (f a, f b)
  > let fixpoly l =
  > fix (fun self l -> map (fun li x -> li (self l) x) l) l
  > let feven p n =
  > let (e, o) = p in
  > if n == 0 then 1 else o (n - 1)
  > let fodd p n =
  > let (e, o) = p in
  > if n == 0 then 0 else e (n - 1)
  > let tie = fixpoly (feven, fodd)
  > let () =
  > let (even,odd) = tie in
  > print_int (odd 1)
  > EOF
  $ chmod +x ../build.sh
  $ cat input.ml | ./test_asm.exe > /dev/null
  $ cm_build/program
  1
  $ ocaml input.ml
  1
