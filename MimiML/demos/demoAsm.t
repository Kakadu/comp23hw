Simple tests
  $ cat > test.ml <<- EOF
  > print_int 5
  > EOF
  $ cat test.ml | ./demoAsm.exe > /dev/null
  $ ocaml test.ml
  5
  $ compiled/program
  5

  $ cat > test.ml <<- EOF
  > let x = 10 in let y = 5 in print_int (x + y)
  > EOF
  $ cat test.ml | ./demoAsm.exe > /dev/null
  $ ocaml test.ml
  15
  $ compiled/program
  15

  $ cat > test.ml <<- EOF
  > print_bool true
  > EOF
  $ cat test.ml | ./demoAsm.exe > /dev/null
  $ compiled/program
  true

  $ cat > test.ml <<- EOF
  > let add_and_square x y =
  >   let square z = z * z in
  >   let sum = x + y in
  >   square sum
  > in 
  > print_int (add_and_square 2 3)
  > EOF
  $ cat test.ml | ./demoAsm.exe > /dev/null
  $ ocaml test.ml
  25
  $ compiled/program
  25

Simple factorial

  $ cat > test.ml <<- EOF
  > let rec fact n = 
  >   if n <= 0 then 1 
  >   else n * fact (n - 1) 
  > in 
  > print_int (fact 4)
  > EOF
  $ cat test.ml | ./demoAsm.exe > /dev/null
  $ ocaml test.ml
  24
  $ compiled/program
  24

CPS factorial

  $ cat > test.ml <<- EOF
  > let rec fact n k = 
  >   if n <= 1 then k n 
  >   else fact (n - 1) (fun m -> k (m * n)) 
  > in 
  > print_int (fact 10 (fun x -> x))
  > EOF
  $ cat test.ml | ./demoAsm.exe > /dev/null
  $ ocaml test.ml
  3628800
  $ compiled/program
  3628800

Passing many arguments 
  $ cat > test.ml <<- EOF
  > let f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
  >   print_int (a2 + a9) in
  > f 1 2 3 4 5 6 7 8 9 10
  > EOF
  $ cat test.ml | ./demoAsm.exe > /dev/null
  $ ocaml test.ml
  11
  $ compiled/program
  11

Partial application
  $ cat > test.ml <<- EOF
  > let f a b c =
  >   let g x y z = 
  >     x + a + y + z + b + c 
  >   in g
  >   in
  >   let p1 = f 1 2 in
  >   let p2 = p1 3 4 in
  >   let p3 = p2 5 6 in
  > print_int p3
  > EOF
  $ cat test.ml | ./demoAsm.exe > /dev/null
  $ ocaml test.ml
  21
  $ compiled/program
  21
