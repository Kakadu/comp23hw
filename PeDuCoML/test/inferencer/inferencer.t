  $ ./inferencer_test.exe <<- EOF
  > let main x y = x + y
  > EOF
  main: int -> int -> int
  $ ./inferencer_test.exe <<- EOF
  > let main (x, y) = x + y
  > EOF
  main: int * int -> int
  $ ./inferencer_test.exe <<- EOF
  > let main ((x, _) :: [('c', "asdf")]) = x
  > EOF
  main: char * string list -> char
  $ ./inferencer_test.exe <<- EOF
  > let add x y = x + y
  > let main = add 1 2 3
  > EOF
  Unification failed: type of the expression is int -> 'e but expected type was int
  $ ./inferencer_test.exe <<- EOF
  > let f arg = match arg with
  >   | [] -> []
  >   | x -> x
  > EOF
  f: 'c list -> 'c list
  $ ./inferencer_test.exe <<- EOF
  > let f arg = match arg with
  >   | [] -> 1
  >   | [x] -> 
  >     (match x with
  >     | (x, y) -> 2)
  >   | x :: y -> 3
  > EOF
  f: 'f * 'g list -> int
  $ ./inferencer_test.exe <<- EOF
  > let f arg = match arg with
  >   | [] -> "aga"
  >   | [x] -> 
  >     (match x with
  >     | (x, y) -> "ugu")
  >   | x :: y -> 
  >     match x with 
  >     | (1, 'c') -> "igi"
  >     | _ -> "ege"
  > EOF
  f: int * char list -> string
  $ ./inferencer_test.exe <<- EOF
  > let f arg = match arg with
  >   | [] -> true
  >   | [x] -> 
  >     (match x with
  >     | (x, y) -> false)
  >   | x :: y -> 
  >     match (x, f y) with 
  >     | (1, 'c') -> true
  >     | _ -> false
  > EOF
  No such variable: f
  $ ./inferencer_test.exe <<- EOF
  > let main = 
  >   let rec even n =
  >     match n with
  >     | 0 -> true
  >     | x -> even (x-1)
  >   and odd n =
  >     match n with
  >     | 0 -> false
  >     | x -> even (x-1)
  >   in even 2
  > EOF
  main: bool
  $ ./inferencer_test.exe <<- EOF
  > let rec id x = x
  > let main = print_int (id id 5) 
  > EOF
  main: bool
