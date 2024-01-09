  $ ./demoANF.exe <<-EOF
  > let x = let g = 123 in g in x
  fn main =
    x@g = 123
    x = x@g
    x

  $ ./demoANF.exe <<-EOF
  > let f x = let g y = let h z w = z + w + x + y in h in g in f
  > EOF
  fn lambda@1 f@g@y, f@x, f@g@h@z, f@g@h@w =
    i@7 = (+ f@g@h@z)
    i@6 = (i@7 f@g@h@w)
    i@5 = (+ i@6)
    i@4 = (i@5 f@x)
    i@3 = (+ i@4)
    i@2 = (i@3 f@g@y)
    i@2
  fn f@g@h f@g@y, f@x, f@g@h@z =
    i@10 = (lambda@1 f@g@h@z)
    i@9 = (i@10 f@x)
    i@8 = (i@9 f@g@y)
    i@8
  fn f@g f@x, f@g@y =
    i@12 = (f@g@h f@x)
    i@11 = (i@12 f@g@y)
    i@11
  fn f f@x =
    i@13 = (f@g f@x)
    i@13
  fn main =
    f

  $ ./demoANF.exe <<-EOF
  > let f y = let g y = y + 1 in g in f
  > EOF
  fn f@g f@g@y =
    i@2 = (+ f@g@y)
    i@1 = (i@2 1)
    i@1
  fn f f@y =
    f@g
  fn main =
    f

  $ ./demoANF.exe <<-EOF
  > let rec fact n = if n <= 0 then 1 else n * fact (n - 1) in fact 4
  > EOF
  fn fact fact@n =
    i@4 = (<= fact@n)
    i@3 = (i@4 0)
    i@2 = if i@3 then
    1
    else  i@6 = (* fact@n)
    i@9 = (- fact@n)
    i@8 = (i@9 1)
    i@7 = (fact i@8)
    i@5 = (i@6 i@7)
    i@5
  
    i@2
  fn main =
    i@1 = (fact 4)
    i@1

  $ ./demoANF.exe <<-EOF
  > let fac n = let rec fack n k = if n <= 0 then k 1 else fack (n-1) (fun m -> k (m * n)) in fack n (fun x -> x) in fac
  > EOF
  fn lambda@2 fac@fack@n, fac@fack@k, fac@fack@m =
    i@7 = (* fac@fack@m)
    i@6 = (i@7 fac@fack@n)
    i@5 = (fac@fack@k i@6)
    i@5
  fn lambda@1 fac@fack@n, fac@fack@n, fac@fack, fac@fack@n, fac@fack@k =
    i@10 = (<= fac@fack@n)
    i@9 = (i@10 0)
    i@8 = if i@9 then
    i@11 = (fac@fack@k 1)
    i@11
    else  i@15 = (- fac@fack@n)
    i@14 = (i@15 1)
    i@13 = (fac@fack i@14)
    i@17 = (lambda@2 fac@fack@k)
    i@16 = (i@17 fac@fack@n)
    i@12 = (i@13 i@16)
    i@12
  
    i@8
  fn fac@fack fac@fack@n =
    i@21 = (lambda@1 fac@fack@n)
    i@20 = (i@21 fac@fack)
    i@19 = (i@20 fac@fack@n)
    i@18 = (i@19 fac@fack@n)
    i@18
  fn lambda@3 fac@x =
    fac@x
  fn fac fac@fack, fac@n =
    i@23 = (fac@fack fac@n)
    i@22 = (i@23 lambda@3)
    i@22
  fn main =
    i@4 = (fac fac@fack)
    i@4
