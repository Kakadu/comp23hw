  $ ./demoInfer.exe <<-EOF
  > let test = 1
  > EOF
  int

  $ ./demoInfer.exe <<-EOF
  > let test = false
  > EOF
  bool

  $ ./demoInfer.exe <<-EOF
  > let test var = 4
  > EOF
  'var0 -> int

  $ ./demoInfer.exe <<-EOF
  > let id var = var
  > EOF
  'var0 -> 'var0

  $ ./demoInfer.exe <<-EOF
  > let app func x = func x
  > EOF
  ('var1 -> 'var2) -> 'var1 -> 'var2

  $ ./demoInfer.exe <<-EOF
  > let func x = y
  > EOF
  Undefined variable 'y'

  $ ./demoInfer.exe <<-EOF
  > let func num = 
  >   let var = 15 * 19 
  >   in 
  >   var + num
  > EOF
  int -> int

  $ ./demoInfer.exe <<-EOF
  > let func num =
  >   let var = false 
  >   in
  >   var + num
  > EOF
  unification failed on bool and int

  $ ./demoInfer.exe <<-EOF
  > let is_positive num =
  >   if num > 0 then true else false
  > EOF
  int -> bool

  $ ./demoInfer.exe <<-EOF
  > let func num predicate =
  >   if num > 0 && predicate then true else false
  > EOF
  int -> bool -> bool

  $ ./demoInfer.exe <<-EOF
  > let func num predicate = 
  >   if num > 0 || predicate then true else false
  > EOF
  int -> bool -> bool

  $ ./demoInfer.exe <<-EOF
  > let func num = if num > 0 then num else false
  > EOF
  unification failed on bool and int

  $ ./demoInfer.exe <<-EOF
  > let add = fun x y -> x + y
  > EOF
  int -> int -> int

  $ ./demoInfer.exe <<-EOF
  > let var_func = fun y z -> (if y > z then true else false)
  > EOF
  'var1 -> 'var1 -> bool

  $ ./demoInfer.exe <<-EOF
  > let fac n = 
  >   let rec helper num acc =
  >     if num = 1 then acc
  >     else helper (num - 1) (acc * num) 
  >   in
  >   helper n 1
  > EOF
  int -> int

  $ ./demoInfer.exe <<-EOF
  > let rec fac n =
  >   if n = 1 then 1 else n * fac (n - 1)
  > EOF
  int -> int

  $ ./demoInfer.exe <<-EOF
  > let func num = 
  > if num > 0 then num else false
  > EOF
  unification failed on bool and int

  $ ./demoInfer.exe <<-EOF
  > let x = 5
  > let test = x
  > EOF
  int
  int

  $ ./demoInfer.exe <<-EOF
  > let x = 5
  > let test x = x + 15
  > EOF
  int
  int -> int

  $ ./demoInfer.exe <<-EOF
  > let id x = x
  > let test x = id x
  > EOF
  'var0 -> 'var0
  'var2 -> 'var2

  $ ./demoInfer.exe <<-EOF
  > let condition x = if x % 2 = 0 then true else false
  > let test x = if condition x then x else x * 2
  > EOF
  int -> bool
  int -> int
