  $ ./demoTypeChecker.exe <<-EOF
  > let rec f = fun n -> f in f
  > EOF
  Occurs check failed

  $ ./demoTypeChecker.exe <<-EOF
  > (225 + 20 * 10) / 5
  > EOF
  int

  $ ./demoTypeChecker.exe <<-"EOF"
  > let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 4
  > EOF
  int
