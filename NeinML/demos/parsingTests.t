  $ ./demoParse.exe <<-EOF
  > let test = 1
  > EOF
  [(Define ("test", (Value (VInt 1))))]

  $ ./demoParse.exe <<-EOF
  > let test = true
  > EOF
  [(Define ("test", (Value (VBool true))))]

  $ ./demoParse.exe <<-EOF
  > let then = true
  > EOF
  Parsing error

  $ ./demoParse.exe <<-EOF
  > let test = 1 + 2 * 3 - 10
  > EOF
  [(Define ("test",
      (Add ((Value (VInt 1)),
         (Sub ((Mul ((Value (VInt 2)), (Value (VInt 3)))), (Value (VInt 10))))
         ))
      ))
    ]

  $ ./demoParse.exe <<-EOF
  > let test = 1 + 2 * 3 / 10 - 10 % 5
  > EOF
  [(Define ("test",
      (Add ((Value (VInt 1)),
         (Sub (
            (Mul ((Value (VInt 2)), (Div ((Value (VInt 3)), (Value (VInt 10))))
               )),
            (Mod ((Value (VInt 10)), (Value (VInt 5))))))
         ))
      ))
    ]

  $ ./demoParse.exe <<-EOF
  > let test var = 10
  > EOF
  [(Define ("test", (Func ("var", (Value (VInt 10))))))]

  $ ./demoParse.exe <<-EOF
  > let id var = var
  > EOF
  [(Define ("id", (Func ("var", (Variable "var")))))]

  $ ./demoParse.exe <<-EOF
  > let id var = var < 10 && var < 5 || var <> 4
  > EOF
  [(Define ("id",
      (Func ("var",
         (Or (
            (And ((Less ((Variable "var"), (Value (VInt 10)))),
               (Less ((Variable "var"), (Value (VInt 5)))))),
            (NotEqual ((Variable "var"), (Value (VInt 4))))))
         ))
      ))
    ]

  $ ./demoParse.exe <<-EOF
  > let id var = (var < 10 && var < 5) || var <> 4
  > EOF
  [(Define ("id",
      (Func ("var",
         (Or (
            (And ((Less ((Variable "var"), (Value (VInt 10)))),
               (Less ((Variable "var"), (Value (VInt 5)))))),
            (NotEqual ((Variable "var"), (Value (VInt 4))))))
         ))
      ))
    ]

  $ ./demoParse.exe <<-EOF
  > let id var = (var < 10 && var < 5) || var <> 4
  > EOF
  [(Define ("id",
      (Func ("var",
         (Or (
            (And ((Less ((Variable "var"), (Value (VInt 10)))),
               (Less ((Variable "var"), (Value (VInt 5)))))),
            (NotEqual ((Variable "var"), (Value (VInt 4))))))
         ))
      ))
    ]

  $ ./demoParse.exe <<-EOF
  > let var x = if x > 15 || x < 10 then x else 9
  > EOF
  [(Define ("var",
      (Func ("x",
         (IfThenElse (
            (Or ((More ((Variable "x"), (Value (VInt 15)))),
               (Less ((Variable "x"), (Value (VInt 10)))))),
            (Variable "x"), (Value (VInt 9))))
         ))
      ))
    ]

  $ ./demoParse.exe <<-EOF
  > let var x = fun y z -> (x + y * z)
  > EOF
  [(Define ("var",
      (Func ("x",
         (Func ("y",
            (Func ("z",
               (Add ((Variable "x"), (Mul ((Variable "y"), (Variable "z")))))))
            ))
         ))
      ))
    ]

  $ ./demoParse.exe <<-EOF
  > let var_func = fun y z -> (if y > z then true else false)
  > EOF
  [(Define ("var_func",
      (Func ("y",
         (Func ("z",
            (IfThenElse ((More ((Variable "y"), (Variable "z"))),
               (Value (VBool true)), (Value (VBool false))))
            ))
         ))
      ))
    ]

  $ ./demoParse.exe <<-EOF
  > let rec func x = 
  > if x > 15 then x else func (x + x)
  > EOF
  [(RecDefine ("func",
      (Func ("x",
         (IfThenElse ((More ((Variable "x"), (Value (VInt 15)))),
            (Variable "x"),
            (Apply ((Variable "func"), (Add ((Variable "x"), (Variable "x")))))
            ))
         ))
      ))
    ]

  $ ./demoParse.exe <<-EOF
  > let func x =
  >   let test1 = 15 
  >   in
  >   let rec test2 y =
  >     if y > 10 then 30 + test1 
  >     else test1 + test2 (y + 1) 
  >   in
  >   test1 + test2 x
  > EOF
  [(Define ("func",
      (Func ("x",
         (LetIn ("test1", (Value (VInt 15)),
            (RecLetIn ("test2",
               (Func ("y",
                  (IfThenElse ((More ((Variable "y"), (Value (VInt 10)))),
                     (Add ((Value (VInt 30)), (Variable "test1"))),
                     (Add ((Variable "test1"),
                        (Apply ((Variable "test2"),
                           (Add ((Variable "y"), (Value (VInt 1))))))
                        ))
                     ))
                  )),
               (Add ((Variable "test1"),
                  (Apply ((Variable "test2"), (Variable "x")))))
               ))
            ))
         ))
      ))
    ]

  $ ./demoParse.exe <<-EOF
  > let rec fac n = if n = 1 then 1 else n * fac (n - 1)
  > EOF
  [(RecDefine ("fac",
      (Func ("n",
         (IfThenElse ((Equal ((Variable "n"), (Value (VInt 1)))),
            (Value (VInt 1)),
            (Mul ((Variable "n"),
               (Apply ((Variable "fac"),
                  (Sub ((Variable "n"), (Value (VInt 1))))))
               ))
            ))
         ))
      ))
    ]

  $ ./demoParse.exe <<-EOF
  > let fac n =
  >   let rec helper num acc =
  >     if num = 1 then acc 
  >     else helper (num - 1) (acc * num)
  >   in
  >   helper n 1
  > EOF
  [(Define ("fac",
      (Func ("n",
         (RecLetIn ("helper",
            (Func ("num",
               (Func ("acc",
                  (IfThenElse ((Equal ((Variable "num"), (Value (VInt 1)))),
                     (Variable "acc"),
                     (Apply (
                        (Apply ((Variable "helper"),
                           (Sub ((Variable "num"), (Value (VInt 1)))))),
                        (Mul ((Variable "acc"), (Variable "num")))))
                     ))
                  ))
               )),
            (Apply ((Apply ((Variable "helper"), (Variable "n"))),
               (Value (VInt 1))))
            ))
         ))
      ))
    ]

  $ ./demoParse.exe <<-EOF
  > let simple_func x = x + 1
  > let if_func x = if x > 0 then true else false
  > let val = 15
  > EOF
  [(Define ("simple_func",
      (Func ("x", (Add ((Variable "x"), (Value (VInt 1))))))));
    (Define ("if_func",
       (Func ("x",
          (IfThenElse ((More ((Variable "x"), (Value (VInt 0)))),
             (Value (VBool true)), (Value (VBool false))))
          ))
       ));
    (Define ("val", (Value (VInt 15))))]
