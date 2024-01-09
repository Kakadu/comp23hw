+------------------+
|  Factorial test  |
+------------------+
  $ ./test_db.exe <<- EOF
  > let fac n =
  > let rec helper n acc =
  > if n <= 1 then 
  > acc
  > else
  > helper (n - 1) (n * acc)
  > in
  > helper n 1
  > EOF
  let 10 = 
    fun (6) -> 
    let 7 =   
      fun (8) ->   
      fun (9) ->   
      if ((v1 v8) 1) then v9 else ((v7 ((v2 v8) 1)) ((v3 v8) v9))
    ((v7 v6) 1)
+-------------------------------------+
|  Here a will be named several ints  |
|    as it is rebound multiple times  |
|    in different contexts            |
+-------------------------------------+
  $ ./test_db.exe <<- EOF
  > let a =
  >   let a =
  >     let a =
  >       let a a =
  >         a - 1 
  >       in
  >       a 4 
  >     in
  >     a 
  >   in
  >   a
  > EOF
  let 10 = 
    let 9 =   
      let 8 =     
        let 7 =       
          fun (6) ->       
          ((v2 v6) 1)
        (v7 4)
      v8
    v9
+---------------------+
|  Reassignment test  |
+---------------------+
  $ ./test_db.exe <<- EOF
  > let () = 
  >   let a = 33 in
  >   let b = a in
  >   let c = b in
  >   let a = 23 in
  >   let b = a in
  >   let d = b in
  >   print_int (d + b)
  > EOF
  let () = 
    let 6 =   
      33
    let 7 =   
      v6
    let 8 =   
      v7
    let 9 =   
      23
    let 10 =   
      v9
    let 11 =   
      v10
    (v5 ((v4 v11) v10))
+-------------------------------------+
|  Here x will become different       |
|  variables, and de-Bruijn naming    |
|  should be correct here             |
|    x is 6 as an argument            |
|    then bound to 7 inside 8         |
|    then bound to 9 in f using 6     |
|    then is rebound twice as 10, 11  |
|    then bound to 14                 |
+-------------------------------------+
  $ ./test_db.exe <<- EOF
  > let f x =
  >   let g x = x + 33 in
  >   let x = g x in
  >   let h x = let j x = x - 2 in j (x + 1) in
  >   let x = h x in
  >   x + 2
  > EOF
  let 15 = 
    fun (6) -> 
    let 8 =   
      fun (7) ->   
      ((v4 v7) 33)
    let 9 =   
      (v8 v6)
    let 13 =   
      fun (10) ->   
      let 12 =     
        fun (11) ->     
        ((v2 v11) 2)
      (v12 ((v4 v10) 1))
    let 14 =   
      (v13 v9)
    ((v4 v14) 2)
+-----------------------------------+
|  Test rec and nonrec              |
|  Rec will bind f to itself        |
|  Nonrec will use f defined upper  |
+-----------------------------------+
  $ ./test_db.exe <<- EOF
  > let f x = x + 1
  > let f x =
  >   f (x * 2)
  > EOF
  let 7 = 
    fun (6) -> 
    ((v4 v6) 1)
  
  let 9 = 
    fun (8) -> 
    (v7 ((v3 v8) 2))
  $ ./test_db.exe <<- EOF
  > let f x = x + 1
  > let rec f x =
  >   f (x * 2)
  > EOF
  let 7 = 
    fun (6) -> 
    ((v4 v6) 1)
  
  let 8 = 
    fun (9) -> 
    (v8 ((v3 v9) 2))
