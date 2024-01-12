+------------------+
|  Factorial test  |
------------------+
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
  let ufac_4 = 
    fun (un_0) -> 
    let uhelper_1 =   
      fun (un_2) ->   
      fun (uacc_3) ->   
      if ((<= un_2) 1) then uacc_3 else ((uhelper_1 ((- un_2) 1)) ((* un_2) uacc_3))
    ((uhelper_1 un_0) 1)

-------------------------------------+
|  Here a will be named several ints  |
|    as it is rebound multiple times  |
|    in different contexts            |
-------------------------------------+
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
  let ua_4 = 
    let ua_3 =   
      let ua_2 =     
        let ua_1 =       
          fun (ua_0) ->       
          ((- ua_0) 1)
        (ua_1 4)
      ua_2
    ua_3

---------------------+
|  Reassignment test  |
---------------------+
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
    let ua_0 =   
      33
    let ub_1 =   
      ua_0
    let uc_2 =   
      ub_1
    let ua_3 =   
      23
    let ub_4 =   
      ua_3
    let ud_5 =   
      ub_4
    (print_int ((+ ud_5) ub_4))

-------------------------------------+
|  Here x will become different       |
|  variables, and de-Bruijn naming    |
|  should be correct here             |
|    x is 6 as an argument            |
|    then bound to 7 inside 8         |
|    then bound to 9 in f using 6     |
|    then is rebound twice as 10, 11  |
|    then bound to 14                 |
-------------------------------------+
  $ ./test_db.exe <<- EOF
  > let f x =
  >   let g x = x + 33 in
  >   let x = g x in
  >   let h x = let j x = x - 2 in j (x + 1) in
  >   let x = h x in
  >   x + 2
  > EOF
  let uf_9 = 
    fun (ux_0) -> 
    let ug_2 =   
      fun (ux_1) ->   
      ((+ ux_1) 33)
    let ux_3 =   
      (ug_2 ux_0)
    let uh_7 =   
      fun (ux_4) ->   
      let uj_6 =     
        fun (ux_5) ->     
        ((- ux_5) 2)
      (uj_6 ((+ ux_4) 1))
    let ux_8 =   
      (uh_7 ux_3)
    ((+ ux_8) 2)

-----------------------------------+
|  Test rec and nonrec              |
|  Rec will bind f to itself        |
|  Nonrec will use f defined upper  |
-----------------------------------+
  $ ./test_db.exe <<- EOF
  > let f x = x + 1
  > let f x =
  >   f (x * 2)
  > EOF
  let uf_1 = 
    fun (ux_0) -> 
    ((+ ux_0) 1)
  
  let uf_3 = 
    fun (ux_2) -> 
    (uf_1 ((* ux_2) 2))

  $ ./test_db.exe <<- EOF
  > let f x = x + 1
  > let rec f x =
  >   f (x * 2)
  > EOF
  let uf_1 = 
    fun (ux_0) -> 
    ((+ ux_0) 1)
  
  let uf_2 = 
    fun (ux_3) -> 
    (uf_2 ((* ux_3) 2))

  $ ./test_db.exe <<- EOF
  > let (a, b, c) = (10 + 11, 12 + 13, 14 + 15)
  > let x = (a, (b, c))
  > let (c, (d, e)) = x
  > let res = c + d + e
  > EOF
  let (ua_0, ub_1, uc_2) = 
    (((+ 10) 11), ((+ 12) 13), ((+ 14) 15))
  
  let ux_3 = 
    (ua_0, (ub_1, uc_2))
  
  let (uc_4, (ud_5, ue_6)) = 
    ux_3
  
  let ures_7 = 
    ((+ ((+ uc_4) ud_5)) ue_6)
