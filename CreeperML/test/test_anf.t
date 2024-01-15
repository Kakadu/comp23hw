+------------------+i_0
|  Factorial test  |
+------------------+
  $ ./test_anf.exe <<- EOF
  > let fac n =
  > let rec helper n acc =
  > if n <= 1 then 
  > acc
  > else
  > helper (n - 1) (n * acc)
  > in
  > helper n 1
  > EOF
  let (uhelper_1)  (i_8) (i_7) =
    let (i_2) = <= i_8 1
    let (i_6) = if i_2 then
      i_7
    else
      let (i_3) = * i_8 i_7
      let (i_4) = - i_8 1
      let (i_5) = uhelper_1 i_4 i_3
      i_5
    i_6
  
  let (ufac_4)  (i_1) =
    let (i_0) = uhelper_1 i_1 1
    i_0


+------------------------+
|  Simple expr test      |
|  a + b + c             |
|  Converts to:          |
|    v7 = (+) 1          |
|    v8 = 1 + 2          |
|    v9 = (+) (1 + 2)    |
|    v10 = (1 + 2) + 3   |
|    res(v6) = v10       |
+------------------------+
  $ ./test_anf.exe <<- EOF
  > let res = 1 + 2 + 3
  > EOF
  let (i_0) = + 1 2
  
  let (i_1) = + i_0 3

+-----------------------------------+
|  Test with functions              |
|  Converts to:                     |
|  g(11) y(15) [x(6)] =             |
|    v7 = y                         |
|    v13 = (+) x                    |
|    v14 = x + y                    |
|    return x + y                   |
|                                   |
|  f(12) x(17) [no env] =           |
|    v6 = x                         |
|    v16 = alloc clsr. g with [x=x] |
|    v8 = allocated g               |
|    return allocated g             |
|                                   |
|  And then result is calculated    |
|    ...                            |
+-----------------------------------+
  $ ./test_anf.exe <<- EOF
  > let f x =
  >   let g y =
  >     x + y
  >   in g
  > let res = f 10 11
  > EOF
  let (ug_2) [ux_0] (i_4) =
    let (i_3) = + ux_0 i_4
    i_3
  
  let (uf_3)  (i_2) =
    let (i_1) = clsr[ug_2][i_2]
    i_1
  
  let (i_0) = uf_3 10 11

+----------------------------------+
|  If-then-else test               |
|  To conserve the lazy nature     |
|    of if-then-else, ANF blocks   |
|    are left inside the branches  |
|    and condition                 |
|                                  |
|  Converts to:                    |
|    v15 = if                      |
|      v7 = (+) 3                  |
|      v8 = 3 + 5                  |
|      v9 = (<=) (3 + 5)           |
|      v10 = 3 + 5 <= 22           |
|      res is 3 + 5 <= 22          |
|    then                          |
|      v11 = (-) 2                 |
|      v12 = 2 - 3                 |
|      res is 2 - 3                |
|    else                          |
|      v13 = (+) 4                 |
|      v14 = 4 + 5                 |
|      res is 4 + 5                |
|    res is ite(15)                |
|                                  |
+----------------------------------+
  $ ./test_anf.exe <<- EOF
  > let r = if 3 + 5 <= 22 then 2 - 3 else 4 + 5
  > EOF
  let (i_0) = + 3 5
  
  let (i_1) = <= i_0 22
  
  let (i_4) = if i_1 then
    let (i_2) = - 2 3
    i_2
  else
    let (i_3) = + 4 5
    i_3

+-----------------------+
|  More if-then-else    |
|  more for testing pp  |
+-----------------------+
  $ ./test_anf.exe <<- EOF
  > let r = if (if 2 - 10 <= 2 then false else true) then 2 - 3 else (if true then 77 - 33 + 23 else (25 + 11 * 3))
  > EOF
  let (i_0) = - 2 10
  
  let (i_1) = <= i_0 2
  
  let (i_2) = if i_1 then
    false
  else
    true
  
  let (i_9) = if i_2 then
    let (i_3) = - 2 3
    i_3
  else
    let (i_8) = if true then
      let (i_4) = - 77 33
      let (i_5) = + i_4 23
      i_5
    else
      let (i_6) = * 11 3
      let (i_7) = + 25 i_6
      i_7
    i_8

+--------------------------+
|  Tuples                  |
|  Decomposing a tuple is  |
|  done like an index      |
+--------------------------+
  $ ./test_anf.exe <<- EOF
  > let (a, b, c) = (10 + 11, 12 + 13, 14 + 15)
  > let x = (a, (b, c))
  > let (c, (d, e)) = x
  > let res = c + d + e
  > EOF
  let (i_8) = + 14 15
  
  let (i_9) = + 12 13
  
  let (i_10) = + 10 11
  
  let (i_11) = (i_10, i_9, i_8)
  
  let (i_14) = i_11[2]
  
  let (i_13) = i_11[1]
  
  let (i_12) = i_11[0]
  
  let (i_6) = (i_13, i_14)
  
  let (i_7) = (i_12, i_6)
  
  let (i_3) = i_7[1]
  
  let (i_5) = i_3[1]
  
  let (i_4) = i_3[0]
  
  let (i_2) = i_7[0]
  
  let (i_0) = + i_2 i_4
  
  let (i_1) = + i_0 i_5

+------------------+
|  Big goofy test  |
+------------------+
  $ ./test_anf.exe <<- EOF
  > let a = 10 + 11 * 32
  > let b = 8 + a * 4
  > let c = b
  > let d = c
  > let e = 3 + c * 2 * b * b
  > let f x = 33 + 22 * x
  > let g a b c =
  >   let h d e f =
  >      a + (b * c) - d * e + 2 * f
  >   in h
  > let p = c + d + g 10 11 12 13 14 15
  > let s = g 55 66 77 88
  > let res = a + b - c + d * (s e (f 19)) + p
  > EOF
  let (uf_6)  (i_34) =
    let (i_32) = * 22 i_34
    let (i_33) = + 33 i_32
    i_33
  
  let (uh_13) [ua_7] [ub_8] [uc_9] (i_31) (i_30) (i_29) =
    let (i_23) = * 2 i_29
    let (i_24) = * i_31 i_30
    let (i_25) = * ub_8 uc_9
    let (i_26) = + ua_7 i_25
    let (i_27) = - i_26 i_24
    let (i_28) = + i_27 i_23
    i_28
  
  let (ug_14)  (i_22) (i_21) (i_20) =
    let (i_19) = clsr[uh_13][i_22, i_21, i_20]
    i_19
  
  let (i_17) = * 11 32
  
  let (i_18) = + 10 i_17
  
  let (i_15) = * i_18 4
  
  let (i_16) = + 8 i_15
  
  let (i_11) = * i_16 2
  
  let (i_12) = * i_11 i_16
  
  let (i_13) = * i_12 i_16
  
  let (i_14) = + 3 i_13
  
  let (i_8) = ug_14 10 11 12 13 14 15
  
  let (i_9) = + i_16 i_16
  
  let (i_10) = + i_9 i_8
  
  let (i_7) = ug_14 55 66 77 88
  
  let (i_0) = uf_6 19
  
  let (i_1) = i_7 i_14 i_0
  
  let (i_2) = * i_16 i_1
  
  let (i_3) = + i_18 i_16
  
  let (i_4) = - i_3 i_16
  
  let (i_5) = + i_4 i_2
  
  let (i_6) = + i_5 i_10
