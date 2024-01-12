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
  let (luhelper_1)  (i_8) (i_7) =
    let (uacc_3) = i_7
    let (un_2) = i_8
    let (i_2) = <= un_2 1
    let (i_6) = if i_2 then
      uacc_3
    else
      let (i_3) = * un_2 uacc_3
      let (i_4) = - un_2 1
      let (i_5) = uhelper_1 i_4 i_3
      i_5
    i_6
  
  let (lufac_4)  (i_1) =
    let (un_0) = i_1
    let (uhelper_1) = luhelper_1
    let (i_0) = uhelper_1 un_0 1
    i_0
  
  let (ufac_4) = lufac_4

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
  
  let (ures_0) = i_1

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
  let (lug_2) [ux_0] (i_4) =
    let (uy_1) = i_4
    let (i_3) = + ux_0 uy_1
    i_3
  
  let (luf_3)  (i_2) =
    let (ux_0) = i_2
    let (i_1) = clsr[lug_2][ux_0]
    let (ug_2) = i_1
    ug_2
  
  let (uf_3) = luf_3
  
  let (i_0) = uf_3 10 11
  
  let (ures_4) = i_0

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
  
  let (ur_0) = i_4

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
  
  let (ur_0) = i_9

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
  
  let (uc_2) = i_14
  
  let (i_13) = i_11[1]
  
  let (ub_1) = i_13
  
  let (i_12) = i_11[0]
  
  let (ua_0) = i_12
  
  let (i_6) = (ub_1, uc_2)
  
  let (i_7) = (ua_0, i_6)
  
  let (ux_3) = i_7
  
  let (i_3) = ux_3[1]
  
  let (i_5) = i_3[1]
  
  let (ue_6) = i_5
  
  let (i_4) = i_3[0]
  
  let (ud_5) = i_4
  
  let (i_2) = ux_3[0]
  
  let (uc_4) = i_2
  
  let (i_0) = + uc_4 ud_5
  
  let (i_1) = + i_0 ue_6
  
  let (ures_7) = i_1

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
  let (luh_13) [ua_7] [ub_8] [uc_9] (i_34) (i_33) (i_32) =
    let (uf_12) = i_32
    let (ue_11) = i_33
    let (ud_10) = i_34
    let (i_26) = * 2 uf_12
    let (i_27) = * ud_10 ue_11
    let (i_28) = * ub_8 uc_9
    let (i_29) = + ua_7 i_28
    let (i_30) = - i_29 i_27
    let (i_31) = + i_30 i_26
    i_31
  
  let (lug_14)  (i_25) (i_24) (i_23) =
    let (uc_9) = i_23
    let (ub_8) = i_24
    let (ua_7) = i_25
    let (i_22) = clsr[luh_13][ua_7, ub_8, uc_9]
    let (uh_13) = i_22
    uh_13
  
  let (luf_6)  (i_21) =
    let (ux_5) = i_21
    let (i_19) = * 22 ux_5
    let (i_20) = + 33 i_19
    i_20
  
  let (i_17) = * 11 32
  
  let (i_18) = + 10 i_17
  
  let (ua_0) = i_18
  
  let (i_15) = * ua_0 4
  
  let (i_16) = + 8 i_15
  
  let (ub_1) = i_16
  
  let (uc_2) = ub_1
  
  let (ud_3) = uc_2
  
  let (i_11) = * uc_2 2
  
  let (i_12) = * i_11 ub_1
  
  let (i_13) = * i_12 ub_1
  
  let (i_14) = + 3 i_13
  
  let (ue_4) = i_14
  
  let (uf_6) = luf_6
  
  let (ug_14) = lug_14
  
  let (i_8) = ug_14 10 11 12 13 14 15
  
  let (i_9) = + uc_2 ud_3
  
  let (i_10) = + i_9 i_8
  
  let (up_15) = i_10
  
  let (i_7) = ug_14 55 66 77 88
  
  let (us_16) = i_7
  
  let (i_0) = uf_6 19
  
  let (i_1) = us_16 ue_4 i_0
  
  let (i_2) = * ud_3 i_1
  
  let (i_3) = + ua_0 ub_1
  
  let (i_4) = - i_3 uc_2
  
  let (i_5) = + i_4 i_2
  
  let (i_6) = + i_5 up_15
  
  let (ures_17) = i_6
