+------------------+
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
  let (7)  (43) (44) = 
    let (8) = v(43)
    let (9) = v(44)
    let (38) = v(1) v(8) l(1)
    let (42) = if v(38) then
      v(9)
    else
      let (39) = v(2) v(8) l(1)
      let (40) = v(3) v(8) v(9)
      let (41) = v(7) v(39) v(40)
      v(41)
    v(42)
  
  let (10)  (46) = 
    let (6) = v(46)
    let (45) = v(7) v(6) l(1)
    v(45)

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
  let (16) = v(4) l(1) l(2)
  
  let (17) = v(4) v(16) l(3)
  
  let (6) = v(17)

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
  let (8) [6] (25) = 
    let (7) = v(25)
    let (24) = v(4) v(6) v(7)
    v(24)
  
  let (9)  (27) = 
    let (6) = v(27)
    let (26) = clsr[v(8)][v(6)]
    let (8) = v(26)
    v(8)
  
  let (28) = v(9) l(10) l(11)
  
  let (10) = v(28)

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
  let (27) = v(4) l(3) l(5)
  
  let (28) = v(1) v(27) l(22)
  
  let (31) = if v(28) then
    let (29) = v(2) l(2) l(3)
    v(29)
  else
    let (30) = v(4) l(4) l(5)
    v(30)
  
  let (6) = v(31)

+-----------------------+
|  More if-then-else    |
|  more for testing pp  |
+-----------------------+
  $ ./test_anf.exe <<- EOF
  > let r = if (if 2 - 10 <= 2 then false else true) then 2 - 3 else (if true then 77 - 33 + 23 else (25 + 11 * 3))
  > EOF
  let (45) = v(2) l(2) l(10)
  
  let (46) = v(1) v(45) l(2)
  
  let (47) = if v(46) then
    l(false)
  else
    l(true)
  
  let (54) = if v(47) then
    let (48) = v(2) l(2) l(3)
    v(48)
  else
    let (53) = if l(true) then
      let (49) = v(2) l(77) l(33)
      let (50) = v(4) v(49) l(23)
      v(50)
    else
      let (51) = v(3) l(11) l(3)
      let (52) = v(4) l(25) v(51)
      v(52)
    v(53)
  
  let (6) = v(54)

+--------------------------+
|  Tuples                  |
|  Decomposing a tuple is  |
|  done like an index      |
+--------------------------+
  $ ./test_anf_opt.exe <<- EOF
  > let (a, b, c) = (10 + 11, 12 + 13, 14 + 15)
  > let x = (a, (b, c))
  > let (c, (d, e)) = x
  > let res = c + d + e
  > EOF
  let (45) = v(4) l(10) l(11)
  
  let (46) = v(4) l(12) l(13)
  
  let (47) = v(4) l(14) l(15)
  
  let (48) = (v(45), v(46), v(47))
  
  let (49) = v(48)[0]
  
  let (50) = v(48)[1]
  
  let (51) = v(48)[2]
  
  let (52) = (v(50), v(51))
  
  let (53) = (v(49), v(52))
  
  let (54) = v(53)[0]
  
  let (55) = v(53)[1]
  
  let (56) = v(55)[0]
  
  let (57) = v(55)[1]
  
  let (58) = v(4) v(54) v(56)
  
  let (59) = v(4) v(58) v(57)

+------------------+
|  Big goody test  |
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
  let (156) = v(3) l(11) l(32)
  
  let (157) = v(4) l(10) v(156)
  
  let (6) = v(157)
  
  let (158) = v(3) v(6) l(4)
  
  let (159) = v(4) l(8) v(158)
  
  let (7) = v(159)
  
  let (8) = v(7)
  
  let (9) = v(8)
  
  let (160) = v(3) v(8) l(2)
  
  let (161) = v(3) v(160) v(7)
  
  let (162) = v(3) v(161) v(7)
  
  let (163) = v(4) l(3) v(162)
  
  let (10) = v(163)
  
  let (12)  (166) = 
    let (11) = v(166)
    let (164) = v(3) l(22) v(11)
    let (165) = v(4) l(33) v(164)
    v(165)
  
  let (19) [13] [14] [15] (173) (174) (175) = 
    let (16) = v(173)
    let (17) = v(174)
    let (18) = v(175)
    let (167) = v(3) v(14) v(15)
    let (168) = v(4) v(13) v(167)
    let (169) = v(3) v(16) v(17)
    let (170) = v(2) v(168) v(169)
    let (171) = v(3) l(2) v(18)
    let (172) = v(4) v(170) v(171)
    v(172)
  
  let (20)  (177) (178) (179) = 
    let (13) = v(177)
    let (14) = v(178)
    let (15) = v(179)
    let (176) = clsr[v(19)][v(13), v(14), v(15)]
    let (19) = v(176)
    v(19)
  
  let (180) = v(4) v(8) v(9)
  
  let (181) = v(20) l(10) l(11) l(12) l(13) l(14) l(15)
  
  let (182) = v(4) v(180) v(181)
  
  let (21) = v(182)
  
  let (183) = v(20) l(55) l(66) l(77) l(88)
  
  let (22) = v(183)
  
  let (184) = v(4) v(6) v(7)
  
  let (185) = v(2) v(184) v(8)
  
  let (186) = v(12) l(19)
  
  let (187) = v(22) v(10) v(186)
  
  let (188) = v(3) v(9) v(187)
  
  let (189) = v(4) v(185) v(188)
  
  let (190) = v(4) v(189) v(21)
  
  let (23) = v(190)
