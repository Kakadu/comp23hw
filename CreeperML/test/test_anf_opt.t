+-------------------------+
|  ANF Optimization       |
|   1. Reduce move count  |
|   2. ???                |
+-------------------------+

+------------------+
|  Factorial test  |
+------------------+
  $ ./test_anf_opt.exe <<- EOF
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
    let (38) = v(1) v(43) l(1)
    let (42) = if v(38) then
      v(44)
    else
      let (39) = v(2) v(43) l(1)
      let (40) = v(3) v(43) v(44)
      let (41) = v(7) v(39) v(40)
      v(41)
    v(42)
  
  let (10)  (46) = 
    let (45) = v(7) v(46) l(1)
    v(45)

+--------------------------------+
|  A lot of moves test           |
|  All these moves are reduced   |
|  And only few values are left  |
|                                |
|  Reduced to something like:    |
|    v6 = 3                      |
|    v11 = 1                     |
|    v18 = 3 + 1 (interm. v17)   |
+--------------------------------+
  $ ./test_anf_opt.exe <<- EOF
  > let a = 3
  > let b = a
  > let c = b
  > let d = c
  > let e = d
  > 
  > let a = 1
  > let f = a
  > let g = f
  > let h = g
  > let i = h
  > 
  > let res = d + i
  > EOF
  let (6) = l(3)
  
  let (11) = l(1)
  
  let (32) = v(4) v(6) v(11)

+----------+
|  Tuples  |
+----------+
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

+-----------------------------+
|  Simple test with closures  |
+-----------------------------+
  $ ./test_anf_opt.exe <<- EOF
  > let f x =
  >   fun y ->
  >     let g z = 
  >       z + y + x
  >     in g
  > 
  > let ires = f 10 11
  > let res = ires 22
  > EOF
  let (9) [6] [7] (35) = 
    let (33) = v(4) v(35) v(7)
    let (34) = v(4) v(33) v(6)
    v(34)
  
  let (10)  (37) (38) = 
    let (36) = clsr[v(9)][v(37), v(38)]
    v(36)
  
  let (39) = v(10) l(10) l(11)
  
  let (40) = v(39) l(22)

+--------------------------------+
|  Big goody test                |
|  According to my calculations  |
|  It reduced 28 lines of code   |
+--------------------------------+
  $ ./test_anf_opt.exe <<- EOF
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
  
  let (158) = v(3) v(157) l(4)
  
  let (159) = v(4) l(8) v(158)
  
  let (160) = v(3) v(159) l(2)
  
  let (161) = v(3) v(160) v(159)
  
  let (162) = v(3) v(161) v(159)
  
  let (163) = v(4) l(3) v(162)
  
  let (12)  (166) = 
    let (164) = v(3) l(22) v(166)
    let (165) = v(4) l(33) v(164)
    v(165)
  
  let (19) [13] [14] [15] (173) (174) (175) = 
    let (167) = v(3) v(14) v(15)
    let (168) = v(4) v(13) v(167)
    let (169) = v(3) v(173) v(174)
    let (170) = v(2) v(168) v(169)
    let (171) = v(3) l(2) v(175)
    let (172) = v(4) v(170) v(171)
    v(172)
  
  let (20)  (177) (178) (179) = 
    let (176) = clsr[v(19)][v(177), v(178), v(179)]
    v(176)
  
  let (180) = v(4) v(159) v(159)
  
  let (181) = v(20) l(10) l(11) l(12) l(13) l(14) l(15)
  
  let (182) = v(4) v(180) v(181)
  
  let (183) = v(20) l(55) l(66) l(77) l(88)
  
  let (184) = v(4) v(157) v(159)
  
  let (185) = v(2) v(184) v(159)
  
  let (186) = v(12) l(19)
  
  let (187) = v(183) v(163) v(186)
  
  let (188) = v(3) v(159) v(187)
  
  let (189) = v(4) v(185) v(188)
  
  let (190) = v(4) v(189) v(182)
