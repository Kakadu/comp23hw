+------------------------------...
|  Format:
|  closure: letc name arg [captured vars list] = ...
|  calling closure: clsr[name][given args]
+------------------------------...

+------------------+
|  Factorial test  |
+------------------+
  $ ./test_closures.exe <<- EOF
  > let fac n =
  > let rec helper n acc =
  > if n <= 1 then 
  > acc
  > else
  > helper (n - 1) (n * acc)
  > in
  > helper n 1
  > EOF
  letc luhelper_1 (un_2) (uacc_3) =
    if ((<= un_2) 1) then uacc_3 else ((uhelper_1 ((- un_2) 1)) ((* un_2) uacc_3))
  
  letc lufac_4 (un_0) =
    let uhelper_1 =  
      luhelper_1
    ((uhelper_1 un_0) 1)
  
  let ufac_4 =
    lufac_4

+-------------------------+
|  Simple function with   |
|  no variables from env  |
+-------------------------+
  $ ./test_closures.exe <<- EOF
  > let f x =
  >   let g y = y + 1 in
  >   g 
  > EOF
  letc lug_2 (uy_1) =
    ((+ uy_1) 1)
  
  letc luf_3 (ux_0) =
    let ug_2 =  
      lug_2
    ug_2
  
  let uf_3 =
    luf_3

+----------------------------------------+
|  Simple function capturing environment |
|  x is bound to 6 and gived to g (10)   |
+----------------------------------------+
  $ ./test_closures.exe <<- EOF
  > let f x =
  >   let g y = y + x + 1 in
  >   g 
  > EOF
  letc lug_2 (uy_1) =
    ((+ ((+ uy_1) ux_0)) 1)
  
  letc luf_3 (ux_0) =
    let ug_2 =  
      clsr[lug_2][ux_0]
    ug_2
  
  let uf_3 =
    luf_3

+--------------------------------------------------+
|  A lot of closures and partial applications      |
|  For each inner function a closure will be done  |
+--------------------------------------------------+
  $ ./test_closures.exe <<- EOF
  > let q a =
  >   let w s =
  >     let e d =
  >       let r f =
  >         let t g =
  >           let y h =
  >             a + s + d + f + g + h in
  >           y in
  >         t in
  >       r in
  >     e in
  >   w
  > let p = q 1 2 3 4 5 6
  > EOF
  letc luy_6 (uh_5) =
    ((+ ((+ ((+ ((+ ((+ ua_0) us_1)) ud_2)) uf_3)) ug_4)) uh_5)
  
  letc lut_7 (ug_4) =
    let uy_6 =  
      clsr[luy_6][ua_0, ud_2, uf_3, ug_4, us_1]
    uy_6
  
  letc lur_8 (uf_3) =
    let ut_7 =  
      clsr[lut_7][ua_0, ud_2, uf_3, us_1]
    ut_7
  
  letc lue_9 (ud_2) =
    let ur_8 =  
      clsr[lur_8][ua_0, ud_2, us_1]
    ur_8
  
  letc luw_10 (us_1) =
    let ue_9 =  
      clsr[lue_9][ua_0, us_1]
    ue_9
  
  letc luq_11 (ua_0) =
    let uw_10 =  
      clsr[luw_10][ua_0]
    uw_10
  
  let uq_11 =
    luq_11
  
  let up_12 =
    ((((((uq_11 1) 2) 3) 4) 5) 6)

  $ ./test_closures.exe <<- EOF
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
