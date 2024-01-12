  $ ./anfTests.exe <<-EOF
  > let fac n =
  > let rec fack n k =
  > if n <= 1 then k 1 else fack (n - 1) (fun m -> k (m * n))
  > in 
  > fack n (fun x -> x)
  > EOF
  let id_1 k n m = let b_op_0 = m * n in
   let app_1 = k b_op_0 in
   app_1
  let rec id_0 n k = let b_op_0 = n <= 1 in
   let if_7 = if b_op_0 then let app_1 = k 1 in
   app_1 else let b_op_2 = n - 1 in
   let app_3 = id_0 b_op_2 in
   let app_4 = id_1 k in
   let app_5 = app_4 n in
   let app_6 = app_3 app_5 in
   app_6 in
   if_7
  let id_2 x = x
  let fac n = let app_0 = id_0 n in
   let app_1 = app_0 id_2 in
   app_1
  $ ./anfTests.exe <<-EOF
  > let fibo n =
  > let rec fibo_cps n acc =
  > if n < 3 then acc 1 else fibo_cps (n - 1) (fun x ->  fibo_cps (n - 2) (fun y -> acc (x + y)))
  > in
  > fibo_cps n (fun x -> x)
  > EOF
  let id_2 acc x y = let b_op_0 = x + y in
   let app_1 = acc b_op_0 in
   app_1
  let id_1 acc fibo_cps n x = let b_op_0 = n - 2 in
   let app_1 = fibo_cps b_op_0 in
   let app_2 = id_2 acc in
   let app_3 = app_2 x in
   let app_4 = app_1 app_3 in
   app_4
  let rec id_0 n acc = let b_op_0 = n < 3 in
   let if_8 = if b_op_0 then let app_1 = acc 1 in
   app_1 else let b_op_2 = n - 1 in
   let app_3 = id_0 b_op_2 in
   let app_4 = id_1 acc in
   let app_5 = app_4 id_0 in
   let app_6 = app_5 n in
   let app_7 = app_3 app_6 in
   app_7 in
   if_8
  let id_3 x = x
  let fibo n = let app_0 = id_0 n in
   let app_1 = app_0 id_3 in
   app_1
  $ ./anfTests.exe <<-EOF
  > let x = (5 + 4) - 2
  > EOF
  let x  = let b_op_0 = 5 + 4 in
   let b_op_1 = b_op_0 - 2 in
   b_op_1
  $ ./anfTests.exe <<-EOF
  > let x = (5 + (4 - 3)) - 2
  > EOF
  let x  = let b_op_0 = 4 - 3 in
   let b_op_1 = 5 + b_op_0 in
   let b_op_2 = b_op_1 - 2 in
   b_op_2
  $ ./anfTests.exe <<-EOF
  > let x = (5 + 4) + (3 + 2)
  > EOF
  let x  = let b_op_0 = 5 + 4 in
   let b_op_1 = 3 + 2 in
   let b_op_2 = b_op_0 + b_op_1 in
   b_op_2
  $ ./anfTests.exe <<-EOF
  > let s1 x =
  > let s2 = x + 5 in
  > let s3 = s2 + 5 in
  > s3
  > EOF
  let s1 x = let b_op_0 = x + 5 in
   let s2 = b_op_0 in
   let b_op_1 = s2 + 5 in
   let s3 = b_op_1 in
   s3
  $ ./anfTests.exe <<-EOF
  > let plus a =
  > let sum b = a + b in
  > sum 5
  > EOF
  let id_0 a b = let b_op_0 = a + b in
   b_op_0
  let plus a = let app_0 = id_0 a in
   let app_1 = app_0 5 in
   app_1
  $ ./anfTests.exe <<-EOF
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = factorial 5
  > EOF
  let rec factorial n = let b_op_0 = n <= 1 in
   let if_4 = if b_op_0 then 1 else let b_op_1 = n - 1 in
   let app_2 = factorial b_op_1 in
   let b_op_3 = n * app_2 in
   b_op_3 in
   if_4
  let main  = let app_0 = factorial 5 in
   app_0
  $ ./anfTests.exe <<-EOF
  > let a c d =
  > let m = c + d in
  > let k l = l + m in
  > k (5 + m)
  > EOF
  let id_0 m l = let b_op_0 = l + m in
   b_op_0
  let a c d = let b_op_0 = c + d in
   let m = b_op_0 in
   let app_1 = id_0 m in
   let b_op_2 = 5 + m in
   let app_3 = app_1 b_op_2 in
   app_3
