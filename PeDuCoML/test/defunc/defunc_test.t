  $ ./defunc_test.exe <<- EOF
  > let main = fun x -> x
  > EOF
  let `ll_0 x = x
  let main  = `ll_0
  $ ./defunc_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k
  >   in add_k 1 2
  > EOF
  let `ll_0 k x y = (x + y) * k
  let main k = ((`ll_0 k) 1) 2
  $ ./defunc_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k in
  >   let waste_of_space = 0 in
  >   (42 + add_k 42 (-42))
  > EOF
  let `ll_0 k x y = (x + y) * k
  let main k = let waste_of_space = 0 in 42 + (((`ll_0 k) 42) (-42))
  $ ./defunc_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k in
  >   - (add_k 1 2)
  > EOF
  let `ll_0 k x y = (x + y) * k
  let main k = -(((`ll_0 k) 1) 2)
  $ ./defunc_test.exe <<- EOF
  > let f ini = 
  >   let g acc = ini :: acc in
  >   let h (head :: tail) = tail in
  >   g (h [1; 2; 3; 42])
  > EOF
  let `ll_0 ini acc = ini :: acc
  let `ll_1 arg_1 = match arg_1 with | head :: tail -> tail
  let f ini = (`ll_0 ini) (`ll_1 ([1; 2; 3; 42]))
  $ ./defunc_test.exe <<- EOF
  > let fib n = 
  >   let rec fibrec n = 
  >     if n = 0 then 1
  >     else (fibrec (n - 1)) + (fibrec (n - 2))
  >   in fibrec n
  > EOF
  let rec `ll_0 n = if n = 0 then 1 else (`ll_0 (n - 1)) + (`ll_0 (n - 2))
  let fib n = `ll_0 n
  $ ./defunc_test.exe <<- EOF
  > let fac n =
  >   let rec fack n k =
  >     if n <= 1 then k 1
  >     else fack (n - 1) (fun m -> k (m * n))
  >   in
  >   fack n (fun x -> x)
  > EOF
  let `ll_1 n k m = k (m * n)
  let rec `ll_0 n k = if n ≤ 1 then k 1 else (`ll_0 (n - 1)) ((`ll_1 n) k)
  let `ll_2 x = x
  let fac n = (`ll_0 n) `ll_2
  $ ./defunc_test.exe <<- EOF
  > let gen seed1 seed2 = 
  >   let gen n = n * seed2 + seed1 * 42 in
  >   [gen 1; gen 2; gen 3]
  > EOF
  let `ll_0 seed2 seed1 n = (n * seed2) + (seed1 * 42)
  let gen seed1 seed2 = [((`ll_0 seed2) seed1) 1; ((`ll_0 seed2) seed1) 2; ((`ll_0 seed2) seed1) 3]
  $ ./defunc_test.exe <<- EOF
  > let gen seed1 seed2 = 
  >   let gen n = n * seed2 + seed1 * 42 in
  >   gen 0 :: [gen 1; gen 2; gen 3]
  > EOF
  let `ll_0 seed2 seed1 n = (n * seed2) + (seed1 * 42)
  let gen seed1 seed2 = (((`ll_0 seed2) seed1) 0) :: ([((`ll_0 seed2) seed1) 1; ((`ll_0 seed2) seed1) 2; ((`ll_0 seed2) seed1) 3])
  $ ./defunc_test.exe <<- EOF
  > let main x = 
  >   let const f = fun s -> f in
  >   let rev_const f s = const s in
  >   rev_const (fun _ -> x)
  > EOF
  let `ll_0 f s = f
  let `ll_1 f s = `ll_0 s
  let `ll_2 x arg_1 = match arg_1 with | _ -> x
  let main x = `ll_1 (`ll_2 x)
  $ ./defunc_test.exe <<- EOF
  > let count_solutions_of_sq_equation a b c =
  >   let sq x = x * x
  >   in
  >   let d = sq b - 4 * a * c
  >   in
  >   if d > 0 then 2 else (if d = 0 then 1 else 0)
  > 
  > let main = count_solutions_of_sq_equation 2 9 4
  > EOF
  let `ll_0 x = x * x
  let count_solutions_of_sq_equation a b c = let d = (`ll_0 b) - ((4 * a) * c) in if d > 0 then 2 else if d = 0 then 1 else 0
  let main  = ((count_solutions_of_sq_equation 2) 9) 4
  $ ./defunc_test.exe <<- EOF
  > let main x = 
  >   fun z ->
  >   match z with
  >   | y -> x
  > EOF
  let `ll_0 x z = match z with | y -> x
  let main x = `ll_0 x
  $ ./defunc_test.exe <<- EOF
  > let main x = 
  >   fun z ->
  >   match z with
  >   | y -> y
  > EOF
  let `ll_0 z = match z with | y -> y
  let main x = `ll_0
  $ ./defunc_test.exe <<-EOF
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = factorial 6
  > EOF
  let rec factorial n = if n ≤ 1 then 1 else n * (factorial (n - 1))
  let main  = factorial 6
  $ ./defunc_test.exe <<-EOF
  > let rec map f list = match list with
  >   | head :: tail -> f head :: map f tail
  >   | _ -> []
  > 
  > let tuple_map f tuple = match tuple with
  >   | (x, y) -> (f x, f y)
  > 
  > let main = map (tuple_map (fun x -> x * 2)) [(1, 2); (5, 6)]
  > EOF
  let rec map f list = match list with | head :: tail -> (f head) :: ((map f) tail) | _ -> []
  let tuple_map f tuple = match tuple with | x, y -> f x, f y
  let `ll_0 x = x * 2
  let main  = (map (tuple_map `ll_0)) ([1, 2; 5, 6])
  $ ./defunc_test.exe <<- EOF
  > let add_cps x y = fun k -> k (x + y)
  > let square_cps x = fun k -> k (x * x)
  > let pythagoras_cps x y = fun k ->
  >   square_cps x (fun x_squared ->
  >     square_cps y (fun y_squared ->
  >       add_cps x_squared y_squared k))
  > EOF
  let `ll_0 y x k = k (x + y)
  let add_cps x y = (`ll_0 y) x
  let `ll_1 x k = k (x * x)
  let square_cps x = `ll_1 x
  let `ll_4 x_squared k y_squared = ((add_cps x_squared) y_squared) k
  let `ll_3 y k x_squared = (square_cps y) ((`ll_4 x_squared) k)
  let `ll_2 y x k = (square_cps x) ((`ll_3 y) k)
  let pythagoras_cps x y = (`ll_2 y) x
  $ ./defunc_test.exe <<- EOF
  > let thrice_cps f_cps x = fun k ->
  >   f_cps x (fun fx ->
  >     f_cps fx (fun ffx ->
  >       f_cps ffx k))
  > EOF
  let `ll_2 k f_cps ffx = (f_cps ffx) k
  let `ll_1 k f_cps fx = (f_cps fx) ((`ll_2 k) f_cps)
  let `ll_0 x f_cps k = (f_cps x) ((`ll_1 k) f_cps)
  let thrice_cps f_cps x = (`ll_0 x) f_cps
  $ ./defunc_test.exe <<- EOF
  > let main k i = 
  >   match (fun x y -> x + k) with
  >   | f ->
  >     let id = fun x -> x in
  >     f i
  > EOF
  let `ll_0 k x y = x + k
  let `ll_1 x = x
  let main k i = match `ll_0 k with | f -> f i
  $ ./defunc_test.exe <<- EOF
  > let phi n = 
  >   let rec helper last1 last2 n = 
  >     if n > 0 then helper last2 (last1 + last2) (n - 1) 
  >     else last2 
  >   in helper 1 1 (n - 2)
  > 
  > let main = phi 10
  > EOF
  let rec `ll_0 last1 last2 n = if n > 0 then ((`ll_0 last2) (last1 + last2)) (n - 1) else last2
  let phi n = ((`ll_0 1) 1) (n - 2)
  let main  = phi 10
  $ ./defunc_test.exe <<- EOF
  > let f (head :: tail) (x, y) = (x + y + head) :: tail
  > 
  > let main = f [1; 2; 3] (5, 10)
  > EOF
  let f arg_4 arg_5 = match arg_5 with | x, y -> match arg_4 with | head :: tail -> ((x + y) + head) :: tail
  let main  = (f ([1; 2; 3])) (5, 10)
  $ ./defunc_test.exe <<- EOF
  > let f (arg_5 :: arg_4) (x, y) = (x + y + arg_5) :: arg_4
  > 
  > let main = f [1; 2; 3] (5, 10)
  > EOF
  let f arg_6 arg_7 = match arg_7 with | x, y -> match arg_6 with | arg_5 :: arg_4 -> ((x + y) + arg_5) :: arg_4
  let main  = (f ([1; 2; 3])) (5, 10)
