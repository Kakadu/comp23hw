  $ ./closure_test.exe <<- EOF
  > let main = fun x -> x
  > EOF
  let main  = fun x -> x
  $ ./closure_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k
  >   in add_k 1 2
  > EOF
  let main k = let add_k k x y = (x + y) * k in ((add_k k) 1) 2
  $ ./closure_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k in
  >   let waste_of_space = 0 in
  >   (42 + add_k 42 (-42))
  > EOF
  let main k = let add_k k x y = (x + y) * k in let waste_of_space  = 0 in 42 + (((add_k k) 42) (-42))
  $ ./closure_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k in
  >   - (add_k 1 2)
  > EOF
  let main k = let add_k k x y = (x + y) * k in -(((add_k k) 1) 2)
  $ ./closure_test.exe <<- EOF
  > let f ini = 
  >   let g acc = ini :: acc in
  >   let h (head :: tail) = tail in
  >   g (h [1; 2; 3; 42])
  > EOF
  let f ini = let g ini acc = ini :: acc in let h head :: tail = tail in (g ini) (h ([1; 2; 3; 42]))
  $ ./closure_test.exe <<- EOF
  > let a c d =
  >   let m = c + d in
  >   let k l = l + m in
  >   k (5 + m)
  > EOF
  let a c d = let m  = c + d in let k m l = l + m in (k m) (5 + m)
  $ ./closure_test.exe <<- EOF
  > let fac n =
  >   let rec fack n k =
  >     if n <= 1 then k 1
  >     else fack (n - 1) (fun m -> k (m * n))
  >   in
  >   fack n (fun x -> x)
  > EOF
  let fac n = let rec fack n k = if n ≤ 1 then k 1 else (fack (n - 1)) (((fun n k m -> k (m * n)) n) k) in (fack n) (fun x -> x)
  $ ./closure_test.exe <<- EOF
  > let gen seed1 seed2 = 
  >   let gen n = n * seed2 + seed1 * 42 in
  >   [gen 1; gen 2; gen 3]
  > EOF
  let gen seed1 seed2 = let gen seed2 seed1 n = (n * seed2) + (seed1 * 42) in [((gen seed2) seed1) 1; ((gen seed2) seed1) 2; ((gen seed2) seed1) 3]
  $ ./closure_test.exe <<- EOF
  > let gen seed1 seed2 = 
  >   let gen n = n * seed2 + seed1 * 42 in
  >   gen 0 :: [gen 1; gen 2; gen 3]
  > EOF
  let gen seed1 seed2 = let gen seed2 seed1 n = (n * seed2) + (seed1 * 42) in (((gen seed2) seed1) 0) :: ([((gen seed2) seed1) 1; ((gen seed2) seed1) 2; ((gen seed2) seed1) 3])
  $ ./closure_test.exe <<- EOF
  > let main x = 
  >   let const f = fun s -> f in
  >   let rev_const f s = const s in
  >   rev_const (fun _ -> x)
  > EOF
  let main x = let const f s = f in let rev_const f s = const s in rev_const ((fun x _ -> x) x)
  $ ./closure_test.exe debug <<- EOF
  > let main x = 
  >   fun z ->
  >   match z with
  >   | y -> x
  > EOF
  let main x = (fun x z -> match z with | y -> x) x
  $ ./closure_test.exe <<-EOF
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = factorial 6
  > EOF
  let rec factorial n = if n ≤ 1 then 1 else n * (factorial (n - 1))
  let main  = factorial 6
  $ ./closure_test.exe <<-EOF
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
  let main  = (map (tuple_map (fun x -> x * 2))) ([1, 2; 5, 6])
  $ ./closure_test.exe debug <<- EOF
  > let add_cps x y = fun k -> k (x + y)
  > let square_cps x = fun k -> k (x * x)
  > let pythagoras_cps x y = fun k ->
  >   square_cps x (fun x_squared ->
  >     square_cps y (fun y_squared ->
  >       add_cps x_squared y_squared k))
  > EOF
  let add_cps x y = ((fun y x k -> k (x + y)) y) x
  let square_cps x = (fun x k -> k (x * x)) x
  let pythagoras_cps x y = ((fun y x k -> (square_cps x) (((fun y k x_squared -> (square_cps y) (((fun x_squared k y_squared -> ((add_cps x_squared) y_squared) k) x_squared) k)) y) k)) y) x
  $ ./closure_test.exe <<- EOF
  > let thrice_cps f_cps x = fun k ->
  >   f_cps x (fun fx ->
  >     f_cps fx (fun ffx ->
  >       f_cps ffx k))
  > EOF
  let thrice_cps f_cps x = ((fun x f_cps k -> (f_cps x) (((fun k f_cps fx -> (f_cps fx) (((fun k f_cps ffx -> (f_cps ffx) k) k) f_cps)) k) f_cps)) x) f_cps
  $ ./closure_test.exe <<- EOF
  > let main k i = 
  >   match (fun x y -> x + k) with
  >   | f ->
  >     let id = fun x -> x in
  >     f i
  > EOF
  let main k i = match (fun k x y -> x + k) k with | f -> let id x = x in f i
  $ ./closure_test.exe <<- EOF
  > let count_solutions_of_sq_equation a b c =
  >   let sq x = x * x
  >   in
  >   let d = sq b - 4 * a * c
  >   in
  >   if d > 0 then 2 else (if d = 0 then 1 else 0)
  > 
  > let main = count_solutions_of_sq_equation 2 9 4
  > EOF
  let count_solutions_of_sq_equation a b c = let sq x = x * x in let d  = (sq b) - ((4 * a) * c) in if d > 0 then 2 else if d = 0 then 1 else 0
  let main  = ((count_solutions_of_sq_equation 2) 9) 4
