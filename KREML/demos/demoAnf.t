  $ ./demoAnf.exe <<- EOF
  > fun fac n = let fun fack n k = if n <= 1 then k 1 else fack (n-1) (fn m => k (m * n)) in fack n (fn x => x) end
  > EOF
  fun Cc_1 k n m = 
    let Anf_1 = (m * n) in
    let Anf_2 = k Anf_1 in
  Anf_2
  
  fun fack n k = 
    let Anf_3 = (n <= 1) in
    let Anf_4 = k 1 in
    let Anf_5 = (n - 1) in
    let Anf_6 = fack Anf_5 in
    let Anf_7 = Cc_1 k in
    let Anf_8 = Anf_7 n in
    let Anf_9 = Anf_6 Anf_8 in
    let Anf_10 = if Anf_3 then Anf_4 else Anf_9 in
  Anf_10
  
  fun Ll_1 x = x
  
  fun fac n = 
    let Anf_11 = fack n in
    let Anf_12 = Anf_11 Ll_1 in
  Anf_12
  
  int -> int

