  $ ./demoAnf.exe <<- EOF
  > fun fac n = let fun fack n k = if n <= 1 then k 1 else fack (n-1) (fn m => k (m * n)) in fack n (fn x => x) end
  > EOF
  fun Cc_1 k n m = 
    let Anf_1 = (m * n) in
    let Anf_2 = k Anf_1 in
  Anf_2
  
  fun fack n k = 
    let Anf_4 = (n <= 1) in
    let Anf_3 = if Anf_4 then 
      let Anf_5 = k 1 in
  Anf_5 else 
      let Anf_6 = (n - 1) in
      let Anf_7 = fack Anf_6 in
      let Anf_8 = Cc_1 k in
      let Anf_9 = Anf_8 n in
      let Anf_10 = Anf_7 Anf_9 in
  Anf_10 in
  Anf_3
  
  fun Ll_1 x = x
  
  fun fac n = 
    let Anf_11 = fack n in
    let Anf_12 = Anf_11 Ll_1 in
  Anf_12
  
  int -> int

