  $ ./llvm_test.exe <<- EOF | tee llvm_test.ll ; echo "---" ; lli-16 -load ../../runtime/peducoml_runtime.so llvm_test.ll
  > let main = print_int 42
  > EOF
  declare i64 @concat_strings(i64, i64)
  declare i64 @compare_strings_lte(i64, i64)
  declare i64 @compare_strings_lt(i64, i64)
  declare i64 @compare_strings_gte(i64, i64)
  declare i64 @compare_strings_gt(i64, i64)
  declare i64 @compare_strings_neq(i64, i64)
  declare i64 @compare_strings_eq(i64, i64)
  declare i64 @compare_tuples_lte(i64, i64)
  declare i64 @compare_tuples_lt(i64, i64)
  declare i64 @compare_tuples_gte(i64, i64)
  declare i64 @compare_tuples_gt(i64, i64)
  declare i64 @compare_tuples_neq(i64, i64)
  declare i64 @compare_tuples_eq(i64, i64)
  declare i64 @concat_lists(i64, i64)
  declare i64 @compare_lists_lte(i64, i64)
  declare i64 @compare_lists_lt(i64, i64)
  declare i64 @compare_lists_gte(i64, i64)
  declare i64 @compare_lists_gt(i64, i64)
  declare i64 @compare_lists_neq(i64, i64)
  declare i64 @compare_lists_eq(i64, i64)
  declare i64 @print_new_line()
  declare i64 @print_string(i64)
  declare i64 @print_tuple(i64)
  declare i64 @print_list(i64)
  declare i64 @print_bool(i64)
  declare i64 @print_char(i64)
  declare i64 @print_int(i64)
  declare i64 @peducoml_tuple_field(i64, i64)
  declare i64 @peducoml_fill_tuple(i64, i64)
  declare i64 @peducoml_divide(i64, i64)
  declare i64 @peducoml_alloc_tuple(i64)
  declare i64 @peducoml_list_length(i64)
  declare i64 @peducoml_tail(i64)
  declare i64 @peducoml_list_field(i64, i64)
  declare i64 @peducoml_add_to_list(i64, i64)
  declare i64 @peducoml_alloc_list()
  declare i64 @peducoml_apply0(i64)
  declare i64 @peducoml_apply(i64, i64)
  declare i64 @peducoml_alloc_closure(i64, i64)
  define i64 @main() {
  entry:
    %"0" = alloca i64, align 8
    %peducoml_alloc_closure_n = call i64 @peducoml_alloc_closure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %peducoml_apply_n = call i64 @peducoml_apply(i64 %peducoml_alloc_closure_n, i64 42)
    store i64 %peducoml_apply_n, ptr %"0", align 4
    %"01" = load i64, ptr %"0", align 4
    ret i64 %"01"
  }
  ---
  42
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_char 'c'
  > EOF
  c
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_int (((13 + 52 - 7) * 3 - 6) / 4)
  > EOF
  42
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main =
  >   let x = 1 in
  >   let y = 1 in
  >   print_bool (x = y)
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_list [1; 2; 3; 4; 5]
  > EOF
  [1; 2; 3; 4; 5]
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_tuple (10, 5, false, true)
  > EOF
  (10, 5, 0, 1)
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = 
  >   let x = 1 in
  >   let y = 1 in
  >   print_bool (x <> y)
  > EOF
  false
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = 
  >   let x = 1 in
  >   let y = 42 in
  >   print_bool (x < y)
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = 
  >   let x = 42 in
  >   let y = 1 in
  >   print_bool (x <= y)
  > EOF
  false
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = 
  >   let x = 1 in
  >   let y = 42 in
  >   print_bool (x > y)
  > EOF
  false
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = 
  >   let x = 42 in
  >   let y = 42 in
  >   print_bool (x >= y)
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = 
  >   let x = true in
  >   let y = false in
  >   print_bool (x && y)
  > EOF
  false
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = 
  >   let x = true in
  >   let y = false in
  >   print_bool (x || y)
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let id x = x
  > let main = print_int (id 42)
  > EOF
  42
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let add x y = x + y
  > let main = print_int (add 12 32)
  > EOF
  44
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let add_and_mul x y z = (x + y) * z
  > let main = print_int (add_and_mul 1 3 5)
  > EOF
  20
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let add x y = x + y
  > let partial_add x = add x
  > let main = print_int (partial_add 1 2)
  > EOF
  3
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_bool (compare_strings_eq "abcdef" "abcdef")
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_bool (compare_strings_gt "akf" "bhrefhwfoiwefohefowhb23r384723")
  > EOF
  false
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_bool (compare_strings_neq "" "")
  > EOF
  false
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_bool (compare_strings_lt "iiub" "iiua")
  > EOF
  false
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_bool (compare_strings_lte "iiua" "iiua")
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_bool (compare_strings_gte "fe" "aujw9")
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = print_int (factorial 6)
  > EOF
  720
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = print_int (factorial 5)
  > EOF
  120
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = print_int (factorial 1)
  > EOF
  1
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let is_greater_than_5 x = if x > 5 then true else false
  > let main = print_bool (is_greater_than_5 6)
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let is_greater_than_5 x = if x > 5 then true else false
  > let main = print_bool (is_greater_than_5 2)
  > EOF
  false
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let rec filter predicate list =
  >   match list with
  >     | h :: t -> if predicate h then h :: filter predicate t else filter predicate t
  >     | _ -> []
  > 
  > let main = print_list (filter (fun v -> v * v < 150) [12; 3; 54; 85; 36; 0; 91; 100; 1; 2; 13; 28; 63])
  > EOF
  [12; 3; 0; 1; 2]
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let rec add_1_to_list list = 
  >   match list with
  >     | head :: tail -> (head + 1) :: add_1_to_list tail
  >     | _ -> []
  > 
  > let main = print_list (add_1_to_list [1; 2; 3; 4; 5])
  > EOF
  [2; 3; 4; 5; 6]
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_list ((5 + 4) :: 3 :: [1; 6; 0])
  > EOF
  [9; 3; 1; 6; 0]
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_list []
  > EOF
  []
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let list1 = [1; 2; 3]
  > let list2 = [2]
  > let main = print_bool (compare_lists_lt list1 list2)
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_bool (compare_lists_eq [1; 2; 3] [1; 2; 3])
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let list1 = [1; 2]
  > let list2 = [1; 2; 3]
  > let main = print_bool (compare_lists_gt list1 list2)
  > EOF
  false
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_bool (compare_lists_lte [true; true] [true; true])
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let list1 = ['a'; 'a'; 'b']
  > let list2 = ['a'; 'a'; 'a']
  > let main = print_bool (compare_lists_gte list1 list2)
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_bool (compare_lists_neq [] [])
  > EOF
  false
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let tuple1 = (1, 2, true, 'a')
  > let tuple2 = (1, 2, true, 'a')
  > let main = print_bool (compare_tuples_eq tuple1 tuple2)
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_bool (compare_tuples_lte (1, 2, true, 'a') (1, 2, true, 'a'))
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let tuple1 = (1, 1, true, 'a')
  > let tuple2 = (1, 2, true, 'a')
  > let main = print_bool (compare_tuples_gt tuple1 tuple2)
  > EOF
  false
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_bool (compare_tuples_gte ('a', 'b') ('b', 'b'))
  > EOF
  false
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let tuple1 = (1, 2, true, 'a')
  > let tuple2 = (1, 1, true, 'a')
  > let main = print_bool (compare_tuples_lt tuple1 tuple2)
  > EOF
  false
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_bool (compare_tuples_neq (1, 8) (1, 7))
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_tuple (10, 20, 888, 60)
  > EOF
  (10, 20, 888, 60)
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let list_to_print = [0; 10; 100]
  > let main = print_list list_to_print
  > EOF
  [0; 10; 100]
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let number_to_print = 5
  > let main = print_int number_to_print
  > EOF
  5
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let bool_to_print = true
  > let main = print_bool bool_to_print
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_new_line
  > EOF
  
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let rec filter predicate list =
  >   match list with
  >     | h :: t -> if predicate h then h :: filter predicate t else filter predicate t
  >     | _ -> []
  > 
  > let main = print_list (filter (fun v -> v > 10) [1;2;3])
  > EOF
  []
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let phi n = let rec helper last1 last2 n = if n > 0 then helper last2 (last1 + last2) (n - 1) else last2 in helper 1 1 (n - 2)
  > 
  > let main = print_int (phi 10)
  > EOF
  55
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let product list =
  >   let rec helper list acc = match list with
  >     | head :: tail -> helper tail (acc * head)
  >     | _ -> acc
  >   in
  >   helper list 1
  > 
  > let main = print_int (product [1; 2; 7; 12; 10; 3; 21])
  > EOF
  105840
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let sum list =
  >   let rec helper list acc = match list with
  >     | head :: tail -> helper tail (acc + head)
  >     | _ -> acc
  >   in
  >   helper list 0
  > 
  > let main = print_int (sum [1; 2; 7; 12; 10; 3; 21; 101; 78; 42; 38])
  > EOF
  315
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let length list =
  >   let rec helper list acc = match list with
  >     | _ :: tail -> helper tail (acc + 1)
  >     | _ -> acc
  >   in
  >   helper list 0
  > 
  > let main = print_int (length [1; 23; 12; 657; 123; 346; 6; 234 ; 99; 34; 78; 28; 123; 0])
  > EOF
  14
  $ ./llvm_test.exe <<- EOF 
  > let filter predicate list =
  >   match list with
  >     | h :: t -> if predicate h then h :: filter predicate t else filter predicate t
  >     | _ -> []
  > 
  > let main = print_list (filter (fun v -> v * v < 150) [12; 3; 54; 85; 36; 0; 91; 100; 1; 2; 13; 28; 63])
  > EOF
  No such variable: filter
  $ ./llvm_test.exe <<- EOF
  > let main = f 1 2
  > EOF
  No such variable: f
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let count_solutions_of_sq_equation a b c =
  >   let sq x = x * x
  >   in
  >   let d = sq b - 4 * a * c
  >   in
  >   if d > 0 then 2 else (if d = 0 then 1 else 0)
  > 
  > let main = print_int (count_solutions_of_sq_equation 2 9 4)
  > EOF
  2
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let count_solutions_of_sq_equation a b c =
  >   let sq x = x * x
  >   in
  >   let d = sq b - 4 * a * c
  >   in
  >   if d > 0 then 2 else (if d = 0 then 1 else 0)
  > 
  > let main = print_int (count_solutions_of_sq_equation 3 6 3)
  > EOF
  1
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let count_solutions_of_sq_equation a b c =
  >   let sq x = x * x
  >   in
  >   let d = sq b - 4 * a * c
  >   in
  >   if d > 0 then 2 else (if d = 0 then 1 else 0)
  > 
  > let main = print_int (count_solutions_of_sq_equation 1 2 3)
  > EOF
  0
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let rec map f list = match list with
  >   | head :: tail -> f head :: map f tail
  >   | _ -> []
  > 
  > let sq = fun x -> x * x
  > 
  > let main = print_list (map sq [1;2;3;4;5;6;7;8;9;10])
  > EOF
  [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let f x y z =
  >   match x, y, z with
  >     | true, true, true -> true
  >     | false, false, false -> true
  >     | _ -> false
  > 
  > let main = print_bool (f (10 * 5 > 49) (58 / 2 = 27) (10 <> 20))
  > EOF
  false
  $ ./llvm_test.exe <<- EOF
  > let main = "abc" + "def"
  > EOF
  Unification failed: type of the expression is string but expected type was int
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let pifagor_check = fun x y z -> x * x + y * y = z * z
  > 
  > let main = print_bool (pifagor_check 3 4 5)
  > EOF
  true
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let some_string = "We are the champions, my friend"
  > 
  > let main = print_string (some_string)
  > EOF
  We are the champions, my friend
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let check_password password = 
  >   if compare_strings_eq password "qwerty123" then "Success" else "FAIL"
  > 
  > let main = print_string (check_password "qwerty")
  > EOF
  FAIL
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let check_password password = 
  >   if compare_strings_eq password "qwerty123" then "Success" else "FAIL"
  > 
  > let main = print_string (check_password "qwerty123")
  > EOF
  Success
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_string (concat_strings "Are we crazy?" " - obviously we are!")
  > EOF
  Are we crazy? - obviously we are!
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let list1 = [1; 2]
  > let list2 = [50; 45; 9; 65]
  > let main = print_list (concat_lists list1 list2)
  > EOF
  [1; 2; 50; 45; 9; 65]
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let fst pair =
  >   match pair with (x, _) -> x
  > 
  > let main = print_int (fst (13, 225))
  > EOF
  13
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let snd pair =
  >   match pair with (_, y) -> y
  > 
  > let main = print_int( snd (13, 225) )
  > EOF
  225
  $ ./llvm_test.exe <<- EOF
  > let fst pair =
  >   match pair with (x, _) -> x
  > 
  > let snd pair =
  >   match pair with (_, y) -> y
  > 
  > let idx = 1
  > 
  > let main = print_int (fst (13, 45, 89))
  > EOF
  Unification failed: type of the expression is int * int * int but expected type was 'j * 'i
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let rec lists_sum list1 list2 =
  >   match list1, list2 with
  >     | h1 :: t1, h2 :: t2 -> (h1 + h2) :: lists_sum t1 t2
  >     | _, _ -> []
  > 
  > let list1 = [1; 5; 7; 10; 9]
  > 
  > let list2 = [14; 0; 17; 7; 6; 0; 0]
  > 
  > let main = print_list (lists_sum list1 list2)
  > EOF
  [15; 5; 24; 17; 15]
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let rec remove_last list = match list with
  > | [] -> []
  > | [head] -> []
  > | head :: tail -> head :: remove_last tail
  > 
  > let main = print_list (remove_last [1;2;3;4;5])
  > EOF
  [1; 2; 3; 4]
  $ ./llvm_test.exe <<- EOF
  > let _ x = x
  > EOF
  :1:6: syntax error
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let apply_function f arg1 arg2 = print_int (f arg1 arg2)
  > 
  > let sum5 a1 a2 a3 a4 a5 = a1 + a2 + a3 + a4 + a5
  > 
  > let main = apply_function (sum5 50 14 26) 95 110
  > EOF
  295
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let fst p = match p with (a,b) -> a
  > let snd p = match p with (a,b) -> b
  > let fac3 n =
  >   let store = (3, (2, (1, 0))) in
  >   let n3 = fst store in
  >   let n2 = fst (snd store) in
  >   let n1 = fst (snd (snd store)) in
  >   n3 * n2 * n1
  > let main =
  >   let alive = (3, (2, (1, 0))) in
  >   let tmp1 = fac3 0 in
  >   let tmpl = print_int 42 in
  >   0
  > EOF
  42
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let rec fack n k = if n<=1 then k 1 else fack (n-1) (fun m -> k(m*n))
  > let fac n = fack n (fun id -> id)
  > let main = print_int (fac 6)
  > EOF
  720
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = (fun x -> x) (fun x -> x) print_int 42
  > EOF
  42
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let func truee = truee
  > let main = print_string (func "Now truee is correct id")  
  > EOF
  Now truee is correct id
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so
  > let main = print_int (1/0)
  > EOF
  Exception: devision by zero. Exited with 1
  [1]
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so -opaque-pointers
  > let rec fib n =
  >   if n=0 then 0 else if n=1 then 1 else fib (n-2) + fib (n-1)
  > let main = let u = print_int (fib 8) in 0
  > EOF
  21
  $ ./llvm_test.exe <<- EOF | lli-16 -load ../../runtime/peducoml_runtime.so -opaque-pointers
  > let rec fib n k =
  >   if n=0 then k 0 else if n=1 then k 1 else fib (n-2) (fun a -> fib (n-1) (fun b -> k(a+b)))
  > let main = let u = print_int (fib 8 (fun w -> w)) in 0
  > EOF
  21
