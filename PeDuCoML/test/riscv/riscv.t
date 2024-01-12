  $ ./riscv_test.exe <<- EOF | tee riscv_test.S
  > let main = 2
  > EOF
      .globl main
      .type main, @function
  main:
      addi sp,sp,-24
      sd ra,16(sp)
      sd s0,8(sp)
      addi s0,sp,24
      call gc_init
      call peducoml_destroy
      li a0,2
      ld ra,16(sp)
      ld s0,8(sp)
      addi sp,sp,24
      ret
  $ riscv64-linux-gnu-gcc -static riscv_test.S -o riscv_test.out -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out
  [2]
  $ ./riscv_test.exe <<- EOF | tee riscv_test.S
  > let main = print_int 2
  > EOF
      .globl main
      .type main, @function
  main:
      addi sp,sp,-40
      sd ra,32(sp)
      sd s0,24(sp)
      addi s0,sp,40
      call gc_init
      lui a0, %hi(print_int)
      addi a0, a0, %lo(print_int)
      li a1,1
      call peducoml_alloc_closure
      sd a0,-24(s0)
      ld a0,-24(s0)
      li a1,2
      call peducoml_apply
      sd a0,-32(s0)
      call peducoml_destroy
      ld a0,-32(s0)
      ld ra,32(sp)
      ld s0,24(sp)
      addi sp,sp,40
      ret
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  2
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let ignore x = 42
  > let main = print_int (ignore 0)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  42
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_char 'c'
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  c
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_int (13 + 52)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  65
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_int (132 - 42)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  90
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_int (51 - 100)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  -49
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_int (13 * 11)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  143
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_int (13 * 11)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  143
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_int (36 / 18)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  2
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_int (((13 + 52 - 7) * 3 - 6) / 4)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  42
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main =
  >   let x = 1 in
  >   let y = 1 in
  >   print_bool (x = y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main =
  >   let x = 1 in
  >   let y = 2 in
  >   print_bool (x = y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  false
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = 
  >   let x = 1 in
  >   let y = 1 in
  >   print_bool (x <> y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  false
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = 
  >   let x = 1 in
  >   let y = 42 in
  >   print_bool (x < y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = 
  >   let x = 42 in
  >   let y = 1 in
  >   print_bool (x <= y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  false
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = 
  >   let x = 1 in
  >   let y = 42 in
  >   print_bool (x > y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  false
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = 
  >   let x = 42 in
  >   let y = 42 in
  >   print_bool (x >= y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = 
  >   let x = true in
  >   let y = false in
  >   print_bool (x && y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  false
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = 
  >   let x = true in
  >   let y = false in
  >   print_bool (x || y)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_list [1; 2; 3; 4; 5]
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  [1; 2; 3; 4; 5]
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_tuple (10, 5, false, true)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  (10, 5, 0, 1)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let add x y = x + y
  > let main = print_int (add 1 2)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  3
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let add_and_mul x y z = (x + y) * z
  > let main = print_int (add_and_mul 1 3 5)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  20
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let add x y = x + y
  > let partial_add x = add x
  > let main = print_int (partial_add 1 2)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  3
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_string "wow, this marvel of thought works!"
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  wow, this marvel of thought works!
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_int (-2)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  -2
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = 
  >   let add x y = x + y in
  >   print_int (-(add 20 22))
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  -42
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_bool (not true)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  false
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main =
  >   if true then print_int 1 else print_int 0
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  1
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = 
  >   let rez = compare_strings_eq "abcdef" "abcdef" in
  >   print_bool rez
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_bool (compare_strings_gt "akf" "bhrefhwfoiwefohefowhb23r384723")
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  false
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_bool (compare_strings_neq "" "")
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  false
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_bool (compare_strings_lt "iiub" "iiua")
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  false
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_bool (compare_strings_lte "iiua" "iiua")
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_bool (compare_strings_gte "fe" "aujw9")
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = print_int (factorial 6)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  720
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = print_int (factorial 5)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  120
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = print_int (factorial 1)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  1
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let is_greater_than_5 x = if x > 5 then true else false
  > let main = print_bool (is_greater_than_5 6)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let is_greater_than_5 x = if x > 5 then true else false
  > let main = print_bool (is_greater_than_5 2)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  false
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let rec filter predicate list =
  >   match list with
  >     | h :: t -> if predicate h then h :: filter predicate t else filter predicate t
  >     | _ -> []
  > 
  > let main = print_list (filter (fun v -> v * v < 150) [12; 3; 54; 85; 36; 0; 91; 100; 1; 2; 13; 28; 63])
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  [12; 3; 0; 1; 2]
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let rec add_1_to_list list = 
  >   match list with
  >     | head :: tail -> (head + 1) :: add_1_to_list tail
  >     | _ -> []
  > 
  > let main = print_list (add_1_to_list [1; 2; 3; 4; 5])
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  [2; 3; 4; 5; 6]
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_list ((5 + 4) :: 3 :: [1; 6; 0])
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  [9; 3; 1; 6; 0]
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_list []
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  []
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let list1 = [1; 2; 3]
  > let list2 = [2]
  > let main = print_bool (compare_lists_lt list1 list2)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_bool (compare_lists_eq [1; 2; 3] [1; 2; 3])
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let list1 = [1; 2]
  > let list2 = [1; 2; 3]
  > let main = print_bool (compare_lists_gt list1 list2)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  false
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_bool (compare_lists_lte [true; true] [true; true])
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let list1 = ['a'; 'a'; 'b']
  > let list2 = ['a'; 'a'; 'a']
  > let main = print_bool (compare_lists_gte list1 list2)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_bool (compare_lists_neq [] [])
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  false
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let tuple1 = (1, 2, true, 'a')
  > let tuple2 = (1, 2, true, 'a')
  > let main = print_bool (compare_tuples_eq tuple1 tuple2)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_bool (compare_tuples_lte (1, 2, true, 'a') (1, 2, true, 'a'))
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let tuple1 = (1, 1, true, 'a')
  > let tuple2 = (1, 2, true, 'a')
  > let main = print_bool (compare_tuples_gt tuple1 tuple2)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  false
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_bool (compare_tuples_gte ('a', 'b') ('b', 'b'))
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  false
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let tuple1 = (1, 2, true, 'a')
  > let tuple2 = (1, 1, true, 'a')
  > let main = print_bool (compare_tuples_lt tuple1 tuple2)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  false
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_bool (compare_tuples_neq (1, 8) (1, 7))
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_tuple (10, 20, 888, 60)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  (10, 20, 888, 60)
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let list_to_print = [0; 10; 100]
  > let main = print_list list_to_print
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  [0; 10; 100]
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let number_to_print = 5
  > let main = print_int number_to_print
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  5
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let bool_to_print = true
  > let main = print_bool bool_to_print
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let rec filter predicate list =
  >   match list with
  >     | h :: t -> if predicate h then h :: filter predicate t else filter predicate t
  >     | _ -> []
  > 
  > let main = print_list (filter (fun v -> v > 10) [1;2;3])
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  []
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let phi n = let rec helper last1 last2 n = if n > 0 then helper last2 (last1 + last2) (n - 1) else last2 in helper 1 1 (n - 2)
  > 
  > let main = print_int (phi 10)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  55
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let product list =
  >   let rec helper list acc = match list with
  >     | head :: tail -> helper tail (acc * head)
  >     | _ -> acc
  >   in
  >   helper list 1
  > 
  > let main = print_int (product [1; 2; 7; 12; 10; 3; 21])
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  105840
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let sum list =
  >   let rec helper list acc = match list with
  >     | head :: tail -> helper tail (acc + head)
  >     | _ -> acc
  >   in
  >   helper list 0
  > 
  > let main = print_int (sum [1; 2; 7; 12; 10; 3; 21; 101; 78; 42; 38])
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  315
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let length list =
  >   let rec helper list acc = match list with
  >     | _ :: tail -> helper tail (acc + 1)
  >     | _ -> acc
  >   in
  >   helper list 0
  > 
  > let main = print_int (length [1; 23; 12; 657; 123; 346; 6; 234 ; 99; 34; 78; 28; 123; 0])
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  14
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let count_solutions_of_sq_equation a b c =
  >   let sq x = x * x
  >   in
  >   let d = sq b - 4 * a * c
  >   in
  >   if d > 0 then 2 else (if d = 0 then 1 else 0)
  > 
  > let main = print_int (count_solutions_of_sq_equation 2 9 4)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  2
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let count_solutions_of_sq_equation a b c =
  >   let sq x = x * x
  >   in
  >   let d = sq b - 4 * a * c
  >   in
  >   if d > 0 then 2 else (if d = 0 then 1 else 0)
  > 
  > let main = print_int (count_solutions_of_sq_equation 3 6 3)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  1
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let count_solutions_of_sq_equation a b c =
  >   let sq x = x * x
  >   in
  >   let d = sq b - 4 * a * c
  >   in
  >   if d > 0 then 2 else (if d = 0 then 1 else 0)
  > 
  > let main = print_int (count_solutions_of_sq_equation 1 2 3)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  0
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let rec map f list = match list with
  >   | head :: tail -> f head :: map f tail
  >   | _ -> []
  > 
  > let sq = fun x -> x * x
  > 
  > let main = print_list (map sq [1;2;3;4;5;6;7;8;9;10])
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let f x y z =
  >   match x, y, z with
  >     | true, true, true -> true
  >     | false, false, false -> true
  >     | _ -> false
  > 
  > let main = print_bool (f (10 * 5 > 49) (58 / 2 = 27) (10 <> 20))
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  false
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let pifagor_check = fun x y z -> x * x + y * y = z * z
  > 
  > let main = print_bool (pifagor_check 3 4 5)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  true
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let some_string = "We are the champions, my friend"
  > 
  > let main = print_string (some_string)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  We are the champions, my friend
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let check_password password = 
  >   if compare_strings_eq password "qwerty123" then "Success" else "FAIL"
  > 
  > let main = print_string (check_password "qwerty")
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  FAIL
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let check_password password = 
  >   if compare_strings_eq password "qwerty123" then "Success" else "FAIL"
  > 
  > let main = print_string (check_password "qwerty123")
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  Success
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_string (concat_strings "Are we crazy?" " - obviously we are!")
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  Are we crazy? - obviously we are!
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let list1 = [1; 2]
  > let list2 = [50; 45; 9; 65]
  > let main = print_list (concat_lists list1 list2)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  [1; 2; 50; 45; 9; 65]
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let fst pair =
  >   match pair with (x, _) -> x
  > 
  > let main = print_int (fst (13, 225))
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  13
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let snd pair =
  >   match pair with (_, y) -> y
  > 
  > let main = print_int( snd (13, 225) )
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  225
  $ ./riscv_test.exe <<- EOF > riscv_test.S
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
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  [15; 5; 24; 17; 15]
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let rec remove_last list = match list with
  > | [] -> []
  > | [head] -> []
  > | head :: tail -> head :: remove_last tail
  > 
  > let main = print_list (remove_last [1;2;3;4;5])
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  [1; 2; 3; 4]
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let apply_function f arg1 arg2 = print_int (f arg1 arg2)
  > 
  > let sum5 a1 a2 a3 a4 a5 = a1 + a2 + a3 + a4 + a5
  > 
  > let main = apply_function (sum5 50 14 26) 95 110
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  295
  $ ./riscv_test.exe <<- EOF > riscv_test.S
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
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  42
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let rec fack n k = if n<=1 then k 1 else fack (n-1) (fun m -> k(m*n))
  > let fac n = fack n (fun id -> id)
  > let main = print_int (fac 6)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  720
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = (fun x -> x) (fun x -> x) print_int 42
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  42
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let func truee = truee
  > let main = print_string (func "Now truee is correct id")  
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  Now truee is correct id
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let main = print_int (1/0)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  Exception: devision by zero. Exited with 1
  [1]
  $ ./riscv_test.exe <<- EOF > riscv_test.S -opaque-pointers
  > let rec fib n =
  >   if n=0 then 0 else if n=1 then 1 else fib (n-2) + fib (n-1)
  > let main = let u = print_int (fib 8) in 0
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  21
  $ ./riscv_test.exe <<- EOF > riscv_test.S -opaque-pointers
  > let rec fib n k =
  >   if n=0 then k 0 else if n=1 then k 1 else fib (n-2) (fun a -> fib (n-1) (fun b -> k(a+b)))
  > let main = let u = print_int (fib 8 (fun w -> w)) in 0
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  21
