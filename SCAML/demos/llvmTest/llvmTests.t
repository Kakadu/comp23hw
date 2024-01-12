  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let fibo n =
  > let rec fibo_cps n acc =
  > if n < 3 then acc 1 else fibo_cps (n - 1) (fun x ->  fibo_cps (n - 2) (fun y -> acc (x + y)))
  > in
  > fibo_cps n (fun x -> x)
  > let main = print_int (fibo 11)
  > EOF
  89
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let fac n =
  > let rec fack n k =
  > if n <= 1 then k 1 else fack (n - 1) (fun m -> k (m * n))
  > in 
  > fack n (fun x -> x)
  > let main = print_int (fac 6)
  > EOF
  720
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let main = print_int ( (5 + 4) - 2 )
  > EOF
  7
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let x = (5 + (4 - 3)) - 2
  > let main = print_int x
  > EOF
  4
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let main = print_int ((5 + 4) + (3 + 2))
  > EOF
  14
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let s1 x =
  > let s2 = x + 5 in
  > let s3 = s2 + 5 in
  > s3
  > let main = print_int (s1 10)
  > EOF
  20
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let plus a =
  > let sum b = a + b in
  > sum 5
  > let main = print_int (plus 5)
  > EOF
  10
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = print_int (factorial 5)
  > EOF
  120

  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let rec foo a1  a1  a1  a1  a1  a1  a1  a1  a1  a1  a1  a1  a1 = a1
  > let main = foo 1
  > EOF
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let rec foo    a b c d e f g h i j k l m n  o p q  = a
  > let main = foo 1 2 3 4 5 6 7 8 9 0 1 2 3 4  5 6 7
  > EOF
  $ ./llvmTests.exe <<- EOF | lli-16 -load ../../lib/runtime/runtime.so
  > let rec foo    _ _ _ _ e f _  = e
  > let k = foo 1 2 3 4 5 6 7
  > let main = print_int k
  > EOF
  5
  $ ./llvmTests.exe <<- EOF
  > let rec foo    _ _ _ _ e f _  = e
  > let k = foo 1 2 3 4 5 6 7
  > let main = print_int k
  > EOF
  declare i64 @print_bool(i64)
  
  declare i64 @print_int(i64)
  
  declare i64 @applyPAppli(i64, i64)
  
  declare i64 @addNewPAppliClosure(i64, i64)
  
  define i64 @foo(i64 %"0_unused", i64 %"0_unused1", i64 %"0_unused2", i64 %"0_unused3", i64 %e, i64 %f, i64 %"0_unused4") {
  entry:
    %"0_unused5" = alloca i64, align 8
    store i64 %"0_unused", ptr %"0_unused5", align 4
    %"0_unused6" = alloca i64, align 8
    store i64 %"0_unused1", ptr %"0_unused6", align 4
    %"0_unused7" = alloca i64, align 8
    store i64 %"0_unused2", ptr %"0_unused7", align 4
    %"0_unused8" = alloca i64, align 8
    store i64 %"0_unused3", ptr %"0_unused8", align 4
    %e9 = alloca i64, align 8
    store i64 %e, ptr %e9, align 4
    %f10 = alloca i64, align 8
    store i64 %f, ptr %f10, align 4
    %"0_unused11" = alloca i64, align 8
    store i64 %"0_unused4", ptr %"0_unused11", align 4
    %e12 = load i64, ptr %e9, align 4
    ret i64 %e12
  }
  
  define i64 @k() {
  entry:
    %PAppliClosure = call i64 @addNewPAppliClosure(i64 ptrtoint (ptr @foo to i64), i64 7)
    %PAppliApplication = call i64 @applyPAppli(i64 %PAppliClosure, i64 1)
    %app_0 = alloca i64, align 8
    store i64 %PAppliApplication, ptr %app_0, align 4
    %app_01 = load i64, ptr %app_0, align 4
    %PAppliApplication2 = call i64 @applyPAppli(i64 %app_01, i64 2)
    %app_1 = alloca i64, align 8
    store i64 %PAppliApplication2, ptr %app_1, align 4
    %app_13 = load i64, ptr %app_1, align 4
    %PAppliApplication4 = call i64 @applyPAppli(i64 %app_13, i64 3)
    %app_2 = alloca i64, align 8
    store i64 %PAppliApplication4, ptr %app_2, align 4
    %app_25 = load i64, ptr %app_2, align 4
    %PAppliApplication6 = call i64 @applyPAppli(i64 %app_25, i64 4)
    %app_3 = alloca i64, align 8
    store i64 %PAppliApplication6, ptr %app_3, align 4
    %app_37 = load i64, ptr %app_3, align 4
    %PAppliApplication8 = call i64 @applyPAppli(i64 %app_37, i64 5)
    %app_4 = alloca i64, align 8
    store i64 %PAppliApplication8, ptr %app_4, align 4
    %app_49 = load i64, ptr %app_4, align 4
    %PAppliApplication10 = call i64 @applyPAppli(i64 %app_49, i64 6)
    %app_5 = alloca i64, align 8
    store i64 %PAppliApplication10, ptr %app_5, align 4
    %app_511 = load i64, ptr %app_5, align 4
    %PAppliApplication12 = call i64 @applyPAppli(i64 %app_511, i64 7)
    %app_6 = alloca i64, align 8
    store i64 %PAppliApplication12, ptr %app_6, align 4
    %app_613 = load i64, ptr %app_6, align 4
    ret i64 %app_613
  }
  
  define i64 @main() {
  entry:
    %PAppliClosure = call i64 @addNewPAppliClosure(i64 ptrtoint (ptr @print_int to i64), i64 1)
    %PAppliClosure1 = call i64 @addNewPAppliClosure(i64 ptrtoint (ptr @k to i64), i64 0)
    %PAppliApplication = call i64 @applyPAppli(i64 %PAppliClosure, i64 %PAppliClosure1)
    %app_0 = alloca i64, align 8
    store i64 %PAppliApplication, ptr %app_0, align 4
    %app_02 = load i64, ptr %app_0, align 4
    ret i64 0
  }
  
