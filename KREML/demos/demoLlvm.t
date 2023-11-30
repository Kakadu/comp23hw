  $ ./demoLlvm.exe <<- EOF
  > fun foo x y = x + y
  > EOF
  define i32 @foo(i32 %x, i32 %y) {
  entry:
    %add = add i32 %x, %y
    ret i32 %add
  }
  

  $ ./demoLlvm.exe <<- EOF
  > fun sub x = x - 1
  > EOF
  define i32 @sub(i32 %x) {
  entry:
    %sub = sub i32 %x, 1
    ret i32 %sub
  }
  

  $ ./demoLlvm.exe <<- EOF
  > val y = let val x = 5 in x end
  > EOF
  i32 5


  $ ./demoLlvm.exe <<- EOF
  > fun f x = if x < 7 then 1 else 0
  > EOF
  define i32 @f(i32 %x) {
  entry:
    %lt = icmp ult i32 %x, 7
    br i1 %lt, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %ifcont
  
  else:                                             ; preds = %entry
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i32 [ 1, %then ], [ 0, %else ]
    ret i32 %iftmp
  }
  

  $ ./demoLlvm.exe <<- EOF
  > val x = 8 / 2 * 3 + 6
  > EOF
  i32 18


  $ ./demoLlvm.exe <<- EOF
  > val x = false
  > EOF
  i32 0


  $ ./demoLlvm.exe <<- EOF
  > val x = ~6
  > EOF
  i32 -6


  $ ./demoLlvm.exe <<- EOF
  > val x = true andalso false
  > EOF
  i32 0


  $ ./demoLlvm.exe <<- EOF
  > fun fact n = if n <= 1 then 1 else n * fact (n - 1)
  > EOF
  define i32 @fact(i32 %n) {
  entry:
    %lte = icmp ule i32 %n, 1
    br i1 %lte, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %ifcont
  
  else:                                             ; preds = %entry
    %sub = sub i32 %n, 1
    %call = call i32 @fact(i32 %sub)
    %mult = mul i32 %n, %call
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i32 [ 1, %then ], [ %mult, %else ]
    ret i32 %iftmp
  }
  

  $ ./demoLlvm.exe <<- EOF
  > val identity = fn x => x
  > EOF
  define i32 @Ll_1(i32 %x) {
  entry:
    ret i32 %x
  }
  
  define i32 @identity(i32 %0) {
  entry:
    %call = call i32 @Ll_1(i32 %0)
    ret i32 %call
  }
  

  $ ./demoLlvm.exe <<- EOF
  > val compare = fn x => fn y => x * y > 10
  > EOF
  define i32 @Ll_1(i32 %x, i32 %y) {
  entry:
    %mult = mul i32 %x, %y
    %qt = icmp ugt i32 %mult, 10
    %boolToInt = zext i1 %qt to i32
    ret i32 %boolToInt
  }
  
  define i32 @compare(i32 %0, i32 %1) {
  entry:
    %call = call i32 @Ll_1(i32 %0, i32 %1)
    ret i32 %call
  }
  

  $ ./demoLlvm.exe <<- EOF
  > fun fac n = let fun fack n k = if n <= 1 then k 1 else fack (n-1) (fn m => k (m * n)) in fack n (fn x => x) end
  > val fat3 = fac 3
  > EOF
  Segmentation fault (core dumped)
  [139]


  $ ./demoLlvm.exe <<- EOF
  > fun y = let fun f x = x * 2 in f 5 end
  > EOF
  define i32 @f(i32 %x) {
  entry:
    %mult = mul i32 %x, 2
    ret i32 %mult
  }
  
  define i32 @y() {
  entry:
    %call = call i32 @f(i32 5)
    ret i32 %call
  }
  


  $ ./demoLlvm.exe <<- EOF
  > fun sum a b c = a + b + c
  > EOF
  define i32 @sum(i32 %a, i32 %b, i32 %c) {
  entry:
    %add = add i32 %a, %b
    %add1 = add i32 %add, %c
    ret i32 %add1
  }
  

  $ ./demoLlvm.exe <<- EOF
  > fun fact n = if n <= 1 then 1 else n * fact (n - 1)
  > val fact3 = fact 3
  > EOF
  define i32 @fact(i32 %n) {
  entry:
    %lte = icmp ule i32 %n, 1
    br i1 %lte, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %ifcont
  
  else:                                             ; preds = %entry
    %sub = sub i32 %n, 1
    %call = call i32 @fact(i32 %sub)
    %mult = mul i32 %n, %call
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i32 [ 1, %then ], [ %mult, %else ]
    ret i32 %iftmp
    %call1 = call i32 @fact(i32 3)
  }
  
    %call1 = call i32 @fact(i32 3)
