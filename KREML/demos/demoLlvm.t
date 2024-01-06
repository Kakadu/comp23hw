  $ ./demoLlvm.exe <<- EOF
  > fun foo x y = x + y
  > val foo_12 = foo 1 2
  > EOF
  define i64 @foo(i64 %x, i64 %y) {
  entry:
    %add = add i64 %x, %y
    ret i64 %add
    %call = call i64 @foo(i64 1)
    %call1 = call addrspace(64) i64 %call(i64 2)
  }

    %call1 = call addrspace(64) i64 %call(i64 2)


  $ ./demoLlvm.exe <<- EOF
  > fun sub x = x - 1
  > val sub2 = sub 2
  > EOF
  define i64 @sub(i64 %x) {
  entry:
    %sub = sub i64 %x, 1
    ret i64 %sub
    %call = call i64 @sub(i64 2)
  }

    %call = call i64 @sub(i64 2)


  $ ./demoLlvm.exe <<- EOF
  > val y = let val x = 5 in x end
  > EOF
  i64 5


  $ ./demoLlvm.exe <<- EOF
  > fun f x = if x < 7 then 1 else 0
  > val f5 = f 5
  > EOF
  define i64 @f(i64 %x) {
  entry:
    %lt = icmp ult i64 %x, 7
    br i1 %lt, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %ifcont
  
  else:                                             ; preds = %entry
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ 1, %then ], [ 0, %else ]
    ret i64 %iftmp
  }
  

  $ ./demoLlvm.exe <<- EOF
  > val x = 8 / 2 * 3 + 6
  > EOF
  i64 18


  $ ./demoLlvm.exe <<- EOF
  > val x = false
  > EOF
  i64 0


  $ ./demoLlvm.exe <<- EOF
  > val x = ~6
  > EOF
  i64 -6


  $ ./demoLlvm.exe <<- EOF
  > val x = true andalso false
  > EOF
  i64 0


  $ ./demoLlvm.exe <<- EOF
  > fun fact n = if n <= 1 then 1 else n * fact (n - 1)
  > EOF
  define i64 @fact(i64 %n) {
  entry:
    %lte = icmp ule i64 %n, 1
    br i1 %lte, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %ifcont
  
  else:                                             ; preds = %entry
    %sub = sub i64 %n, 1
    %call = call i64 @fact(i64 %sub)
    %mult = mul i64 %n, %call
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ 1, %then ], [ %mult, %else ]
    ret i64 %iftmp
  }
  

  $ ./demoLlvm.exe <<- EOF
  > val identity = fn x => x
  > EOF
  Segmentation fault (core dumped)
  [139]
  $ ./demoLlvm.exe <<- EOF
  > val compare = fn x => fn y => x * y > 10
  > EOF
  Segmentation fault (core dumped)
  [139]
  $ ./demoLlvm.exe <<- EOF
  > fun fac n = let fun fack n k = if n <= 1 then k 1 else fack (n-1) (fn m => k (m * n)) in fack n (fn x => x) end
  > val fat3 = fac 3
  > EOF
  define i64 @Cc_1(i64 %k, i64 %n, i64 %m) {
  entry:
    %mult = mul i64 %m, %n
    %call = call addrspace(64) i64 %k(i64 %mult)
    ret i64 %call
  }
  
  define i64 @fack(i64 %n, i64 %k) {
  entry:
    %lte = icmp ule i64 %n, 1
    br i1 %lte, label %then, label %else
  
  then:                                             ; preds = %entry
    %call = call addrspace(64) i64 %k(i64 1)
    br label %ifcont
  
  else:                                             ; preds = %entry
    %sub = sub i64 %n, 1
    %call1 = call i64 @fack(i64 %sub)
    %call2 = call i64 @Cc_1(i64 %k)
    %call3 = call addrspace(64) i64 %call2(i64 %n)
    %call4 = call addrspace(64) i64 %call1(i64 %call3)
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ %call, %then ], [ %call4, %else ]
    ret i64 %iftmp
  }
  
  define i64 @Ll_1(i64 %x) {
  entry:
    ret i64 %x
  }
  
  define i64 @fac(i64 %n) {
  entry:
    %call = call i64 @fack(i64 %n)
    %call1 = call addrspace(64) i64 %call(ptr @Ll_1)
    ret i64 %call1
    %call2 = call i64 @fac(i64 3)
  }
  
    %call2 = call i64 @fac(i64 3)


  $ ./demoLlvm.exe <<- EOF
  > fun y = let fun f x = x * 2 in f 5 end
  > EOF
  define i64 @f(i64 %x) {
  entry:
    %mult = mul i64 %x, 2
    ret i64 %mult
  }
  
  define i64 @y() {
  entry:
    %call = call i64 @f(i64 5)
    ret i64 %call
  }
  

  $ ./demoLlvm.exe <<- EOF
  > val y = let fun f x = x * 2 in f 5 end
  > EOF
  define i64 @f(i64 %x) {
  entry:
    %mult = mul i64 %x, 2
    ret i64 %mult
    %call = call i64 @f(i64 5)
  }
  
    %call = call i64 @f(i64 5)


  $ ./demoLlvm.exe <<- EOF
  > fun sum a b c = a + b + c
  > val sum123 = sum 1 2 3
  > EOF
  define i64 @sum(i64 %a, i64 %b, i64 %c) {
  entry:
    %add = add i64 %a, %b
    %add1 = add i64 %add, %c
    ret i64 %add1
    %call = call i64 @sum(i64 1)
    %call2 = call addrspace(64) i64 %call(i64 2)
    %call3 = call addrspace(64) i64 %call2(i64 3)
  }

    %call3 = call addrspace(64) i64 %call2(i64 3)


  $ ./demoLlvm.exe <<- EOF
  > fun fact n = if n <= 1 then 1 else n * fact (n - 1)
  > val fact3 = fact 3
  > EOF
  define i64 @fact(i64 %n) {
  entry:
    %lte = icmp ule i64 %n, 1
    br i1 %lte, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %ifcont
  
  else:                                             ; preds = %entry
    %sub = sub i64 %n, 1
    %call = call i64 @fact(i64 %sub)
    %mult = mul i64 %n, %call
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ 1, %then ], [ %mult, %else ]
    ret i64 %iftmp
    %call1 = call i64 @fact(i64 3)
  }
  
    %call1 = call i64 @fact(i64 3)

