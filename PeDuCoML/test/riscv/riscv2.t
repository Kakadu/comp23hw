Copyright 2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0


  $ ./riscv_test.exe <<- EOF
  > let sum x y = x+y
  > let main = print_int (if true then (sum 1) else (sum (-1)))
  > EOF
  Unification failed: type of the expression is int -> int but expected type was int
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let sum a b = a+b
  > let main = 
  >   let u = if false then sum 1 else print_int in 
  >   print_int (u 0)
  > EOF
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  00
  $ ./riscv_test.exe <<- EOF > riscv_test.S
  > let lol i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 =
  >   i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11 + i12 + i13 + i14 + i15
  > let main = print_int (lol 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
  $ riscv64-linux-gnu-gcc -static -o riscv_test.out riscv_test.S -L../../runtime/ -l:libruntime.a
  $ qemu-riscv64-static riscv_test.out 
  120
