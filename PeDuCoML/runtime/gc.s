    .text
    .globl gc_init
    .type gc_init, @function
gc_init:
    mv a0, sp
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    call peducoml_init
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
    .globl gc_get_stack_top
    .type gc_get_stack_top, @function
gc_get_stack_top:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    mv a0, sp
    call print_int
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
    .globl gc_stack_scan
    .type gc_stack_scan, @function
gc_stack_scan:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 24
    mv t0, sp
    mv t1, a0
.Lstack_loop:
    bge t0, t1, .Lloop_break
    ld a0, 0(t0)
    mv a1, sp
    sd t0, 0(sp)
    call check_pointer
    ld t0, 0(sp)
    sd a0, 0(t0)
    addi t0, t0, 8
    j .Lstack_loop
.Lloop_break:
    ld ra, 16(sp)
    ld s0, 8(sp)
    addi sp, sp, 24
    ret 
