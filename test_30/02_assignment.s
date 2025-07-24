    .text
    .globl main
    # ToyC Compiler Generated Code
main:
    # Function: main
    # prologue
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    li t0, 1
    sw t0, -4(fp)
    lw t1, -4(fp)
    li t2, 2
    add t3, t1, t2
    sw t3, -4(fp)
    lw t4, -4(fp)
    mv a0, t4
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
