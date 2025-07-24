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
    li t0, 5
    sw t0, -4(fp)
    li t1, 3
    sw t1, -8(fp)
    lw t2, -4(fp)
    lw t3, -8(fp)
    add t4, t2, t3
    sw t4, -4(fp)
    lw t5, -4(fp)
    mv a0, t5
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
