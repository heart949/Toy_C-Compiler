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
    li t0, 3
    sw t0, -4(fp)
    lw t1, -4(fp)
    li t2, 2
    slt t3, t2, t1
    beq t3, zero, else0
    lw t4, -4(fp)
    li t5, 1
    add t6, t4, t5
    sw t6, -4(fp)
    j endif1
else0:
    lw t0, -4(fp)
    li t1, 1
    sub t2, t0, t1
    sw t2, -4(fp)
endif1:
    lw t3, -4(fp)
    mv a0, t3
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
