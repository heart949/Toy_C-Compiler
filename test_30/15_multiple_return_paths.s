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
    lw t1, -4(fp)
    li t2, 0
    slt t3, t2, t1
    beq t3, zero, else0
    li t4, 1
    mv a0, t4
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
    j endif1
else0:
endif1:
    li a0, 0
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
