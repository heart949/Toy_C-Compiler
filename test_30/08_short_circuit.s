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
    li t0, 0
    sw t0, -4(fp)
    li t1, 1
    sw t1, -8(fp)
    lw t2, -4(fp)
    lw t3, -8(fp)
    lw t4, -4(fp)
    div t5, t3, t4
    sltu t0, zero, t2
    sltu t1, zero, t5
    and t6, t0, t1
    beq t6, zero, else0
    li t0, 1
    mv a0, t0
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
    j endif1
else0:
    li a0, 0
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
endif1:
