    .text
    .globl main
    # ToyC Compiler Generated Code
fact:
    # Function: fact
    # prologue
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    sw a0, -4(fp)
    lw t0, -4(fp)
    li t1, 1
    slt t2, t1, t0
    xori t2, t2, 1
    beq t2, zero, else0
    li t3, 1
    mv a0, t3
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
    j endif1
else0:
    lw t4, -4(fp)
    lw t5, -4(fp)
    li t6, 1
    sub t0, t5, t6
    mv a0, t0
    jal ra, fact
    mul t1, t4, a0
    mv a0, t1
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
endif1:
main:
    # Function: main
    # prologue
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    li t0, 5
    mv a0, t0
    jal ra, fact
    mv a0, a0
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
