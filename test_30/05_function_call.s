    .text
    .globl main
    # ToyC Compiler Generated Code
add:
    # Function: add
    # prologue
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    sw a0, -4(fp)
    sw a1, -8(fp)
    lw t0, -4(fp)
    lw t1, -8(fp)
    add t2, t0, t1
    mv a0, t2
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
main:
    # Function: main
    # prologue
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    li t0, 3
    mv a0, t0
    li t1, 4
    mv a1, t1
    jal ra, add
    sw a0, -4(fp)
    lw t2, -4(fp)
    mv a0, t2
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
