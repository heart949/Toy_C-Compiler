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
    li t1, 0
    sw t1, -8(fp)
loop0:
    lw t2, -4(fp)
    li t3, 5
    slt t4, t2, t3
    beq t4, zero, endloop1
    lw t5, -4(fp)
    li t6, 1
    add t0, t5, t6
    sw t0, -4(fp)
    lw t1, -4(fp)
    li t2, 3
    sub t3, t1, t2
    sltiu t3, t3, 1
    beq t3, zero, else2
    j loop0
    j endif3
else2:
endif3:
    lw t4, -8(fp)
    li t5, 1
    add t6, t4, t5
    sw t6, -8(fp)
    j loop0
endloop1:
    lw t0, -8(fp)
    mv a0, t0
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
