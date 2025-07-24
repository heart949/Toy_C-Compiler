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
loop0:
    lw t1, -4(fp)
    li t2, 5
    slt t3, t1, t2
    beq t3, zero, endloop1
    lw t4, -4(fp)
    li t5, 2
    rem t6, t4, t5
    li t0, 0
    sub t1, t6, t0
    sltiu t1, t1, 1
    beq t1, zero, else2
    lw t2, -4(fp)
    li t3, 2
    add t4, t2, t3
    sw t4, -4(fp)
    j endif3
else2:
    lw t5, -4(fp)
    li t6, 1
    add t0, t5, t6
    sw t0, -4(fp)
endif3:
    j loop0
endloop1:
    lw t1, -4(fp)
    mv a0, t1
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
