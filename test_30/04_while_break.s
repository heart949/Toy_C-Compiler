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
    li t2, 10
    slt t3, t1, t2
    beq t3, zero, endloop1
    lw t4, -4(fp)
    li t5, 5
    sub t6, t4, t5
    sltiu t6, t6, 1
    beq t6, zero, else2
    j endloop1
    j endif3
else2:
endif3:
    lw t0, -4(fp)
    li t1, 1
    add t2, t0, t1
    sw t2, -4(fp)
    j loop0
endloop1:
    lw t3, -4(fp)
    mv a0, t3
    lw ra, 12(sp)
    lw fp, 8(sp)
    addi sp, sp, 16
    ret
