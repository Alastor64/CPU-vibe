    .section .text
    .globl _start
_start:
    la      t0, scratch

    li      t1, 0xf0
    li      t2, 0x3c
    or      t3, t1, t2
    li      t4, 0xfc
    li      a0, 1
    bne     t3, t4, done
    and     t3, t1, t2
    li      t4, 0x30
    li      a0, 2
    bne     t3, t4, done

    li      t1, -1
    li      t2, 1
    slt     t3, t1, t2
    li      a0, 3
    beq     t3, zero, done
    sltu    t3, t1, t2
    li      a0, 4
    bne     t3, zero, done
    slti    t3, t1, 0
    li      a0, 5
    beq     t3, zero, done
    sltiu   t3, t1, -1
    li      a0, 6
    bne     t3, zero, done
    ori     t3, zero, 0x55
    li      a0, 7
    li      t4, 0x55
    bne     t3, t4, done

    li      t1, -8
    srai    t3, t1, 2
    li      a0, 8
    li      t4, -2
    bne     t3, t4, done

    li      t1, 0x80ff7f01
    sw      t1, 0(t0)
    lb      t3, 2(t0)
    li      a0, 9
    li      t4, -1
    bne     t3, t4, done
    lbu     t3, 2(t0)
    li      a0, 10
    li      t4, 0xff
    bne     t3, t4, done
    lh      t3, 2(t0)
    li      a0, 11
    li      t4, 0xffff80ff
    bne     t3, t4, done
    lhu     t3, 2(t0)
    li      a0, 12
    li      t4, 0x80ff
    bne     t3, t4, done

    li      t1, 0x1234
    sh      t1, 2(t0)
    lhu     t3, 2(t0)
    li      a0, 13
    li      t4, 0x1234
    bne     t3, t4, done

    li      t1, -1
    li      t2, -1
    mulh    t3, t1, t2
    li      a0, 14
    bne     t3, zero, done

    li      t1, 0x80000000
    li      t2, 2
    mulh    t3, t1, t2
    li      a0, 15
    li      t4, 0xffffffff
    bne     t3, t4, done
    mulhu   t3, t1, t2
    li      a0, 16
    li      t4, 1
    bne     t3, t4, done

    li      t1, -1
    li      t2, -1
    mulhsu  t3, t1, t2
    li      a0, 17
    li      t4, 0xffffffff
    bne     t3, t4, done

    li      t1, 100
    li      t2, 7
    divu    t3, t1, t2
    li      a0, 18
    li      t4, 14
    bne     t3, t4, done
    remu    t3, t1, t2
    li      a0, 19
    li      t4, 2
    bne     t3, t4, done

    li      a0, 0
done:
    li      t0, 0x80000000
    sw      a0, 0(t0)
1:
    j       1b

    .section .data
    .align 2
scratch:
    .word   0
