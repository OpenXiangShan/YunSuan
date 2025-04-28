.section .text.init
.global _start
_start:
    la sp, _stack_top   /* 设置堆栈指针 */
    li t0, 1 << 13   # mstatus.FS = 1
    csrs mstatus, t0
    li t0, (1 << 9)  # mstatus.VS = 01 (Initial)
    csrs mstatus, t0
    call _trm_init           /* 调用C主函数 */
    j .                 /* 主函数返回后循环 */