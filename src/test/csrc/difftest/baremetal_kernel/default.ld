/* 定义入口点为 _start（通常由启动代码提供） */
ENTRY(_start)

/* 内存区域定义 */
MEMORY {
    /* RAM 起始地址 0x80000000，假设容量为 128MB */
    RAM (rwx) : ORIGIN = 0x80000000, LENGTH = 128M
}

/* 段布局 */
SECTIONS {
    /* .text 段（代码） */
    .text : {
        *(.text.init)      /* 启动代码（如 crt0.s） */
        *(.text .text.*)  /* 所有代码段 */
    } > RAM

    /* .rodata 段（只读数据） */
    .rodata : {
        *(.rodata .rodata.*)
    } > RAM

    /* .data 段（已初始化的全局变量） */
    .data : {
        *(.data .data.*)
    } > RAM

    /* .bss 段（未初始化的全局变量） */
    .bss : {
        _sbss = .;         /* BSS 段的起始地址 */
        *(.bss .bss.*)
        *(COMMON)
        _ebss = .;         /* BSS 段的结束地址 */
    } > RAM

    /* 堆栈空间定义（可选） */
    . = ALIGN(16);         /* 按 16 字节对齐 */
    _stack_end = .;        /* 堆栈起始地址 */
     .stack : {
        . = . + 0x8000;  /* 32KB stack */
     }
    _stack_top = .;        /* 堆栈顶部地址 */
}