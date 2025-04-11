#ifndef __MEM_H__
#define __MEM_H__
#include <reg.h>

#include <VVTopDebug.h>
#include "verilated.h"
#include "svdpi.h"
#include "VVTopDebug__Dpi.h"
#include <verilated_vcd_c.h>

#define CONFIG_PMEM 0
#define CONFIG_MSIZE 0x8000000

extern "C" void pmem_read(unsigned long long paddr,uint32_t output_bits[VLEN/32] );

extern "C" void get_vreg(
    svBit is_store,
    svBit wr_rf,
    uint8_t rf_addr,                 // 8-bit signed register address
    uint8_t rf_group_size, 
    const uint32_t data_0[VLEN / 32],
    const uint32_t data_1[VLEN / 32],
    const uint32_t data_2[VLEN / 32],
    const uint32_t data_3[VLEN / 32],
    const uint32_t data_4[VLEN / 32],
    const uint32_t data_5[VLEN / 32],
    const uint32_t data_6[VLEN / 32],
    const uint32_t data_7[VLEN / 32]);

#endif