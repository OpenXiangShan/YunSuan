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

// extern "C"  void get_vreg(const svLogicVecVal* data);

#endif