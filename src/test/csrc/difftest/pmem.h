#ifndef __DIFFTEST_PMEM_H__
#define __DIFFTEST_PMEM_H__
#include "common.h"

#define PMEM_SIZE 0x8000000
#define MBASE 0x80000000

uint8_t *guest_to_host(uint32_t paddr);
static long load_img(const char *img_file);
uint32_t imem_read(uint32_t addr);

static inline bool in_pmem(uint32_t addr) {
    return addr - MBASE <  PMEM_SIZE;
  }
uint64_t mem_addr_read(uint32_t addr, int len);
static uint64_t mem_read(uint32_t addr, int len);

#endif