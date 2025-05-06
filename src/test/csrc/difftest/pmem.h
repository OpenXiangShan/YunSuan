#ifndef __DIFFTEST_PMEM_H__
#define __DIFFTEST_PMEM_H__
#include "common.h"

#define PMEM_SIZE 0x8000000
#define MBASE 0x80000000


#if CONFIG_MBASE + CONFIG_MSIZE > 0x100000000ul
#define PMEM64 1
#endif
typedef MUXDEF(PMEM64, uint64_t, uint32_t) paddr_t;
#define FMT_PADDR MUXDEF(PMEM64, "0x%016" PRIx64, "0x%08" PRIx32)


uint8_t *guest_to_host(paddr_t paddr);
static long load_img(const char *img_file);

static inline bool in_pmem(paddr_t addr) {
    return addr - MBASE <  PMEM_SIZE;
  }
uint64_t mem_addr_read(uint64_t pc,paddr_t addr, int len);
static uint64_t mem_read(paddr_t addr, int len);
void mem_addr_write(uint64_t pc, paddr_t addr, uint64_t data, int len);
void mem_write(paddr_t addr, uint64_t data, int len);
#endif