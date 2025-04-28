#include "common.h"
#include "pmem.h"


uint8_t pmem[PMEM_SIZE] __attribute((aligned(4096))) ={};

uint8_t *guest_to_host(uint32_t paddr){ return pmem +paddr - MBASE;}

static void out_of_bound(uint32_t addr) {
    printf("address = 0x%08 is out of bound of pmem [0x%08, 0x%08] at pc = 0x%08",
        addr, (uint32_t)MBASE, (uint32_t)(MBASE + PMEM_SIZE));
    assert(0);
  }
  

static long load_img(const char *img_file) {
    if (img_file == NULL) {
      printf("No image is give.");
      return 4096; // built-in image size
    }
  
    FILE *fp = fopen(img_file, "rb");
    assert(fp!=NULL);
  
    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);
  
    printf("The image is %s, size = %ld\n", img_file, size);
  
    fseek(fp, 0, SEEK_SET);
    int ret = fread(guest_to_host(RESET_VECTOR), size, 1, fp);
    assert(ret == 1);
  
    fclose(fp);
    return size;
  }

  uint64_t mem_addr_read(uint32_t addr, int len)
  {
      if (in_pmem(addr))
      {
          return mem_read(addr, len);
      }
      out_of_bound(addr);
      return 0;
  }

uint64_t mem_read(uint32_t addr, int len) {
    uint8_t *host_addr=guest_to_host(addr);
    switch (len){
        case 1: return *(uint8_t *)host_addr;
        case 2: return *(uint16_t *)host_addr;
        case 4: return *(uint32_t *)host_addr;
        case 8: return *(uint64_t *)host_addr;
        default: assert(0);
    }
  }
  