#include "common.h"
#include "pmem.h"


uint8_t pmem[PMEM_SIZE] __attribute((aligned(4096))) ={};

uint8_t *guest_to_host(uint32_t paddr){ return pmem +paddr - MBASE;}

void out_of_bound(uint64_t pc,uint32_t addr) {
    printf("address = 0x%08x is out of bound of pmem [0x%08x, 0x%08x] at pc = 0x%08lx\n",
        addr, (uint32_t)MBASE, (uint32_t)(MBASE + PMEM_SIZE),pc);
    assert(0);
  }
  

long load_img(const char *img_file) {
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

  uint64_t mem_addr_read(uint64_t pc,paddr_t addr, int len)
  {
      if (in_pmem(addr))
      {
          return mem_read(addr, len);
      }
      out_of_bound(pc, addr);
      return 0;
  }

uint64_t mem_read(paddr_t addr, int len) {
    uint8_t *host_addr=guest_to_host(addr);
    switch (len){
        case 1: return *(uint8_t *)host_addr;
        case 2: return *(uint16_t *)host_addr;
        case 4: return *(uint32_t *)host_addr;
        case 8: return *(uint64_t *)host_addr;
        default: assert(0);
    }
  }

void  mem_addr_write(uint64_t pc, paddr_t addr, uint64_t data, int len)
  {
      if (in_pmem(addr))
      {
          mem_write(addr, data, len);
          return ;
      }
      out_of_bound(pc,addr);
  }

void mem_write(paddr_t addr, uint64_t data, int len){
  uint8_t *host_addr=guest_to_host(addr);
  switch (len){
    case 1:  *(uint8_t *)host_addr =data ; return;
    case 2:  *(uint16_t *)host_addr=data ; return;
    case 4:  *(uint32_t *)host_addr=data ; return;
    case 8:  *(uint64_t *)host_addr=data ; return;
    default: assert(0);
}

}