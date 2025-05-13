#include <stdio.h>
#include <mem.h>
#include <stdint.h>
#include <cstring>
#include <reg.h>

// float *pmem=NULL;
// void guest_to_host(uint64_t paddr){ return pmem+paddr-CONFTG_PMEM};

FloatUintUnion pmem[CONFIG_MSIZE] = {};
extern FloatUintUnion diff_vreg[32][VLEN/32];
extern VCSR vcsr;
bool commit_global=false;
void out_of_bound(uint64_t paddr)
{
  if ((paddr - CONFIG_PMEM + vcsr.vl) < CONFIG_MSIZE)
    return;
  printf("Memory is out of bound!\n");
  assert(0);
}


extern "C" void pmem_read(unsigned long long paddr, uint32_t output_bits[VLEN / 32])
{ 
  out_of_bound(paddr);//memory bound check
  int start_addr = paddr/4 - CONFIG_PMEM;
  for (int i = start_addr; i < start_addr+VLEN/32; i++)
  {
    std::memcpy(&output_bits[i-start_addr], &pmem[i].as_uint32, sizeof(uint32_t));
    // std::memcpy(&reg[i-start_addr].as_uint32, &output_bits[i-start_addr], sizeof(float));
  }
}

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
  const uint32_t data_7[VLEN / 32])
{
  uint32_t data[8][VLEN/32];
  for(int i=0;i<VLEN/32;i++){
    std::memcpy(&data[0][i], &data_0[i], sizeof(uint32_t));
    std::memcpy(&data[1][i], &data_1[i], sizeof(uint32_t));
    std::memcpy(&data[2][i], &data_2[i], sizeof(uint32_t));
    std::memcpy(&data[3][i], &data_3[i], sizeof(uint32_t));
    std::memcpy(&data[4][i], &data_4[i], sizeof(uint32_t));
    std::memcpy(&data[5][i], &data_5[i], sizeof(uint32_t));
    std::memcpy(&data[6][i], &data_6[i], sizeof(uint32_t));
    std::memcpy(&data[7][i], &data_7[i], sizeof(uint32_t));
  }
  if(wr_rf==1){
    for(int chunk = 0; chunk < VLEN / 32; chunk++) {
      for(int i = 0; i < rf_group_size; i++) {
        std::memcpy(&diff_vreg[rf_addr+i][chunk].as_uint32, &data[i][chunk], sizeof(uint32_t));
      }
     }
  }
  
 

//  for(int i=0;i<VLEN/32;i++){
//   printf("vreg[%u][%d]=%u\t diff_vreg[%u][%d]=%f\n",rf_addr,i,data_0[i],rf_addr,i,diff_vreg[rf_addr][i].as_float);
// }
// FloatUintUnion reg[VLEN/32];   
// for(int i=0;i<VLEN/32;i++){
//   std::memcpy(&reg[i].as_uint32, &data_0[i], sizeof(uint32_t));
// }   
commit_global=true;
}

