#include "common.h"
#include "emu.h"
#include "pmem.h"
#include "dpic_port.h"
#define MEM_TRACE
VPU_STATE dut_state;
extern uint8_t pmem [PMEM_SIZE];
uint64_t vpu_pc=0;

bool commit=false;
uint8_t commit_v_index=0;

//   import "DPI-C" function void pmem_read(input longint unsigned paddr,input byte unsigned len, output int unsigned output_bits[VLEN/32]);

extern "C" void pmem_read(unsigned long long paddr, uint32_t output_bits[VLEN / 32])
{
  uint64_t pc = Emulator::get_current_instance()->get_current_pc();
  vluint64_t sim_time = Emulator::get_current_instance()->get_sim_time();
  if(in_pmem(paddr)){
    int start_addr = paddr - MBASE;
    const int expected_bytes = (VLEN / 32) * sizeof(uint32_t);
    std::memcpy(output_bits, &pmem[start_addr], expected_bytes);
#ifdef MEM_TRACE
    std::stringstream ss;
    ss << "Load data from pmem at address 0x"  << std::hex << std::setfill('0') << std::setw(8) << static_cast<unsigned>(paddr)<<", ";
    ss << "at simulation time = " << std::to_string(sim_time) << " ps!";
    Emulator::get_current_instance()->log(ss.str());
#endif 
  }else{
    out_of_bound(pc,paddr);//memory bound check
    return ;
  }
}

//   import "DPI-C" function void pmem_write(input longint unsigned paddr, input int unsigned input_bits[VLEN/32]);
extern "C" void pmem_write(unsigned long long paddr, const unsigned int input_bits[VLEN / 32])
{ 
  uint64_t pc = Emulator::get_current_instance()->get_current_pc();
  vluint64_t sim_time = Emulator::get_current_instance()->get_sim_time();
  if(in_pmem(paddr)){
    int start_addr = paddr - MBASE;
    const int expected_bytes = (VLEN / 32) * sizeof(uint32_t);
    std::memcpy(&pmem[start_addr], input_bits, expected_bytes);
#ifdef MEM_TRACE
    std::stringstream ss;
    ss << "Store data to pmem at address 0x"  << std::hex << std::setfill('0') << std::setw(8) << static_cast<unsigned>(paddr)<<".";
    ss << "at simulation time = " << std::to_string(sim_time) << " ps!";
    Emulator::get_current_instance()->log(ss.str());
#endif 
  }else{
    uint64_t pc = Emulator::get_current_instance()->get_current_pc();
    out_of_bound(pc,paddr);//memory bound check
    return ;
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
        std::memcpy(&dut_state.vr[rf_addr+i]._32[chunk], &data[i][chunk], sizeof(uint32_t));
      }
     }
     commit=true;
     commit_v_index=rf_addr;
  }
}

