
#ifndef __EMU_H
#define __EMU_H

#include <VVTopDebug.h>
#include "verilated.h"
#include "svdpi.h"
#include "VVTopDebug__Dpi.h"
#include <verilated_vcd_c.h>
#include "VVTopDebug__Syms.h"
#include "difftest.h"
#include "common.h"
#include <iostream>
#include <iomanip> 
#include <fstream>


#ifdef EMU_THREAD
#include <verilated_threads.h>
#endif

#define CLK_PERIOD 10 

struct EmuArgs {
  uint32_t reset_cycles = 50;
  uint32_t seed = 0;
  uint64_t max_cycles = -1;
  uint64_t fork_interval = 1000;
  uint64_t max_instr = -1;
  uint64_t warmup_instr = -1;
  uint64_t stat_cycles = -1;
  uint64_t log_begin = 0, log_end = -1;
  uint64_t overwrite_nbytes = 0xe00;
  
};

typedef union
{
    uint32_t val;
} Inst;

typedef struct Decode
{
    uint64_t pc;
    Inst inst;
    bool is_vec;
    bool is_vec_cfg;
    bool is_vec_store;
    bool is_scalar_store;
    bool is_scalar_gpr;
    bool is_fp_reg;
    bool is_ebreak;
    uint64_t rs1;
    uint64_t rs2;
    uint64_t imm;
    uint8_t store_instr;
    // char logbuf[128];
} Decode;
typedef struct VPU_STATE{
    union {
      uint64_t _64[VENUM64];
      uint32_t _32[VENUM32];
      uint16_t _16[VENUM16];
      uint8_t  _8[VENUM8];
    } vr[32];
    uint64_t pc;
    Inst inst;
    uint8_t robidx;
    bool robIdx_flag;
    vluint64_t issued_time;
}VPU_STATE;

class Emulator {

private:

  VerilatedContext *contextp ;
  VVTopDebug *dut_ptr ;
  VerilatedFstC *wave ;
  uint8_t total_vector_instr;

  // emu control variable
  vluint64_t sim_time;
  int trapCode;
  EmuArgs args;
  void reset_ncycles(size_t n);
  void single_cycle();

  // Decode *present;
  // Decode *next;
  std::unique_ptr<Decode> present;  // 自动管理内存
  std::unique_ptr<Decode> next;
  VPU_STATE ref_output_pool[16];

  uint8_t store_ptr;
  uint8_t cur_vec_ptr;

  uint8_t robIdx;
  bool robIdx_flag;
  static Emulator* current_instance;
  std::ofstream log_file;
  bool log_initialized = false;

public:
  Emulator(int argc, const char *argv[]);
  ~Emulator();
  uint64_t execute(uint64_t max_cycle, uint64_t max_instr);
  vluint64_t get_sim_time() const {
    return sim_time;
  }
  EmuArgs get_args() const {
    return args;
  }
  // bool is_good_trap() {
  //   return trapCode == STATE_GOODTRAP || trapCode == STATE_LIMIT_EXCEEDED || trapCode == STATE_SIM_EXIT;
  // };
  int get_trapcode() {
    return trapCode;
  }
  int tick();
  int decode_instr(Decode* s);
  void vpu_state_store();
  bool check_vregs_state(VPU_STATE *dut);
  bool is_finished();
  uint64_t get_current_pc() const {
    return ref_output_pool[cur_vec_ptr].pc;  // 假设VPU_STATE结构中有pc字段
  }
  static Emulator* get_current_instance() { return current_instance; }
  void clear_flags(Decode &s);

  void log(const std::string& message);

};

enum {
    STATE_TRAP = 0,
    STATE_RUNNING = -1
};



#endif