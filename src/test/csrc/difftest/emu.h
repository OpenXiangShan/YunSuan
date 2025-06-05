
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
  char *log_file_name ;
  char *waveform_file ;
  char *img_file ; 
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

enum {
    STATE_BADTRAP = 0,
    STATE_RUNNING = -1,
    STATE_STOPPED =-2
};


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

  bool breakpoint = false;

  uint8_t uncommit_cycle =0;

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
  bool is_bad_trap() {
    return trapCode == STATE_BADTRAP;
  }
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
  int parse_args(int argc, const char *argv[]);

  template <typename T>
  void print_vector_register(std::stringstream &ss, int vreg_idx,
                             decltype(VPU_STATE::vr[0]) &ref_reg, decltype(VPU_STATE::vr[0]) &dut_reg,
                             int elements_per_line ){
    constexpr int element_width = sizeof(T) * 8;
    constexpr int hex_width = element_width / 4;
    int num_elements = VLEN / element_width;

    // Print reference
    ss << "REF model [v" << vreg_idx << "]: \n";
    for (int e = 0; e < num_elements; e++){
      ss << "[" << std::setfill('0') << std::setw(2) << e << "] "
         << std::hex << std::setfill('0') << std::setw(hex_width);

      if constexpr(std::is_same<T, uint8_t>::value) {
        ss << static_cast<uint32_t>(ref_reg._8[e]);//uint8 -> uint32; uint8 = ascii
      } else if constexpr(std::is_same<T, uint16_t>::value) {
        ss << static_cast<uint32_t>(ref_reg._16[e]);
      } else if constexpr(std::is_same<T, uint32_t>::value) {
        ss << ref_reg._32[e];
      } else {
        ss << ref_reg._64[e];
      }

      if (e % elements_per_line == elements_per_line - 1)
        ss << "\n";
    }

    // Print DUT
    ss << "DUT [v" << vreg_idx << "]: \n";
    for (int e = 0; e < num_elements; e++){
      ss << "[" << std::setfill('0') << std::setw(2) << e << "] "
         << std::hex << std::setfill('0') << std::setw(hex_width);
      
      if constexpr(std::is_same<T, uint8_t>::value) {
        ss << static_cast<uint32_t>(dut_reg._8[e]);//uint8 -> uint32; uint8 = ascii
      } else if constexpr(std::is_same<T, uint16_t>::value) {
        ss << static_cast<uint32_t>(dut_reg._16[e]);
      } else if constexpr(std::is_same<T, uint32_t>::value) {
        ss << dut_reg._32[e];
      } else {
        ss << dut_reg._64[e];
      }

      if (e % elements_per_line == elements_per_line - 1)
        ss << "\n";
    }
  }
};



#endif