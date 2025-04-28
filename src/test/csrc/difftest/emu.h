/***************************************************************************************
* Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* DiffTest is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

#ifndef __EMU_H
#define __EMU_H

#include <VVTopDebug.h>
#include "verilated.h"
#include "svdpi.h"
#include "VVTopDebug__Dpi.h"
#include <verilated_vcd_c.h>
#include "VVTopDebug__Syms.h"

#include "common.h"


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
    bool is_scalar_store;
    bool is_scalar_gpr;
    bool is_fp_reg;
    uint64_t rs1;
    uint64_t rs2;
    uint64_t imm;
    char logbuf[128];
} Decode;

class Emulator {

private:

  VerilatedContext *contextp ;
  VVTopDebug *dut_ptr ;
  VerilatedVcdC *wave ;
  uint8_t total_vector_instr;

  // emu control variable
  vluint64_t sim_time;
  int trapCode;
  EmuArgs args;
  void reset_ncycles(size_t n);
  void single_cycle();
  uint64_t next_pc;
  uint64_t present_pc;

  Decode *present;
  Decode *next;

 

public:
  Emulator(int argc, const char *argv[]);
  ~Emulator();
  uint64_t execute(uint64_t max_cycle, uint64_t max_instr);
  uint64_t get_cycles() const {
    return cycles;
  }
  EmuArgs get_args() const {
    return args;
  }
  bool is_good_trap() {
    return trapCode == STATE_GOODTRAP || trapCode == STATE_LIMIT_EXCEEDED || trapCode == STATE_SIM_EXIT;
  };
  int get_trapcode() {
    return trapCode;
  }
  int tick();
  int decode_exec(Decode* s)

};

enum {
    STATE_GOODTRAP = 0,
    STATE_BADTRAP = 1,
    STATE_ABORT = 2,
    STATE_SIM_EXIT = 6,
    STATE_RUNNING = -1
};



#endif