#ifndef __EMU_H
#define __EMU_H

#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include "test_driver.h"
#include "VSimTop.h"
#include "verilated_vcd_c.h"

struct EmuArgs {
  uint32_t seed;
  uint64_t max_cycles;
  uint64_t max_operations;
  uint64_t log_begin, log_end;
  uint64_t enable_waveform;

  bool verbose;

  EmuArgs() {
    seed = 0;
    max_cycles = -1;
    max_operations = -1;
    log_begin = -1;
    log_end = -1;
    enable_waveform = false;
    verbose = false;
  }
};

class Emulator {

private:
  VSimTop *dut_ptr;
#if VM_TRACE == 1
  VerilatedVcdC* tfp;
#endif
  TestDriver test_driver;

  EmuArgs args;
  uint64_t cycles;
  uint64_t operations;
  inline char* timestamp_filename(time_t t, char *buf);
  inline char* waveform_filename(time_t t, const char *s);
public:
  Emulator(int argc, const char *argv[]);
  ~Emulator();

  EmuArgs get_args() const { return args; }

  int single_cycle();
  void dummy_single_cycle();
  void reset_ncycles(size_t cycles);
  bool execute();
  int execute_operations(uint64_t ops);
};

#endif
