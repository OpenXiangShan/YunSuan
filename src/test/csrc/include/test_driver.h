#ifndef __TEST_DRIVER_H
#define __TEST_DRIVER_H

#include <stdio.h>
#include <cstdlib>
#include <stdint.h>
#include "vpu_constant.h"
#include "VSimTop.h"
#include "verilated_vcd_c.h"
#include "gm_common.h"
#include "iotype.h"

enum {
  // STATE_GOODTRAP = 0;
  STATE_BADTRAP = 1,
  // STATE_TESTWRONG = 2
  // STATE_GMWRONG = 3;
  STATE_LIMIT_EXCEEDED = 4,
  STATE_FINISH_OPERATION = 5,
  STATE_RUNNING = -1
};

struct TestType {
  bool pick_fuType;
  uint8_t fuType;
  bool pick_fuOpType;
  uint8_t fuOpType;
};

class TestDriver {
private:
  VecInput input;
  VecOutput expect_output;
  VecOutput dut_output;
  TestType test_type;
  bool issued;

  VGMFloatAdder vfa;
  VGMFloatFMA vff;
  VGMFloatDivider vfd;
  VGMIntegerALU valu;
  VGMPermutation vperm;
  VGMIntegerALUF vialuF;
  VGMIntegerDividier vid;
  VGMFloatCvt vcvt;
  SGMFloatCvt scvt;

public:
  TestDriver();
  ~TestDriver();

  void set_default_value(VSimTop *dut_ptr);
  void set_test_type();
  void gen_next_test_case(/*type wanted*/);
  
  uint8_t gen_random_futype(std::initializer_list<uint8_t> futype_list);
  uint8_t gen_random_optype();
  uint8_t gen_random_sew();
  bool gen_random_widen();
  bool gen_random_src_widen();
  bool gen_random_is_frs1();
  bool gen_random_is_frs2();
  void gen_random_vecinfo();
  void gen_random_uopidx();
  void gen_input_vperm();
  void gen_random_idiv_input();

  void get_random_input();
  void get_expected_output();
  uint64_t rand64();
  // dut io check, return fire or not
  bool assign_input_raising(VSimTop *dut_ptr);
  int diff_output_falling(VSimTop *dut_ptr);
  void display_ref_input();
  void display_ref_output();
  void display_dut();
  void display();

  uint64_t stuck_count;
  const uint64_t stuck_limit = 100;

  bool verbose;
  bool keepinput;
  void verbose_exec() {
    verbose = true;
    vfd.verbose_exec(); valu.verbose_exec();
    vperm.verbose_exec();vid.verbose_exec();
  }
  void keep_input() { keepinput = true; }
};

#endif
