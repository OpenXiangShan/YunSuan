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

  VGMFloatDivider vfd;
  VGMIntegerALU valu;

public:
  TestDriver();
  ~TestDriver();

  void set_default_value(VSimTop *dut_ptr);
  void set_test_type();
  void gen_next_test_case(/*type wanted*/);
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
};

#endif
