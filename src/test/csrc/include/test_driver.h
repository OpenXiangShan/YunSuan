#ifndef __TEST_DRIVER_H
#define __TEST_DRIVER_H

#include <stdio.h>
#include <array>
#include <cstdlib>
#include <stdint.h>
#include <string>
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

enum Vfexp2FormatMode {
  VFEXP2_MODE_MIXED = 0,
  VFEXP2_MODE_FP16 = 1,
  VFEXP2_MODE_BF16 = 2,
  VFEXP2_MODE_FP32 = 3
};

struct Vfexp2LaneStats {
  uint64_t total = 0;
  uint64_t exact = 0;
  uint64_t zero_equivalent = 0;
  uint64_t nan_equivalent = 0;
  uint64_t ulp_le_1 = 0;
  uint64_t log2_ulp_le_13 = 0;
  uint64_t ulp_le_4_rto = 0;
  uint64_t ulp_over_budget = 0;
  uint64_t max_ulp_by_rm[8] = {};
};

struct Vfexp2WorstEntry {
  uint64_t src_bits, dut_bits, ref_bits, ulp;
  uint8_t rm;
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
  VGMFloatExp2 vexp2;
  SGMFloatCvt scvt;

  bool vfexp2_only;
  uint8_t vfexp2_format_mode;
  uint64_t vfexp2_logged_diffs;
  const uint64_t vfexp2_log_limit = 32;
  Vfexp2LaneStats vfexp2_stats[3];
  Vfexp2WorstEntry vfexp2_worst[3][3];

  uint8_t pick_vfexp2_format() const;
  uint64_t gen_exp2_lane_bits(uint8_t format_mode);
  void get_random_exp2_input();
  bool is_vfexp2_case() const;
  bool compare_vfexp2_output();
  void record_vfexp2_match(uint8_t format_mode, int lane, uint64_t src_bits, uint64_t dut_bits, uint64_t ref_bits);
  static const char *vfexp2_format_name(uint8_t format_mode);

public:
  TestDriver();
  ~TestDriver();

  void set_default_value(VSimTop *dut_ptr);
  void configure_vfexp2_test(bool enabled, uint8_t format_mode);
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
  void print_summary() const;

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
