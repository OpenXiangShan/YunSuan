#include <stdio.h>
#include <cstdlib>
#include <stdint.h>
extern "C" {
#include <softfloat.h>
}
#include "VSimTop.h"
#include "verilated_vcd_c.h"
#include "include/vpu_constant.h"
#include "include/test_driver.h"

TestDriver::TestDriver():
  issued(false), verbose(false), keepinput(false)
{
  // aviod random value
  set_test_type();
  gen_next_test_case();
}

TestDriver::~TestDriver() {
}

void TestDriver::set_default_value(VSimTop *dut_ptr) {
  dut_ptr->io_in_valid = false;
  dut_ptr->io_out_ready = true;
}

void TestDriver::set_test_type() {
  test_type.pick_fuType = true;
  test_type.pick_fuOpType = true;
  test_type.fuType = VIntegerALU;
  test_type.fuOpType = VRSUB;
  // printf("Set Test Type Res: fuType:%d fuOpType:%d\n", test_type.fuType, test_type.fuOpType);
}
void TestDriver::gen_next_test_case(/*type wanted*/) {
  issued = false;
  get_random_input();
  if (verbose) { display_ref_input(); }
  get_expected_output();
  if (verbose) { display_ref_output(); }
}

void TestDriver::get_random_input() {
  if (keepinput) { return; }
  input.src1[0] = rand64();
  input.src1[1] = rand64();
  input.src2[0] = rand64();
  input.src2[1] = rand64();
  input.src3[0] = rand64();
  input.src3[1] = rand64();
  input.src4[0] = rand64();
  input.src4[1] = rand64();
  if (!test_type.pick_fuType) { input.fuType = rand(); }
  else { input.fuType = test_type.fuType; }
  if (!test_type.pick_fuOpType) { input.fuOpType = rand() % 2; }
  else { input.fuOpType = test_type.fuOpType; }
  input.sew = rand() % 4;//rand() & 0x3; // TODO
  input.src_widen = false;
  input.widen = false;
  input.rm = RM_RDN; // TODO
  input.rm_s = rand() % 5;
}

void TestDriver::get_expected_output() {
  switch (input.fuType) {
    case VIntegerALU:
      if (verbose) { printf("FuType:%d, choose VIntegerALU %d\n", input.fuType, VIntegerALU); }
      expect_output = valu.get_expected_output(input); return;
    case VFloatDivider:
      if (verbose) { printf("FuType:%d, choose VFloatDivider %d\n", input.fuType, VFloatDivider); }
      expect_output = vfd.get_expected_output(input); return;
    default:
      printf("Unsupported FuType %d\n", input.fuType);
      exit(1);
      return;
  }
}
uint64_t TestDriver::rand64() {
  uint64_t tmp = rand();
  tmp = (tmp << 32) + (uint32_t) rand();
  return tmp;
}
// dut io check, return fire or not
bool TestDriver::assign_input_raising(VSimTop *dut_ptr) {
  if (!issued) {
    dut_ptr->io_in_valid = true;
    if (dut_ptr->io_in_ready) {
      issued = true;
      stuck_count = 0;
    }
  } else {
    dut_ptr->io_in_valid = false;
  }
  dut_ptr->io_in_bits_src_0_0 = input.src1[0];
  dut_ptr->io_in_bits_src_0_1 = input.src1[1];
  dut_ptr->io_in_bits_src_1_0 = input.src2[0];
  dut_ptr->io_in_bits_src_1_1 = input.src2[1];
  dut_ptr->io_in_bits_src_2_0 = input.src3[0];
  dut_ptr->io_in_bits_src_2_1 = input.src3[1];
  dut_ptr->io_in_bits_src_3_0 = input.src4[0];
  dut_ptr->io_in_bits_src_3_1 = input.src4[1];
  dut_ptr->io_in_bits_fuType  = input.fuType;
  dut_ptr->io_in_bits_fuOpType = input.fuOpType;
  dut_ptr->io_in_bits_sew     = input.sew;
  dut_ptr->io_in_bits_src_widen = input.src_widen;
  dut_ptr->io_in_bits_widen   = input.widen;
  dut_ptr->io_in_bits_rm      = input.rm;
  // printf("fuType:%d fuOpType:%d inFuType:%d inFuOpType:%d\n", dut_ptr->io_in_bits_fuType,dut_ptr->io_in_bits_fuOpType,input.fuType,input.fuOpType);
  return  dut_ptr->io_in_valid;
}

int TestDriver::diff_output_falling(VSimTop *dut_ptr) {
  bool finish = dut_ptr->io_out_valid;
  if (finish) {
    // printf("Finished\n");
    dut_output.result[0] = dut_ptr->io_out_bits_result_0;
    dut_output.result[1] = dut_ptr->io_out_bits_result_1;
    dut_output.fflags[0] = dut_ptr->io_out_bits_fflags_0;
    dut_output.fflags[1] = dut_ptr->io_out_bits_fflags_1;

    if (memcmp(&dut_output, &expect_output, sizeof(dut_output))) {
      printf("Error, compare failed\n");
      display();
      return STATE_BADTRAP;
    } else {
      gen_next_test_case();
    }
    return STATE_FINISH_OPERATION;
  } else {
    stuck_count ++;
    if (stuck_count >= stuck_limit) {
      printf("DUT stucked. Not finished in %lu cycles\n", stuck_limit);
      stuck_count = 0;
      return STATE_BADTRAP;
    }
    return STATE_RUNNING;
  }
}

void TestDriver::display_ref_input() {
  printf("REF Input:\n");
  printf("  src1 %016lx_%016lx src2 %016lx_%016lx src3 %016lx_%016lx src4 %016lx_%016lx\n", input.src1[1], input.src1[0], input.src2[1], input.src2[0], input.src3[1], input.src3[0], input.src4[1], input.src4[0]);
  printf("  fuType %x fuOpType %x sew %x src_widen %d widen %d rm %d\n", input.fuType, input.fuOpType, input.sew, input.src_widen, input.widen, input.rm);
}

void TestDriver::display_ref_output() {
  printf("Expected Output \n");
  printf("  result  %016lx_%016lx fflags: %x_%x\n", expect_output.result[1], expect_output.result[0], expect_output.fflags[1], expect_output.fflags[0]);
}

void TestDriver::display_dut() {
  printf("DUT Output:\n");
  printf("  result  %016lx_%016lx fflags: %x_%x\n", dut_output.result[1], dut_output.result[0], dut_output.fflags[1], dut_output.fflags[0]);
}

void TestDriver::display() {
  display_ref_input();
  display_ref_output();
  display_dut();
}