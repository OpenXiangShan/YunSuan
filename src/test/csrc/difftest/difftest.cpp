#include "difftest.h"
#include "pmem.h"

extern diff_context_t ref_cpu_state;

static bool is_skip_ref = false;
static int skip_dut_nr_inst = 0;

void init_difftest(const char *ref_so_file,long img_size, int port) {
    assert(ref_so_file != NULL);
  
    void *handle;
    handle = dlopen(ref_so_file, RTLD_LAZY);
    assert(handle);
  
    ref_difftest_memcpy = reinterpret_cast<decltype(ref_difftest_memcpy)>(dlsym(handle, "difftest_memcpy"));
    assert(ref_difftest_memcpy);
  
    ref_difftest_regcpy = reinterpret_cast<decltype(ref_difftest_regcpy)>(dlsym(handle, "difftest_regcpy"));
    assert(ref_difftest_regcpy);
  
    ref_difftest_exec = reinterpret_cast<decltype(ref_difftest_exec)>(dlsym(handle, "difftest_exec"));
    assert(ref_difftest_exec);
  
    ref_difftest_raise_intr = reinterpret_cast<decltype(ref_difftest_raise_intr)>(dlsym(handle, "difftest_raise_intr"));
    assert(ref_difftest_raise_intr);
  
    ref_isa_reg_display= reinterpret_cast<decltype(ref_isa_reg_display)>(dlsym(handle, "isa_reg_display"));
  
    void (*ref_difftest_init)(int) = reinterpret_cast<decltype(ref_difftest_init)>(dlsym(handle, "difftest_init"));
    assert(ref_difftest_init);
  
    ref_difftest_init(port);
    ref_difftest_memcpy(RESET_VECTOR, guest_to_host(RESET_VECTOR), img_size, DIFFTEST_TO_REF);
    ref_difftest_regcpy(&ref_cpu_state, DIFFTEST_TO_REF,0);
  }

  // this is used to let ref skip instructions which
  // can not produce consistent behavior with NEMU
  void difftest_skip_ref() {
    is_skip_ref = true;
    // If such an instruction is one of the instruction packing in QEMU
    // (see below), we end the process of catching up with QEMU's pc to
    // keep the consistent behavior in our best.
    // Note that this is still not perfect: if the packed instructions
    // already write some memory, and the incoming instruction in NEMU
    // will load that memory, we will encounter false negative. But such
    // situation is infrequent.
    skip_dut_nr_inst = 0;
  }
  
  // this is used to deal with instruction packing in QEMU.
  // Sometimes letting QEMU step once will execute multiple instructions.
  // We should skip checking until NEMU's pc catches up with QEMU's pc.
  // The semantic is
  //   Let REF run `nr_ref` instructions first.
  //   We expect that DUT will catch up with REF within `nr_dut` instructions.
  void difftest_skip_dut(int nr_ref, int nr_dut) {
    skip_dut_nr_inst += nr_dut;
  
    while (nr_ref -- > 0) {
      ref_difftest_exec(1);
    }
  }
  