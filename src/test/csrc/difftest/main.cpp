#include <dlfcn.h>
#include "difftest.h"
#include "emu.h"
#include "pmem.h"

diff_context_t ref_cpu_state={};

static void restart(diff_context_t *cpu) {
  /* Set the initial program counter. */
  cpu->pc = RESET_VECTOR;

  /* The zero register is always 0. */
  cpu->gpr[0] = 0;
  cpu->vlenb =128;
}

void init_isa(diff_context_t *cpu) {

  /* Initialize this virtual computer system. */
  restart(cpu);
}


int main(int argc, char *argv[]){
    const char *ref_so_file="./riscv64-spike-so";
    // const char *img_file="./baremetal_kernel/build/softmax.bin";

    auto emu=new Emulator(argc,(const char **)argv);
    EmuArgs args = emu->get_args();
    
    long img_size=load_img(args.img_file);
    init_isa(&ref_cpu_state);
    init_difftest(ref_so_file,img_size, 0);
    while(!emu->is_finished()){
      emu->tick();
  }
  int is_bad_trap = emu->is_bad_trap();
  if (!is_bad_trap) {
    emu->log("Simulation finished successfully.");
  }
  delete emu;
  
  return is_bad_trap;
}