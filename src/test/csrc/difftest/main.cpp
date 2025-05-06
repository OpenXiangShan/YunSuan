#include <dlfcn.h>
#include "difftest.h"
#include "emu.h"

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


int main(){
    const char *ref_so_file="./riscv64-spike-so";
    const char *img_file="./baremetal_kernel/build/softmax.bin";

    auto emu=new Emulator(0,nullptr);
    
    long img_size=load_img(img_file);
    init_isa(&ref_cpu_state);
    init_difftest(ref_so_file,img_size, 0);
    // while(cpu.pc != 0x800001c8){
      // ref_difftest_exec(1);
      // ref_difftest_regcpy(&cpu,DIFFTEST_TO_DUT,0);
    // }

    while(!emu->is_finished()){
      emu->tick();
  }

    ref_isa_reg_display();
    printf("pc=0x%0lx\n",cpu.pc);
    return 0;
}