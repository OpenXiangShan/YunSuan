#include "emu.h"
#include "decode.h"
Emulator::Emulator(int argc, const char *argv[]) {

    sim_time=0;

    contextp = new VerilatedContext;
    contextp->commandArgs(argc, argv);

    dut_ptr = new VVTopDebug{contextp};
    wave = new VerilatedVcdC;

    contextp->traceEverOn(true);
    dut_ptr->trace(wave, 5);
    wave->open("build/vpu.vcd");
    wave->set_time_unit("ns");
    trapCode = STATE_RUNNING;

    // init core
    reset_ncycles(3);

    total_vector_instr =0;
    present->pc= 0x80000000;
    next->pc= 0x80000000;

}

Emulator::~Emulator() {
    if(wave) wave->close();
    delete wave;
    delete dut_ptr;
    delete contextp;
}

void Emulator::reset_ncycles(size_t n) {
    dut_ptr->reset = 1;
    n--;
    while (n-- > 0)
    {
        single_cycle();
    }

    // add another cycle to make sure the reset is effective
    dut_ptr->clock = 1;
    dut_ptr->eval();
    wave->dump(sim_time); // simulation time
    sim_time += CLK_PERIOD / 2;
    dut_ptr->clock = 0;
    dut_ptr->reset = 0;
    dut_ptr->eval();
    wave->dump(sim_time); // simulation time
    sim_time += CLK_PERIOD / 2;
}

void Emulator::single_cycle()
{
    dut_ptr->clock = 1;
    dut_ptr->eval();
    wave->dump(sim_time); // simulation time
    sim_time += CLK_PERIOD / 2;
    dut_ptr->clock = 0;
    dut_ptr->eval();
    wave->dump(sim_time); // simulation time
    sim_time += CLK_PERIOD / 2;
}

int Emulator::tick(){

    single_cycle();

    if ( total_vector_instr <= 16 ) {
        ref_difftest_exec(1);
        present->pc=next->pc;
        ref_difftest_regcpy(&ref_cpu_state, DIFFTEST_TO_DUT,0);
        present->inst.val = mem_addr_read(present->pc,4);

    }

}

int Emulator::decode(Decode *s) {
    bool is_vec;
    bool is_scalar_store;
    bool is_scalar_gpr;
    bool is_fp_reg;
    uint64_t rs1;
    uint64_t rs2;

#define INSTPAT_INST(s) ((s)->inst.val)
#define INSTPAT_MATCH(s, name, type)             \
    {                                                                    \
        decode_operand(s, concat(TYPE_, type)); \
    }

    INSTPAT_START();
    INSTPAT("??????? ????? ????? ??? ????? 00101 11", auipc, U, R(rd) = s->pc + imm);
}