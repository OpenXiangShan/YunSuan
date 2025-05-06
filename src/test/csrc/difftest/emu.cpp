#include "emu.h"
#include "decode.h"
#include "difftest.h"
extern diff_context_t ref_cpu_state;
// Emulator::Emulator(int argc, const char *argv[]) {

//     sim_time=0;

//     contextp = new VerilatedContext;
//     contextp->commandArgs(argc, argv);

//     dut_ptr = new VVTopDebug{contextp};
//     wave = new VerilatedVcdC;

//     contextp->traceEverOn(true);
//     dut_ptr->trace(wave, 5);
//     wave->open("build/vpu.vcd");
//     wave->set_time_unit("ns");
//     trapCode = STATE_RUNNING;

//     // init core
//     reset_ncycles(3);

//     total_vector_instr = 0;
//     store_ptr = 0;
//     cmp_ptr = 0;

//     present = new Decode();  // 分配内存
//     next = new Decode();
//     present->pc= 0x80000000;
//     next->pc= 0x80000000;
// }

Emulator::Emulator(int argc, const char *argv[])
    : contextp(new VerilatedContext),   // 初始化列表显式初始化成员
      dut_ptr(new VVTopDebug{contextp}),
      wave(new VerilatedVcdC),
      total_vector_instr(0),
      sim_time(0),
      trapCode(STATE_RUNNING),
      next_pc(0x80000000),
      present_pc(0x80000000),
      store_ptr(0),
      cmp_ptr(0),
      present(std::make_unique<Decode>()),
      next(std::make_unique<Decode>()),
      robIdx(0){    // 初始化列表顺序与类声明一致

    // Verilator相关初始化
    contextp->commandArgs(argc, argv);
    contextp->traceEverOn(true);
    dut_ptr->trace(wave, 5);
    wave->open("build/vpu.vcd");
    wave->set_time_unit("ns");

    // 硬件复位
    reset_ncycles(3);

    present->pc = 0x80000000;
    next->pc = 0x80000000;
    dut_ptr->io_dispatch_s2v_bits_robIdx_flag =false;
}

// 析构函数释放内存
Emulator::~Emulator() {
    delete wave;
    delete dut_ptr;
    delete contextp;
}

// Emulator::~Emulator() {
//     if(wave) wave->close();
//     delete wave;
//     delete dut_ptr;
//     delete contextp;
// }

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

int Emulator::tick()
{
    clear_flags(*next);
    clear_flags(*present);
    single_cycle();
    if (total_vector_instr <= 16)
    {
        ref_difftest_exec(1);
        present->pc = next->pc;
        ref_difftest_regcpy(&ref_cpu_state, DIFFTEST_TO_DUT, 0);
        present->inst.val = mem_addr_read(present->pc,present->pc, 4);
        decode_instr(present);

        if (present->is_vec == true)
        {
            vpu_state_store();
            store_ptr = store_ptr == 15 ? 0 : store_ptr + 1;
            total_vector_instr += 1;
        }
        else if(present->is_vector_cfg == true){
            top->io_dispatch_s2v_bits_vcsr_vstart=ref_cpu_state.vstart; 
            top->io_dispatch_s2v_bits_vcsr_vl=ref_cpu_state.vl; 
            top->io_dispatch_s2v_bits_vcsr_vxrm=ref_cpu_state.vxrm; 
            top->io_dispatch_s2v_bits_vcsr_frm=((ref_cpu_state.fcsr)>> 5) &0b111U; 
            top->io_dispatch_s2v_bits_vcsr_ma=(ref_cpu_state.vtype>>7)&0b1U; 
            top->io_dispatch_s2v_bits_vcsr_ta=(ref_cpu_state.vtype>>8)&0b1U; 
            top->io_dispatch_s2v_bits_vcsr_vill=(ref_cpu_state.vtype>>31)&0b1U; 
        }
        else if (present->is_ebreak == true)
        {
            trapCode = STATE_TRAP;
            return 0;
        }
        else if (present->is_scalar_store == true)
        {
            while (total_vector_instr > 0)
            {
                single_cycle();
                // compare stage TODO!!!
                if (dut_ptr->commit == true)
                {
                }
            }
            switch (present->store_instr)
            {
            case SD:
                mem_addr_write(present->pc, present->rs1 + present->imm, ref_cpu_state.gpr[present->rs2], 8);
                break;
            default:
                printf("Unsupported store instruction type!\n");
                assert(0);
            }
        }
        next->pc =ref_cpu_state.pc;
        next->inst.val = mem_addr_read(next->pc,next->pc, 4);
        decode_instr(next);
        if(next->is_vec == true)
        {
            output_pool[store_ptr].pc=next->pc;
            output_pool[store_ptr].inst.val=next->inst.val;
            while(dut->ready!=1)
            {
                single_cycle();
            }
            dut_ptr->io_dispatch_s2v_valid=1; 
            dut_ptr->io_dispatch_s2v_bits_robIdx_flag=(robIdx==0xff?~dut_ptr->io_dispatch_s2v_bits_robIdx_flag:dut_ptr->io_dispatch_s2v_bits_robIdx_flag); 
            dut_ptr->io_dispatch_s2v_bits_robIdx_value=robIdx++; 
            dut_ptr->io_dispatch_s2v_bits_inst=next->inst.val;
            dut_ptr->io_dispatch_s2v_bits_rs1=next->rs1;
            dut_ptr->io_dispatch_s2v_bits_rs2=next->rs2;
        }else
        {
            dut_ptr->io_dispatch_s2v_valid=0;
        }
    }
    if(dut_ptr->commit ==true)
    {
        //TODO:COMPARE
    }
}

int Emulator::decode_instr(Decode *s) {
    bool is_vec;
    bool is_scalar_store;
    bool is_scalar_gpr;
    bool is_fp_reg;
    uint64_t rs1;
    uint64_t rs2;

#define INSTPAT_INST(s) ((s)->inst.val)
#define INSTPAT_MATCH(s, name, type, ...)             \
    {                                                                    \
        decode_operand(s, concat(TYPE_, type)); \
        __VA_ARGS__ ;\
    }

    INSTPAT_START();
    INSTPAT("??????? ????? ????? 011 ????? 01000 11", sd, S,s->is_scalar_store = true,s->store_instr=SD);
    INSTPAT("0000000 00001 00000 000 00000 11100 11", ebreak, I, s->is_ebreak =true );
    INSTPAT("??? 0 00 ? ????? ????? 000 ????? 00001 11", vl*, VL);
    INSTPAT("??? 0 00 ? ????? ????? 101 ????? 00001 11", vl*, VL);
    INSTPAT("??? 0 00 ? ????? ????? 11? ????? 00001 11", vl*, VL);
    INSTPAT("??? 0 10 ? ????? ????? 000 ????? 00001 11", vls*, VLS);
    INSTPAT("??? 0 10 ? ????? ????? 101 ????? 00001 11", vls*, VLS);
    INSTPAT("??? 0 10 ? ????? ????? 11? ????? 00001 11", vls*, VLS);
    INSTPAT("??? 0 ?1 ? ????? ????? 000 ????? 00001 11", vlx*, VLX);
    INSTPAT("??? 0 ?1 ? ????? ????? 101 ????? 00001 11", vlx*, VLX);
    INSTPAT("??? 0 ?1 ? ????? ????? 11? ????? 00001 11", vlx*, VLX);

    INSTPAT("??? 0 00 ? ????? ????? 000 ????? 01001 11", vs*,  VS);
    INSTPAT("??? 0 00 ? ????? ????? 101 ????? 01001 11", vs*,  VS);
    INSTPAT("??? 0 00 ? ????? ????? 11? ????? 01001 11", vs*,  VS);
    INSTPAT("??? 0 10 ? ????? ????? 000 ????? 01001 11", vss*, VSS);
    INSTPAT("??? 0 10 ? ????? ????? 101 ????? 01001 11", vss*, VSS);
    INSTPAT("??? 0 10 ? ????? ????? 11? ????? 01001 11", vss*, VSS);
    INSTPAT("??? 0 ?1 ? ????? ????? 000 ????? 01001 11", vsx*, VSX);
    INSTPAT("??? 0 ?1 ? ????? ????? 101 ????? 01001 11", vsx*, VSX);
    INSTPAT("??? 0 ?1 ? ????? ????? 11? ????? 01001 11", vsx*, VSX);

    INSTPAT("?????? ? ????? ????? 000 ????? 10101 11", ivector-vector, OPIVV);
    INSTPAT("?????? ? ????? ????? 001 ????? 10101 11", fvector-vector, OPFVV);
    INSTPAT("?????? ? ????? ????? 010 ????? 10101 11", mvector-vector, OPMVV);
    INSTPAT("?????? ? ????? ????? 011 ????? 10101 11", ivector-immediate, OPIVI);
    INSTPAT("?????? ? ????? ????? 100 ????? 10101 11", ivector-scalar, OPIVX);
    INSTPAT("?????? ? ????? ????? 101 ????? 10101 11", fvector-scalar, OPFVF);
    INSTPAT("?????? ? ????? ????? 110 ????? 10101 11", mvector-scalar, OPMVX);
    INSTPAT("?????? ? ????? ????? 111 ????? 10101 11", vector-cfg, OPCFG);



    INSTPAT_END();

	//R(0) = 0; // reset $zero to 0

	return 0;
}

void Emulator::vpu_state_store(){
    for (int i=0;i<32;i++){
        for(int j=0; j < VENUM64; j++){
            ref_ouput_pool[store_ptr].vr[i]._64[j] = ref_cpu_state.vr[i]._64[j];
        }
    }
}

void clear_flags(Decode &s) {
    s.is_vec            = false;
    s.is_vec_cfg        = false;
    s.is_scalar_store   = false;
    s.is_scalar_gpr     = false;
    s.is_fp_reg         = false;
    s.is_ebreak         = false;
    s.inst,val          = 0    ;
    s.rs1               = 0    ;
    s.rs2               = 0    ;
    s.imm               = 0    ;
    s.store_instr       = 0    ;
}