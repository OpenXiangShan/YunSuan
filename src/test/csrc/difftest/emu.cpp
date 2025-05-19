#include "emu.h"
#include "decode.h"
#include "dpic_port.h"
#include "pmem.h"
extern diff_context_t ref_cpu_state;
extern bool commit;
extern uint8_t commit_v_index;
#define VEC_STORE_TRACE
Emulator* Emulator::current_instance = nullptr; 
Emulator::Emulator(int argc, const char *argv[])
    : contextp(new VerilatedContext),   // 初始化列表显式初始化成员
      dut_ptr(new VVTopDebug{contextp}),
      wave(new VerilatedFstC),
      total_vector_instr(0),
      sim_time(0),
      trapCode(STATE_RUNNING),
      store_ptr(0),
      cur_vec_ptr(0),
      present(std::make_unique<Decode>()),
      next(std::make_unique<Decode>()),
      robIdx(0){    // 初始化列表顺序与类声明一致

    // Verilator相关初始化
    contextp->commandArgs(argc, argv);
    contextp->traceEverOn(true);
    dut_ptr->trace(wave, 5);
    wave->open("build/vpu.fst");
    wave->set_time_unit("ns");

    // 硬件复位
    reset_ncycles(3);

    present->pc = 0x80000000;
    next->pc = 0x80000000;
    dut_ptr->io_dispatch_s2v_bits_robIdx_flag =false;

    //initialize log file
    log_file.open("emu.log",std::ios::out | std::ios::trunc);
    if (log_file.is_open()) {
        log_initialized = true;
        log_file << "==== Simulation Start ====\n";
        log_file.flush();
    }else {
        std::cerr << "无法打开日志文件！错误: " << strerror(errno) << std::endl;
    }

}

// 析构函数释放内存
Emulator::~Emulator() {
    delete wave;
    delete dut_ptr;
    delete contextp;
    if (log_initialized) {
        log_file << "==== Simulation End ====\n";
        log_file.flush();
        log_file.close();
    }
}

void Emulator::reset_ncycles(size_t n) {
    dut_ptr->reset = 1;
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
    single_cycle();//DUT executes one cycle
    if (total_vector_instr <= 16)
    {
        // if(dut_state.vr[5]._32[0] !=0){
        //     std::stringstream ss;
        //     ss << "v5!=0, The present pc is=0x" << std::hex << ref_output_pool[cur_vec_ptr].pc;
        //     log(ss.str());
        // } 
        ref_difftest_exec(1);//REF executes one cycle
        present->pc = next->pc;
        ref_difftest_regcpy(&ref_cpu_state, DIFFTEST_TO_DUT, 0);
        present->inst.val = mem_addr_read(present->pc,present->pc, 4);
        decode_instr(present.get());
        
        // if(ref_cpu_state.pc==0x80000240||ref_cpu_state.pc==0x8000026c){
        //     printf("present->pc=0x%08lx\n",present->pc);
        //     printf("next->pc=0x%08lx\n",next->pc);
        //     for(int i=0; i<32;i++){
        //         printf("gpr[%02d]:0x%08lx ",i,ref_cpu_state.gpr[i]);
        //         if(i%4==3) printf("\n");
        //     }
        //     // ref_isa_reg_display();
        // }

        if (present->is_vec == true)
        {
            vpu_state_store();
            #ifdef VEC_STORE_TRACE
            // printf("A vector instr result is stored in output_pool[%d]!\n",store_ptr);
            log("A vector instr result is stored in output_pool["+std::to_string(store_ptr)+"]!");
            #endif
            store_ptr = store_ptr == 15 ? 0 : store_ptr + 1;
            total_vector_instr += 1;
        }
        else if(present->is_vec_cfg == true){
            dut_ptr->io_dispatch_s2v_bits_vcsr_vstart=ref_cpu_state.vstart; 
            dut_ptr->io_dispatch_s2v_bits_vcsr_vl=ref_cpu_state.vl; 
            dut_ptr->io_dispatch_s2v_bits_vcsr_vxrm=ref_cpu_state.vxrm; 
            dut_ptr->io_dispatch_s2v_bits_vcsr_frm=((ref_cpu_state.fcsr)>> 5) &0b111U; 
            dut_ptr->io_dispatch_s2v_bits_vcsr_ma=(ref_cpu_state.vtype>>7)&0b1U; 
            dut_ptr->io_dispatch_s2v_bits_vcsr_ta=(ref_cpu_state.vtype>>8)&0b1U; 
            dut_ptr->io_dispatch_s2v_bits_vcsr_vill=(ref_cpu_state.vtype>>31)&0b1U;
            dut_ptr->io_dispatch_s2v_bits_vcsr_vsew =(ref_cpu_state.vtype>>3)&0b111U;
            dut_ptr->io_dispatch_s2v_bits_vcsr_vlmul=(ref_cpu_state.vtype)&0b111U;
        }
        else if (present->is_ebreak == true)
        {
            trapCode = STATE_TRAP;
            return 0;
        }
        else if (present->is_scalar_store == true)
        {
            log("Here is a scalar store instruction!");
            while (total_vector_instr > 0)
            {
                if (commit == true)//in case the same instruction commits two cycles....
                {
                    #ifdef VEC_STORE_TRACE
                    // printf("A vector instr result is fetched from output_pool[%d]!\n",cur_vec_ptr);
                    log("A vector instr result is fetched from output_pool["+std::to_string(cur_vec_ptr)+"]!");
                    #endif
                    log("v["+std::to_string(commit_v_index)+"] is committed!");
                    if(check_vregs_state(&dut_state) ==false) {
                        // printf("VPU state mismatch at pc=0x%016lx, instr=0x%08x\n", ref_output_pool[cur_vec_ptr].pc,ref_output_pool[cur_vec_ptr].inst.val);
                        // std::stringstream ss;
                        // ss << "VPU state mismatch at simulation time= "<< std::to_string(get_sim_time()) << "PC=0x" << std::hex << std::setfill('0') << std::setw(16) << ref_output_pool[cur_vec_ptr].pc<<", instr=0x"  \
                        << std::hex << std::setfill('0') << std::setw(8) << ref_output_pool[cur_vec_ptr].inst.val<< " ";
                        // log(ss.str());
                        trapCode=STATE_TRAP;
                        return 0;

                    }else {
                        // printf("PC=0x%016lx,instr=0x%08x  passes!\n",ref_output_pool[cur_vec_ptr].pc,ref_output_pool[cur_vec_ptr].inst.val);
                        std::stringstream ss;
                        ss << "PC=0x" << std::hex << std::setfill('0') << std::setw(16) << ref_output_pool[cur_vec_ptr].pc<<", instr=0x"  \
                        << std::hex << std::setfill('0') << std::setw(8) << ref_output_pool[cur_vec_ptr].inst.val<< " passes! ";
                        log(ss.str());
                    }
                    cur_vec_ptr = cur_vec_ptr ==15 ? 0 : cur_vec_ptr + 1;
                    total_vector_instr= total_vector_instr > 0? total_vector_instr-1 :0;
                    commit=false;
                }
                single_cycle();
            }
            switch (present->store_instr)
            {
            case SD:
                // printf("present->pc=0x%08lx\n",present->pc);
                // printf("next->pc=0x%08lx\n",next->pc);
                // printf("rs1=%lx\n",present->rs1);
                // printf("rs2=%lx\n",present->rs2);
                // printf("imm=%lx\n",present->imm);
                mem_addr_write(present->pc, present->rs1 + present->imm, present->rs2, 8);
                break;
            default:
                printf("Unsupported store instruction type!\n");
                assert(0);
            }
        }
        next->pc =ref_cpu_state.pc;
        next->inst.val = mem_addr_read(next->pc,next->pc, 4);
        decode_instr(next.get());
        if(next->is_vec == true)
        {
            ref_output_pool[store_ptr].pc=next->pc;
            ref_output_pool[store_ptr].inst.val=next->inst.val;
            while(dut_ptr->io_dispatch_s2v_ready!=1)
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
    if(commit ==true)
    {
        #ifdef VEC_STORE_TRACE
        // printf("A vector instr result is fetched from output_pool[%d]!\n",cur_vec_ptr);
        log("A vector instr result is fetched from output_pool["+std::to_string(cur_vec_ptr)+"]!");
        #endif
        log("v["+std::to_string(commit_v_index)+"] is committed!");
        // if(dut_state.vr[5]._32[0] !=0){
        //     std::stringstream ss;
        //     ss << "v5!=0, The present pc is=0x" << std::hex << ref_output_pool[cur_vec_ptr].pc;
        //     log(ss.str());
        // } 
        if(check_vregs_state(&dut_state) ==false) {
            // printf("VPU state mismatch at pc=0x%016lx, instr=0x%08x\n", ref_output_pool[cur_vec_ptr].pc,ref_output_pool[cur_vec_ptr].inst.val);
            // std::stringstream ss;
            // ss << "VPU state mismatch at simulation time= "<< std::to_string(get_sim_time()) << ", PC=0x" \
               << std::hex << std::setfill('0') << std::setw(16) << ref_output_pool[cur_vec_ptr].pc << ", instr=0x"\
               << std::hex << std::setfill('0') << std::setw(8) << ref_output_pool[cur_vec_ptr].inst.val << " ";
            // log(ss.str());
            trapCode=STATE_TRAP;
            return 0;
        }else {
            // printf("PC=0x%016lx,instr=0x%08x  passes!\n",ref_output_pool[cur_vec_ptr].pc,ref_output_pool[cur_vec_ptr].inst.val);
            std::stringstream ss;
            ss << "PC=0x" << std::hex << std::setfill('0') << std::setw(16) << ref_output_pool[cur_vec_ptr].pc << ", instr=0x"
               << std::hex << std::setfill('0') << std::setw(8) << ref_output_pool[cur_vec_ptr].inst.val << " passes! ";
            log(ss.str());
        }
        
        cur_vec_ptr = cur_vec_ptr ==15 ? 0 : cur_vec_ptr + 1;
        total_vector_instr= total_vector_instr > 0? total_vector_instr-1 :0;
        commit=false;
    }
    
    return 0;
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
    INSTPAT("010000 1 ????? 00000 001 ????? 10101 11", fvector-vector, VFMV_F_S);//skip vfmv.f.s 
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
            ref_output_pool[store_ptr].vr[i]._64[j] = ref_cpu_state.vr[i]._64[j];
        }
    }
    ref_output_pool[store_ptr].issued_time=get_sim_time()-10;
}

void Emulator::clear_flags(Decode &s) {
    s.is_vec            = false;
    s.is_vec_cfg        = false;
    s.is_scalar_store   = false;
    s.is_scalar_gpr     = false;
    s.is_fp_reg         = false;
    s.is_ebreak         = false;
    s.inst.val          = 0    ;
    s.rs1               = 0    ;
    s.rs2               = 0    ;
    s.imm               = 0    ;
    s.store_instr       = 0    ;
}

bool Emulator::check_vregs_state(VPU_STATE *dut){
    for(int i=0;i<32;i++){
        for(int j=0;j<VLEN/32;j++){
            if(dut->vr[i]._32[j] != ref_output_pool[cur_vec_ptr].vr[i]._32[j]){
                std::stringstream ss;
                ss << "VPU state mismatch at simulation time = "<< std::to_string(get_sim_time()-10) << " ps, PC=0x" << std::hex << std::setfill('0') << std::setw(16) << ref_output_pool[cur_vec_ptr].pc << ", instr=0x"
                   << std::hex << std::setfill('0') << std::setw(8) << ref_output_pool[cur_vec_ptr].inst.val << ".\n";
                ss <<"It is dispatched to vpu at simulation time = "<<std::to_string( ref_output_pool[cur_vec_ptr].issued_time)<<" ps.\n";
                // printf("VPU state mismatch at vreg[%d][%d]: expected %016x, got %016x\n", i, j, ref_output_pool[cur_vec_ptr].vr[i]._32[j], dut->vr[i]._32[j]);
                ss << "vreg[" << i << "][" << j << "]: expected 0x"
                   << std::hex << std::setfill('0') << std::setw(8)
                   << ref_output_pool[cur_vec_ptr].vr[i]._32[j] << ", got 0x" 
                   << std::hex << std::setfill('0') << std::setw(8)
                   << dut->vr[i]._32[j] << "\n";
                /*print register value*/
                ss <<"REF model [v" << std::to_string(i) <<"]: \n";
                for(int element = 0; element < VLEN/32;  element++) {
                    // printf("[%02d] %08x  ", element , ref_output_pool[cur_vec_ptr].vr[i]._32[element]);
                    ss << "[" << std::setfill('0') << std::setw(2) << std::to_string(element) << "] " 
                        << std::hex << std::setfill('0') << std::setw(8) 
                        << ref_output_pool[cur_vec_ptr].vr[i]._32[element] <<" ";
                    if (element % 4 == 3)   {
                        ss <<"\n";
                        // printf("\n");

                    }
                }
                // printf("DUT model [v%d]: \n",i);
                ss <<"DUT [v" << std::to_string(i) <<"]: \n";
                for(int element = 0; element < VLEN/32;  element++) {
                    // printf("[%02d] %08x  ", element , dut->vr[i]._32[element]);
                    ss << "[" << std::setfill('0') << std::setw(2) << std::to_string(element) << "] " 
                        << std::hex << std::setfill('0') << std::setw(8) 
                        << dut->vr[i]._32[element] <<" ";
                    if (element % 4 == 3)
                    {
                        ss << "\n";
                        // printf("\n");
                    }
                }
                log(ss.str());
                return false;
            }
        }
    }
    return true;
}

bool Emulator::is_finished() {
    return trapCode ==STATE_TRAP || contextp->gotFinish();
}

void Emulator::log(const std::string& message) {
    if (log_initialized) {
        log_file << message << "\n";
        log_file.flush();
    }
    // 同时输出到控制台
    printf("%s\n", message.c_str()); 
}

