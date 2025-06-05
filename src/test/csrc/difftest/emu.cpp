#include "emu.h"
#include "decode.h"
#include "dpic_port.h"
#include "pmem.h"
extern diff_context_t ref_cpu_state;
extern bool commit;
extern uint8_t commit_v_index;
extern uint8_t commit_veew ;
extern bool commit_isfp ;
extern uint8_t commit_rf_addr ;
extern uint8_t commit_group_size ;
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

    current_instance = this;
    parse_args(argc, argv);
    // Verilator相关初始化
    contextp->commandArgs(argc, argv);
    contextp->traceEverOn(true);
    dut_ptr->trace(wave, 5);
    // wave->open("build/vpu.fst");
    wave->open(args.waveform_file);
    wave->set_time_unit("ps");
    

    // 硬件复位
    reset_ncycles(3);

    present->pc = 0x80000000;
    next->pc = 0x80000000;
    dut_ptr->io_dispatch_s2v_bits_robIdx_flag =false;

    //initialize log file
    // log_file.open("emu.log",std::ios::out | std::ios::trunc);
    log_file.open(args.log_file_name,std::ios::out | std::ios::trunc);
    if (log_file.is_open()) {
        log_initialized = true;
        log_file << "==== Simulation Start ====\n";
        log_file.flush();
        printf("==== Simulation Start ====\n"); 
    }else {
        std::cerr << "无法打开日志文件！错误: " << strerror(errno) << std::endl;
    }

}

// 析构函数释放内存
Emulator::~Emulator() {
    current_instance = nullptr; 
    delete wave;
    delete dut_ptr;
    delete contextp;
    if (log_initialized) {
        log_file << "==== Simulation End ====\n";
        log_file.flush();
        log_file.close();
        printf("==== Simulation End ====\n");
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
    // std::stringstream ss;
    // ss << "Before eval (clock=1)" <<"\n";
    dut_ptr->clock = 1;
    dut_ptr->eval();
    // ss << "After eval (clock=1)";
    wave->dump(sim_time); // simulation time
    sim_time += CLK_PERIOD / 2;
    dut_ptr->clock = 0;
    dut_ptr->eval();
    wave->dump(sim_time); // simulation time
    sim_time += CLK_PERIOD / 2;
    // log(ss.str());
}

int Emulator::tick()
{
    clear_flags(*next);
    clear_flags(*present);
    single_cycle();//DUT executes one cycle
    if(commit == false){// avoid infinite loop
        uncommit_cycle += 1;
        if (uncommit_cycle > 100){
            std::stringstream ss;
            ss << "Uncommitted cycle exceeds 100 at simulation time = "<< std::to_string(get_sim_time()) << " ps, PC=0x" << std::hex << std::setfill('0') << std::setw(8) << present->pc << ".";
            log(ss.str());
            trapCode = STATE_BADTRAP;
            return 0;
        }
    }else {
        uncommit_cycle = 0;
    }

    if (total_vector_instr < 16)
    {
        ref_difftest_exec(1);//REF executes one cycle
        present->pc = next->pc;
        ref_difftest_regcpy(&ref_cpu_state, DIFFTEST_TO_DUT, 0);
        present->inst.val = mem_addr_read(present->pc,present->pc, 4);
        decode_instr(present.get());

        if (present->is_vec == true)
        {
            vpu_state_store();
            #ifdef VEC_STORE_TRACE
            std::stringstream ss;
            ss << "[ISSUE] The result of Vector instruction PC = 0x" 
                << std::hex << std::setfill('0') << std::setw(8) << present->pc << ", instr=0x"
               << std::hex << std::setfill('0') << std::setw(8) << present->inst.val <<" is stored in output_pool["<<std::to_string(store_ptr)<<"]! ";
            ss <<"ROB index is 0x" << std::hex<< static_cast<unsigned>(dut_ptr->io_dispatch_s2v_bits_robIdx_value) << ", ";
                ss <<"ROB flag is " <<+dut_ptr->io_dispatch_s2v_bits_robIdx_flag << ".";
            log(ss.str());
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
            breakpoint = true;
        }
        else if (present->is_scalar_store == true)
        {
            log("Here is a scalar store instruction!");
            while (total_vector_instr > 0)
            {
                if (commit == true)//in case the same instruction commits two cycles....
                {
                #ifdef VEC_STORE_TRACE
                    std::stringstream ss;
                    ss << "[COMMIT] The result of Vector instruction PC = 0x" 
                       << std::hex << std::setfill('0') << std::setw(8) << ref_output_pool[cur_vec_ptr].pc << ", instr=0x"
                       << std::hex << std::setfill('0') << std::setw(8) << ref_output_pool[cur_vec_ptr].inst.val <<" is fetched from output_pool["<<std::to_string(cur_vec_ptr) 
                       <<"] at simulation time = "<< std::to_string(get_sim_time()-10)<<" ps! ";
                    ss <<"ROB index is 0x" << std::hex<< static_cast<unsigned>(ref_output_pool[cur_vec_ptr].robidx) << ", ";
                    ss <<"ROB flag is " <<ref_output_pool[cur_vec_ptr].robIdx_flag << ".";
                    log(ss.str());
                #endif
                    log("v["+std::to_string(commit_v_index)+"] is committed!");
                    if(check_vregs_state(&dut_state) ==false) {
                        trapCode=STATE_BADTRAP;
                        return 0;
                    }else {
                        // printf("PC=0x%016lx,instr=0x%08x  passes!\n",ref_output_pool[cur_vec_ptr].pc,ref_output_pool[cur_vec_ptr].inst.val);
                        std::stringstream ss;
                        ss << "PC=0x" << std::hex << std::setfill('0') << std::setw(8) << ref_output_pool[cur_vec_ptr].pc<<", instr=0x"  \
                        << std::hex << std::setfill('0') << std::setw(8) << ref_output_pool[cur_vec_ptr].inst.val<< " passes! \n";
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
        }else if (breakpoint==true && total_vector_instr == 0){
            trapCode=STATE_STOPPED;
            return 0;
        }
        next->pc =ref_cpu_state.pc;
        next->inst.val = mem_addr_read(next->pc,next->pc, 4);
        decode_instr(next.get());
        if(next->is_vec == true)
        {
            // ref_output_pool[store_ptr].pc=next->pc;
            // ref_output_pool[store_ptr].inst.val=next->inst.val;
            int loop=0;
            while(dut_ptr->io_dispatch_s2v_ready!=1)
            {
                single_cycle();
                loop++;
                if(loop>40){
                    std::stringstream ss;
                    ss << "VPU is busy at simulation time = "<< std::to_string(get_sim_time()) << " ps, PC=0x" << std::hex << std::setfill('0') << std::setw(8) << next->pc << ", instr=0x"
                       << std::hex << std::setfill('0') << std::setw(8) << next->inst.val<< ".\n";
                    log(ss.str());
                    trapCode = STATE_BADTRAP;
                    return 0;
                }
            }
            dut_ptr->io_dispatch_s2v_valid=1; 
            dut_ptr->io_dispatch_s2v_bits_robIdx_flag=(robIdx==0xff?~dut_ptr->io_dispatch_s2v_bits_robIdx_flag:dut_ptr->io_dispatch_s2v_bits_robIdx_flag); 
            dut_ptr->io_dispatch_s2v_bits_robIdx_value=robIdx++; 
            dut_ptr->io_dispatch_s2v_bits_inst=next->inst.val;
            dut_ptr->io_dispatch_s2v_bits_rs1=next->rs1;
            dut_ptr->io_dispatch_s2v_bits_rs2=next->rs2;
        }else if(next->is_vec_store == true){
            int loop=0;
             while(dut_ptr->io_dispatch_s2v_ready!=1)
            {
                single_cycle();
                loop++;
                if(loop>40){
                    std::stringstream ss;
                    ss << "VPU is busy at simulation time = "<< std::to_string(get_sim_time()) << " ps, PC=0x" << std::hex << std::setfill('0') << std::setw(8) << next->pc << ", instr=0x"
                       << std::hex << std::setfill('0') << std::setw(8) << next->inst.val<< ".\n";
                    log(ss.str());
                    trapCode = STATE_BADTRAP;
                    return 0;
                }
            }
            dut_ptr->io_dispatch_s2v_valid=1; 
            dut_ptr->io_dispatch_s2v_bits_robIdx_flag=(robIdx==0xff?~dut_ptr->io_dispatch_s2v_bits_robIdx_flag:dut_ptr->io_dispatch_s2v_bits_robIdx_flag); 
            dut_ptr->io_dispatch_s2v_bits_robIdx_value=robIdx++; 
            dut_ptr->io_dispatch_s2v_bits_inst=next->inst.val;
            dut_ptr->io_dispatch_s2v_bits_rs1=next->rs1;
            dut_ptr->io_dispatch_s2v_bits_rs2=next->rs2;
            std::stringstream ss;
            ss << "[ISSUE] Vector store instruction 0x" << std::hex << std::setfill('0') << std::setw(8) << next->inst.val << " is sent to VPU at PC = 0x"
               << std::hex << std::setfill('0') << std::setw(8) <<next->pc<<" , simulation time = " 
                << std::to_string(get_sim_time()) << " ps! ";
             ss <<"ROB index is 0x" << std::hex<< static_cast<unsigned>(dut_ptr->io_dispatch_s2v_bits_robIdx_value) << ", ";
                ss <<"ROB flag is " <<+dut_ptr->io_dispatch_s2v_bits_robIdx_flag << ".";
            log(ss.str());
        }
        else{
            dut_ptr->io_dispatch_s2v_valid=0;
        }
    }else{
        dut_ptr->io_dispatch_s2v_valid = 0;
    }
    if(commit ==true)
    {
    #ifdef VEC_STORE_TRACE
        std::stringstream ss;
        ss << "[COMMIT] The result of Vector instruction PC = 0x" 
            << std::hex << std::setfill('0') << std::setw(8) << ref_output_pool[cur_vec_ptr].pc << ", instr=0x"
            << std::hex << std::setfill('0') << std::setw(8) << ref_output_pool[cur_vec_ptr].inst.val <<" is fetched from output_pool["<<std::to_string(cur_vec_ptr)
            <<"] at simulation time = "<< std::to_string(get_sim_time()-10)<<" ps! ";
        ss <<"ROB index is 0x" << std::hex<< static_cast<unsigned>(ref_output_pool[cur_vec_ptr].robidx) << ", ";
        ss <<"ROB flag is " <<ref_output_pool[cur_vec_ptr].robIdx_flag << ".";
        log(ss.str());
     #endif
        log("v["+std::to_string(commit_v_index)+"] is committed!");
        if(check_vregs_state(&dut_state) ==false) {
            trapCode=STATE_BADTRAP;
            return 0;
        }else {
            // printf("PC=0x%016lx,instr=0x%08x  passes!\n",ref_output_pool[cur_vec_ptr].pc,ref_output_pool[cur_vec_ptr].inst.val);
            std::stringstream ss;
            ss << "PC=0x" << std::hex << std::setfill('0') << std::setw(8) << ref_output_pool[cur_vec_ptr].pc << ", instr=0x"
               << std::hex << std::setfill('0') << std::setw(8) << ref_output_pool[cur_vec_ptr].inst.val << " passes! \n";
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
    ref_output_pool[store_ptr].pc=present->pc;
    ref_output_pool[store_ptr].inst.val=present->inst.val;
    ref_output_pool[store_ptr].robidx=dut_ptr->io_dispatch_s2v_bits_robIdx_value;
    ref_output_pool[store_ptr].robIdx_flag=dut_ptr->io_dispatch_s2v_bits_robIdx_flag;
}

void Emulator::clear_flags(Decode &s) {
    s.is_vec            = false;
    s.is_vec_cfg        = false;
    s.is_vec_store      = false;
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
    int ele_width = 8;
    switch(commit_veew){
        case 0b000: ele_width = 8; break;
        case 0b001: ele_width =16; break;
        case 0b010: ele_width =32; break;
        case 0b011: ele_width =64; break;
        default: ele_width =0;log("Unsupported FP SEW: " + std::to_string(commit_veew)); break;
    }
    int elems_per_reg =VLEN / ele_width;
    for(int i=commit_rf_addr;i<commit_rf_addr + commit_group_size;i++){
        for(int j=0;j<VLEN/elems_per_reg;j++){
            bool mismatch = false;
            if (commit_isfp) {
                switch (ele_width) {
                    case 8: {
                        // TODO: 处理fp8
                        uint8_t ref = ref_output_pool[cur_vec_ptr].vr[i]._8[j];
                        uint8_t dut_val = dut->vr[i]._8[j];
                        mismatch = (ref != dut_val); 
                        break;
                    }
                    case 16: {
                        // TODO: 处理fp16
                        half_float::half fp16_val_src1;
                        half_float::half fp16_val_src2;
                        uint16_t ref_raw = ref_output_pool[cur_vec_ptr].vr[i]._16[j];
                        uint16_t dut_raw = dut->vr[i]._16[j];

                        //无符号数复制到fp16
                        std::memcpy(&fp16_val_src1, &ref_raw, sizeof(uint16_t));
                        std::memcpy(&fp16_val_src2, &dut_raw, sizeof(uint16_t));

                        //转换成fp32 进行误差分析
                        float f_ref = static_cast<float>(fp16_val_src1);
                        float f_dut = static_cast<float>(fp16_val_src2);

                        float abs_err = fabsf(f_ref - f_dut);
                        float denom = fmaxf(fabsf(f_ref), 1e-10f);
                        float rel_err = fabsf(f_ref - f_dut) / (denom); // 防止除 0
                        mismatch = (abs_err > 1e-5f && rel_err > 1e-4f);
                        // if (mismatch) {
                        //     std::stringstream ss;
                        //     ss << "abs_err=" << abs_err << ", rel_err=" << rel_err;
                        //     log(ss.str());
                        // }
                        break;
                    }
                    case 32: {
                        float ref = *reinterpret_cast<float*>(&ref_output_pool[cur_vec_ptr].vr[i]._32[j]);
                        float dut_val = *reinterpret_cast<float*>(&dut->vr[i]._32[j]);
                        float abs_err = fabsf(ref - dut_val);
                        float denom = fmaxf(fabsf(ref), 1e-10f);
                        float rel_err = fabsf(ref - dut_val) / (denom); // 防止除 0
                        mismatch = (abs_err > 1e-5f && rel_err > 1e-4f);
                        // if (mismatch) {
                        //     std::stringstream ss;
                        //     ss << "abs_err=" << abs_err << ", rel_err=" << rel_err;
                        //     log(ss.str());
                        // }
                        break;
                    }
                    case 64: {
                        double ref = *reinterpret_cast<double*>(&ref_output_pool[cur_vec_ptr].vr[i]._64[j]);
                        double dut_val = *reinterpret_cast<double*>(&dut->vr[i]._64[j]);
                        double abs_err = fabs(ref - dut_val);
                        double denom = fmax(fabs(ref), 1e-14f);
                        double rel_err = fabs(ref - dut_val) / (denom);
                        mismatch = (abs_err > 1e-10 && rel_err > 1e-9);
                        // if (mismatch) {
                        //     std::stringstream ss;
                        //     ss << "abs_err=" << abs_err << ", rel_err=" << rel_err;
                        //     log(ss.str());
                        // }
                        break;
                    }
                    default:
                        log("Unsupported FP Element width: " + std::to_string(ele_width));
                        return false;
                }
            } else {
                switch (ele_width) {
                    case 8:
                        mismatch = (dut->vr[i]._8[j] != ref_output_pool[cur_vec_ptr].vr[i]._8[j]);
                        break;
                    case 16:
                        mismatch = (dut->vr[i]._16[j] != ref_output_pool[cur_vec_ptr].vr[i]._16[j]);
                        break;
                    case 32:
                        mismatch = (dut->vr[i]._32[j] != ref_output_pool[cur_vec_ptr].vr[i]._32[j]);
                        break;
                    case 64:
                        mismatch = (dut->vr[i]._64[j] != ref_output_pool[cur_vec_ptr].vr[i]._64[j]);
                        break;
                    default:
                        log("Unsupported int Element width: " + std::to_string(ele_width));
                        return false;
                }
            }
            if(mismatch == true){
                std::stringstream ss;
                ss <<"\n";
                // ss << "commit_veew="<<std::to_string(commit_veew) << ", commit_isfp=" << commit_isfp << "\n";
                ss << "VPU state mismatch at simulation time = "<< std::to_string(get_sim_time()-10) << " ps, PC=0x" << std::hex << std::setfill('0') << std::setw(8) << ref_output_pool[cur_vec_ptr].pc << ", instr=0x"
                   << std::hex << std::setfill('0') << std::setw(8) << ref_output_pool[cur_vec_ptr].inst.val << ". ";
                ss <<"It is dispatched to vpu at simulation time = "<<std::to_string( ref_output_pool[cur_vec_ptr].issued_time)<<" ps. ";
                ss <<"ROB index is 0x" << std::hex<< static_cast<unsigned>(ref_output_pool[cur_vec_ptr].robidx) << ", ";
                ss <<"ROB flag is " <<ref_output_pool[cur_vec_ptr].robIdx_flag << ".\n";
                switch(ele_width) {
                  case 8: {
                      ss << "v[" << i << "][" << j << "]: expected 0x"
                         << std::hex << std::setfill('0') << std::setw(8)
                         << ref_output_pool[cur_vec_ptr].vr[i]._8[j] << ", got 0x"
                         << std::hex << std::setfill('0') << std::setw(8)
                         << dut->vr[i]._8[j] << "\n";
                         print_vector_register<uint8_t>(ss, i, ref_output_pool[cur_vec_ptr].vr[i], dut->vr[i],16);
                         break;
                  }
                  case 16: {
                      ss << "v[" << i << "]["  << j << "]: expected 0x"
                         << std::hex << std::setfill('0') << std::setw(4)
                         << ref_output_pool[cur_vec_ptr].vr[i]._16[j] << ", got 0x"
                         << std::hex << std::setfill('0') << std::setw(4)
                         << dut->vr[i]._16[j] << "\n";
                         print_vector_register<uint16_t>(ss, i, ref_output_pool[cur_vec_ptr].vr[i], dut->vr[i],8);
                         break;
                  } 
                  case 32: {
                      ss << "v[" << i << "]["  << j << "]: expected 0x"
                         << std::hex << std::setfill('0') << std::setw(8)
                         << ref_output_pool[cur_vec_ptr].vr[i]._32[j] << ", got 0x"
                         << std::hex << std::setfill('0') << std::setw(8)
                         << dut->vr[i]._32[j] << "\n";
                         print_vector_register<uint32_t>(ss, i, ref_output_pool[cur_vec_ptr].vr[i], dut->vr[i],4);
                         break;
                  } 
                  case 64: {
                      ss << "v[" << i << "][" << j << "]: expected 0x"
                         << std::hex << std::setfill('0') << std::setw(8)
                         << ref_output_pool[cur_vec_ptr].vr[i]._64[j] << ", got 0x"
                         << std::hex << std::setfill('0') << std::setw(8)
                         << dut->vr[i]._64[j] << "\n";
                         print_vector_register<uint64_t>(ss, i, ref_output_pool[cur_vec_ptr].vr[i], dut->vr[i],2);
                         break;
                  }  
                }


                // /*print register value*/
                // ss <<"REF model [v" << std::to_string(i) <<"]: \n";
                // for(int element = 0; element < VLEN/32;  element++) {
                //     ss << "[" << std::setfill('0') << std::setw(2) << std::to_string(element) << "] " 
                //         << std::hex << std::setfill('0') << std::setw(8) 
                //         << ref_output_pool[cur_vec_ptr].vr[i]._32[element] <<" ";
                //     if (element % 4 == 3)   {
                //         ss <<"\n";
                //     }
                // }
                // ss <<"DUT [v" << std::to_string(i) <<"]: \n";
                // for(int element = 0; element < VLEN/32;  element++) {
                //     ss << "[" << std::setfill('0') << std::setw(2) << std::to_string(element) << "] " 
                //         << std::hex << std::setfill('0') << std::setw(8) 
                //         << dut->vr[i]._32[element] <<" ";
                //     if (element % 4 == 3)
                //     {
                //         ss << "\n";
                //     }
                // }
                log(ss.str());
                return false;
            }
        }
    }
    return true;
}

bool Emulator::is_finished() {
    return trapCode ==STATE_BADTRAP || contextp->gotFinish() || trapCode == STATE_STOPPED;
}

void Emulator::log(const std::string& message) {
    if (log_initialized) {
        log_file << message << "\n";
        log_file.flush();
    }
    // 同时输出到控制台
    // printf("%s\n", message.c_str()); 
}
int Emulator::parse_args(int argc, const char *argv[])
{
  const struct option table[] = {
      {"log", required_argument, NULL, 'l'},
      {"waveform", required_argument, NULL, 'w'},
      {"help", no_argument, NULL, 'h'},
      {0, 0, NULL, 0},
  };
  int o;
  while ((o = getopt_long(argc, const_cast<char *const *>(argv), "-hl:w:", table, NULL)) != -1)
  {
    switch (o)
    {
    case 'l':args.log_file_name = optarg;break;
    case 'w':args.waveform_file = optarg;break;
    case 1:args.img_file = optarg;return 0;
    default:
      printf("Usage: %s [OPTION...] IMAGE [args]\n\n", argv[0]);
      printf("\t-l,--log=FILE           output log to FILE\n");
      printf("\t-w,--waveform=waveform file         waveform file name\n");
      printf("\n");
      exit(0);
    }
  }
  return 0;
}
