#ifndef __DIFFTEST_H__
#define __DIFFTEST_H__

#include "common.h"
#include <dlfcn.h>

#define CONFIG_DIFF_RVV
#define CONFIG_DIFF_FPU
#define CONFIG_DIFF_DEBUG_MODE
#define CONFIG_DIFF_SDTRIG
#define CONFIG_DIFF_RVH

typedef struct {
    uint64_t gpr[32];
  #ifdef CONFIG_DIFF_FPU
    uint64_t fpr[32];
  #endif
    uint64_t priv;
    uint64_t mstatus;
    uint64_t sstatus;
    uint64_t mepc;
    uint64_t sepc;
    uint64_t mtval;
    uint64_t stval;
    uint64_t mtvec;
    uint64_t stvec;
    uint64_t mcause;
    uint64_t scause;
    uint64_t satp;
    uint64_t mip;
    uint64_t mie;
    uint64_t mscratch;
    uint64_t sscratch;
    uint64_t mideleg;
    uint64_t medeleg;
    uint64_t pc;
  
  #ifdef CONFIG_DIFF_RVH
    uint64_t v;
    uint64_t mtval2;
    uint64_t mtinst;
    uint64_t hstatus;
    uint64_t hideleg;
    uint64_t hedeleg;
    uint64_t hcounteren;
    uint64_t htval;
    uint64_t htinst;
    uint64_t hgatp;
    uint64_t vsstatus;
    uint64_t vstvec;
    uint64_t vsepc;
    uint64_t vscause;
    uint64_t vstval;
    uint64_t vsatp;
    uint64_t vsscratch;
  #endif // CONFIG_DIFF_RVH
  
  #ifdef CONFIG_DIFF_RVV
    #define VLEN 1024
    #define VENUM64 (VLEN/64)
    #define VENUM32 (VLEN/32)
    #define VENUM16 (VLEN/16)
    #define VENUM8  (VLEN/8)
  
    union {
      uint64_t _64[VENUM64];
      uint32_t _32[VENUM32];
      uint16_t _16[VENUM16];
      uint8_t  _8[VENUM8];
    } vr[32];
  
    uint64_t vstart;
    uint64_t vxsat;
    uint64_t vxrm;
    uint64_t vcsr;
    uint64_t vl;
    uint64_t vtype;
    uint64_t vlenb;
  #endif // CONFIG_DIFF_RVV
  
  #ifdef CONFIG_DIFF_FPU
    uint64_t fcsr;
  #endif // CONFIG_DIFF_FPU
  
  #ifdef CONFIG_DIFF_SDTRIG
    uint64_t tselect;
    uint64_t tdata1;
    uint64_t tinfo;
  #endif // CONFIG_DIFF_SDTRIG
  
  #ifdef CONFIG_DIFF_DEBUG_MODE
    uint64_t debugMode;
    uint64_t dcsr;
    uint64_t dpc;
    uint64_t dscratch0;
    uint64_t dscratch1;
  #endif // CONFIG_DIFF_DEBUG_MODE
  } diff_context_t;

  void (*ref_difftest_memcpy)(uint64_t addr, void *buf, size_t n, bool direction) = NULL;
  void (*ref_difftest_regcpy)(void *dut, bool direction, bool on_demand) = NULL;
  void (*ref_difftest_exec)(uint64_t n) = NULL;
  void (*ref_difftest_raise_intr)(uint64_t NO) = NULL;
  void (*ref_difftest_init)(int port) =NULL ;
  void (*ref_isa_reg_display)()=NULL;

  enum { DIFFTEST_TO_DUT, DIFFTEST_TO_REF };

  #endif