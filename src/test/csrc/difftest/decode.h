#ifndef _DECODE_H_
#define _DECODE_H_
#include "common.h"
#include "emu.h"
#include "difftest.h"

enum {
	TYPE_I, TYPE_U, TYPE_S, TYPE_J, TYPE_N, TYPE_R,TYPE_B, TYPE_OPIVV, TYPE_OPFVV, TYPE_OPMVV, TYPE_OPIVI, TYPE_OPIVX, TYPE_OPFVF, TYPE_OPMVX, TYPE_OPCFG,TYPE_VL, TYPE_VLS,TYPE_VLX,TYPE_VS 
    ,TYPE_VSS,TYPE_VSX,TYPE_VFMV_F_S
};
enum {
	SD
};
#define GPR(idx) ref_cpu_state.gpr[idx]
#define FPR(idx) ref_cpu_state.fpr[idx]

static inline void pattern_decode(const char *str, int len,
                                  uint64_t *key, uint64_t *mask, uint64_t *shift)
{
    uint64_t __key = 0, __mask = 0, __shift = 0;
#define macro(i)                                                       \
    if ((i) >= len)                                                    \
        goto finish;                                                   \
    else                                                               \
    {                                                                  \
        char c = str[i];                                               \
        if (c != ' ')                                                  \
        {                                                              \
            if (c == '0' || c == '1' || c == '?')                      \
            {                                                          \
                __key = (__key << 1) | (c == '1' ? 1 : 0);             \
                __mask = (__mask << 1) | (c == '?' ? 0 : 1);           \
                __shift = (c == '?' ? __shift + 1 : 0);                \
            }                                                          \
            else                                                       \
            {                                                          \
                printf("invalid character '%c' in pattern string", c); \
                assert(0);                                             \
            }                                                          \
        }                                                              \
    }

#define macro2(i)  macro(i);   macro((i) + 1)
#define macro4(i)  macro2(i);  macro2((i) + 2)
#define macro8(i)  macro4(i);  macro4((i) + 4)
#define macro16(i) macro8(i);  macro8((i) + 8)
#define macro32(i) macro16(i); macro16((i) + 16)
#define macro64(i) macro32(i); macro32((i) + 32)
  macro64(0);
  printf("pattern too long");
#undef macro
finish:
  *key = __key >> __shift;
  *mask = __mask >> __shift;
  *shift = __shift;
}

#define INSTPAT(pattern,...) do { \
    uint64_t key, mask, shift; \
    pattern_decode(pattern, sizeof(pattern)-1, &key, &mask, &shift); \
    if ((((uint64_t)INSTPAT_INST(s) >> shift) & mask) == key) { \
      INSTPAT_MATCH(s, ##__VA_ARGS__); \
      goto *(__instpat_end); \
    } \
  } while (0)


  
#define INSTPAT_START(name) \
    {                       \
        const void **__instpat_end = (const void**)&&concat(__instpat_end_, name);
#define INSTPAT_END(name)           \
    concat(__instpat_end_, name) :; \
    }



    #define gpr_src1() do { s->rs1 = GPR(rs1_addr); } while (0)
    #define gpr_src2() do { s->rs2 = GPR(rs2_addr); } while (0)
    #define fpr_src1() do { s->rs1 = FPR(rs1_addr); } while (0)
    #define fpr_src2() do { s->rs2 = FPR(rs2_addr); } while (0)
    #define isvec() do { s->is_vec = true; } while (0)
    #define isveccfg() do { s->is_vec_cfg = true; } while (0)
    #define isvecstore() do { s->is_vec_store = true; } while (0)
    // #define is_scalar_store() do { s->is_scalar_store = true; } while (0)



    #define immI() do { s->imm = SEXT(BITS(i, 31, 20), 12); } while(0)
    #define immU() do { s->imm = SEXT(BITS(i, 31, 12), 20) << 12; } while(0)
    #define immS() do { s->imm = (SEXT(BITS(i, 31, 25), 7) << 5) | BITS(i, 11, 7); } while(0)
    #define immJ() do { s->imm = SEXT(BITS(i,31, 31), 1) << 20 | BITS(i, 19, 12)<< 12|BITS(i, 20, 20) << 11 | BITS(i,30,21) << 1 ; } while(0)
    #define immB() do { s->imm =SEXT(BITS(i, 31, 31), 1) << 12 | BITS(i,7,7) << 11 | BITS(i, 30, 25) << 5 | BITS(i, 11, 8) << 1;} while(0)

    // #define immOPIVI() do { s->rs1 =SEXT(BITS(i, 19, 15), 5); } while(0)
    
    
    static void decode_operand(Decode* s, int type) {
        uint32_t i = s->inst.val;
        int rs1_addr = BITS(i, 19, 15);
        int rs2_addr = BITS(i, 24, 20);
        // *rd = BITS(i, 11, 7);
        switch (type) {
        case TYPE_I: gpr_src1();             immI(); break;
        case TYPE_U:                         immU(); break;
        case TYPE_S:  gpr_src1(); gpr_src2(); immS(); break;
        case TYPE_J:									 immJ(); break;
        case TYPE_R: gpr_src1(); gpr_src2();         break;
        case TYPE_B: gpr_src1(); gpr_src2(); immB(); break;
        case TYPE_OPIVV:isvec();                     break;
        case TYPE_OPFVV:isvec();                     break;
        case TYPE_OPMVV:isvec();                     break;
        case TYPE_OPIVI:isvec();                     break;
        case TYPE_OPIVX:isvec();gpr_src1();          break;
        case TYPE_OPFVF:isvec();fpr_src1();          break;  
        case TYPE_OPMVX:isvec();gpr_src1();          break;
        case TYPE_OPCFG:        isveccfg();          break;
        case TYPE_VL :isvec();gpr_src1();            break;
        case TYPE_VLS:isvec();gpr_src1();gpr_src2(); break;
        case TYPE_VLX:isvec();gpr_src1();            break;
        case TYPE_VS :isvecstore();gpr_src1();            break;
        case TYPE_VSS:isvecstore();gpr_src1();gpr_src2(); break;
        case TYPE_VSX:isvecstore();gpr_src1();            break;
        case TYPE_VFMV_F_S:                          break;
        default:printf("Unsupported type!\n");assert(0);break;

        }
    }
#endif