#include <stdio.h>
#include <mem.h>
#include <stdint.h>
#include <cstring>
#include <reg.h>

// float *pmem=NULL;
// void guest_to_host(uint64_t paddr){ return pmem+paddr-CONFTG_PMEM};
extern float diff_vreg[32][VLEN / 32];

FloatUintUnion pmem[CONFIG_MSIZE] = {};
extern VCSR vcsr;

void out_of_bound(uint64_t paddr)
{
  if ((paddr - CONFIG_PMEM + vcsr.vl) < CONFIG_MSIZE)
    return;
  printf("Memory is out of bound!\n");
  assert(0);
}

extern "C" void pmem_read(unsigned long long paddr, uint32_t output_bits[VLEN / 32])
{ 
  out_of_bound(paddr);//memory bound check
  int start_addr = paddr - CONFIG_PMEM;
  for (int i = start_addr; i < vcsr.vl; i++)
  {
    std::memcpy(&output_bits[i], &pmem[i].as_float, sizeof(float));
  }
}

// extern "C" void get_vreg(const svLogicVecVal *data)
// {
//   for (int vreg_idx = 0; vreg_idx < 1; vreg_idx++)
//   {
//     const svLogicVecVal *vreg_start = data + (vreg_idx * 32);

//     for (int chunk = 0; chunk < VLEN / 32; chunk++)
//     {
//       // 获取32位数据块
//       uint32_t sv_bits = vreg_start[chunk].aval;

//       // 字节序转换
//       union
//       {
//         uint32_t u32;
//         uint8_t bytes[4];
//       } converter;

//       converter.bytes[0] = sv_bits & 0xFF;
//       converter.bytes[1] = (sv_bits >> 8) & 0xFF;
//       converter.bytes[2] = (sv_bits >> 16) & 0xFF;
//       converter.bytes[3] = (sv_bits >> 24) & 0xFF;

//       // 使用memcpy保证严格别名规则
//       std::memcpy(&diff_vreg[0][chunk], &converter.u32, sizeof(float));
//     }
//   }
// }

extern "C" void get_vreg(const svLogicVecVal *data)
{
  for (int vreg_idx = 0; vreg_idx < 1; vreg_idx++)
  {
    const svLogicVecVal *vreg_start = data + (vreg_idx * 32);

    for (int chunk = 0; chunk < VLEN / 32; chunk++)
    {
      // 获取32位数据块
      uint32_t sv_bits = vreg_start[chunk].aval;

      // 字节序转换
      union
      {
        uint32_t u32;
        uint8_t bytes[4];
      } converter;

      converter.bytes[0] = sv_bits & 0xFF;
      converter.bytes[1] = (sv_bits >> 8) & 0xFF;
      converter.bytes[2] = (sv_bits >> 16) & 0xFF;
      converter.bytes[3] = (sv_bits >> 24) & 0xFF;

      // 使用memcpy保证严格别名规则
      std::memcpy(&diff_vreg[0][chunk], &converter.u32, sizeof(float));
    }
  }
}