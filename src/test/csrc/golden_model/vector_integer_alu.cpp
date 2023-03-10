#include "../include/gm_common.h"

// bool VGMIntegerALU::bad_fuType(VecInput input) {
//   return false;
// }

// bool VGMIntegerALU::bad_fuOpType(VecInput input) {
//   return false;
// }


ElementOutput VGMIntegerALU::calculation_e8(ElementInput input) {
  ElementOutput output;
  switch(input.fuOpType) {
    case VADD:
      output.result = (uint8_t)((uint8_t)input.src1 + (uint8_t)input.src2); break;
    case VADC:
      output.result = (uint8_t)((uint8_t)input.src1 + (uint8_t)input.src2 + (uint8_t)input.src4); break;
    case VSUB:
      output.result = (uint8_t)((uint8_t)input.src2 - (uint8_t)input.src1); break;
    case VSBC:
      output.result = (uint8_t)((uint8_t)input.src2 - (uint8_t)input.src1 - (uint8_t)input.src4); break;
    case VMAXU:
      if((uint8_t)input.src1 >= (uint8_t)input.src2) {
        output.result = (uint8_t)input.src1;
      } else {
        output.result = (uint8_t)input.src2;
      }
      break;
    case VMINU:
      if((uint8_t)input.src1 <= (uint8_t)input.src2) {
        output.result = (uint8_t)input.src1;
      } else {
        output.result = (uint8_t)input.src2;
      }
      break;
    case VMAX:
      if((int8_t)input.src1 >= (int8_t)input.src2) {
        output.result = (uint8_t)input.src1;
      } else {
        output.result = (uint8_t)input.src2;
      }
      break;
    case VMIN:
      if((int8_t)input.src1 <= (int8_t)input.src2) {
        output.result = (uint8_t)input.src1;
      } else {
        output.result = (uint8_t)input.src2;
      }
      break;
    case VAND:
      output.result = (uint8_t)((uint8_t)input.src1 & (uint8_t)input.src2); break;
    case VNAND:
      output.result = (uint8_t)(~((uint8_t)input.src1 & (uint8_t)input.src2)); break;
    case VOR:
      output.result = (uint8_t)((uint8_t)input.src1 | (uint8_t)input.src2); break;
    case VNOR:
      output.result = (uint8_t)(~((uint8_t)input.src1 | (uint8_t)input.src2)); break;
    case VXOR:
      output.result = (uint8_t)((uint8_t)input.src1 ^ (uint8_t)input.src2); break;
    case VXNOR:
      output.result = (uint8_t)(~((uint8_t)input.src1 ^ (uint8_t)input.src2)); break;
    case VSLL:
      output.result = (uint8_t)((uint8_t)input.src2 << ((uint8_t)input.src1 & 0x7)); break;
    case VSRL:
      output.result = (uint8_t)((uint8_t)input.src2 >> ((uint8_t)input.src1 & 0x7)); break;
    case VSRA:
      output.result = (uint8_t)((int8_t)input.src2 >> ((uint8_t)input.src1 & 0x7)); break;
    case VSSRL: {
      __uint128_t val_u = input.src2;

      INT_ROUNDING(val_u, input.rm_s, input.src1 & 0x7);
      output.result = (uint8_t)(val_u >> (input.src1 & 0x7));
      break;
    }
    case VSSRA: {
      __int128_t val = input.src2;

      INT_ROUNDING(val, input.rm_s, input.src1 & 0x7);
      output.result = (uint8_t)(val >> (input.src1 & 0x7));
      break;
    }
    case VRSUB:
      output.result = (uint8_t)((uint8_t)input.src1 - (uint8_t)input.src2); break;
  }
  output.fflags = 0;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMIntegerALU::calculation_e16(ElementInput input) {
  ElementOutput output;
  switch(input.fuOpType) {
    case VADD:
      output.result = (uint16_t)((uint16_t)input.src1 + (uint16_t)input.src2); break;
    case VADC:
      output.result = (uint16_t)((uint16_t)input.src1 + (uint16_t)input.src2 + (uint16_t)input.src4); break;
    case VSUB:
      output.result = (uint16_t)((uint16_t)input.src2 - (uint16_t)input.src1); break;
    case VSBC:
      output.result = (uint16_t)((uint16_t)input.src2 - (uint16_t)input.src1 - (uint16_t)input.src4); break;
    case VMAXU:
      if((uint16_t)input.src1 >= (uint16_t)input.src2) {
        output.result = (uint16_t)input.src1;
      } else {
        output.result = (uint16_t)input.src2;
      }
      break;
    case VMINU:
      if((uint16_t)input.src1 <= (uint16_t)input.src2) {
        output.result = (uint16_t)input.src1;
      } else {
        output.result = (uint16_t)input.src2;
      }
      break;
    case VMAX:
      if((int16_t)input.src1 >= (int16_t)input.src2) {
        output.result = (uint16_t)input.src1;
      } else {
        output.result = (uint16_t)input.src2;
      }
      break;
    case VMIN:
      if((int16_t)input.src1 <= (int16_t)input.src2) {
        output.result = (uint16_t)input.src1;
      } else {
        output.result = (uint16_t)input.src2;
      }
      break;
    case VAND:
      output.result = (uint16_t)((uint16_t)input.src1 & (uint16_t)input.src2); break;
    case VNAND:
      output.result = (uint16_t)(~((uint16_t)input.src1 & (uint16_t)input.src2)); break;
    case VOR:
      output.result = (uint16_t)((uint16_t)input.src1 | (uint16_t)input.src2); break;
    case VNOR:
      output.result = (uint16_t)(~((uint16_t)input.src1 | (uint16_t)input.src2)); break;
    case VXOR:
      output.result = (uint16_t)((uint16_t)input.src1 ^ (uint16_t)input.src2); break;
    case VXNOR:
      output.result = (uint16_t)(~((uint16_t)input.src1 ^ (uint16_t)input.src2)); break;
    case VSLL:
      output.result = (uint16_t)((uint16_t)input.src2 << ((uint16_t)input.src1 & 0xf)); break;
    case VSRL:
      output.result = (uint16_t)((uint16_t)input.src2 >> ((uint16_t)input.src1 & 0xf)); break;
    case VSRA:
      output.result = (uint16_t)((int16_t)input.src2 >> ((uint16_t)input.src1 & 0xf)); break;
    case VSSRL: {
      __uint128_t val_u = input.src2;

      INT_ROUNDING(val_u, input.rm_s, input.src1 & 0xf);
      output.result = (uint16_t)(val_u >> (input.src1 & 0xf));
      break;
    }
    case VSSRA: {
      __int128_t val = input.src2;

      INT_ROUNDING(val, input.rm_s, input.src1 & 0xf);
      output.result = (uint16_t)(val >> (input.src1 & 0xf));
      break;
    }
    case VRSUB:
      output.result = (uint16_t)((uint16_t)input.src1 - (uint16_t)input.src2); break;
    
  }
  output.fflags = 0;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMIntegerALU::calculation_e32(ElementInput input) {
  ElementOutput output;
  switch(input.fuOpType) {
    case VADD:
      output.result = (uint32_t)((uint32_t)input.src1 + (uint32_t)input.src2); break;
    case VADC:
      output.result = (uint32_t)((uint32_t)input.src1 + (uint32_t)input.src2 + (uint32_t)input.src4); break;
    case VSUB:
      output.result = (uint32_t)((uint32_t)input.src2 - (uint32_t)input.src1); break;
    case VSBC:
      output.result = (uint32_t)((uint32_t)input.src2 - (uint32_t)input.src1 - (uint32_t)input.src4); break;
    case VMAXU:
      if((uint32_t)input.src1 >= (uint32_t)input.src2) {
        output.result = (uint32_t)input.src1;
      } else {
        output.result = (uint32_t)input.src2;
      }
      break;
    case VMINU:
      if((uint32_t)input.src1 <= (uint32_t)input.src2) {
        output.result = (uint32_t)input.src1;
      } else {
        output.result = (uint32_t)input.src2;
      }
      break;
    case VMAX:
      if((int32_t)input.src1 >= (int32_t)input.src2) {
        output.result = (uint32_t)input.src1;
      } else {
        output.result = (uint32_t)input.src2;
      }
      break;
    case VMIN:
      if((int32_t)input.src1 <= (int32_t)input.src2) {
        output.result = (uint32_t)input.src1;
      } else {
        output.result = (uint32_t)input.src2;
      }
      break;
    case VAND:
      output.result = (uint32_t)((uint32_t)input.src1 & (uint32_t)input.src2); break;
    case VNAND:
      output.result = (uint32_t)(~((uint32_t)input.src1 & (uint32_t)input.src2)); break;
    case VOR:
      output.result = (uint32_t)((uint32_t)input.src1 | (uint32_t)input.src2); break;
    case VNOR:
      output.result = (uint32_t)(~((uint32_t)input.src1 | (uint32_t)input.src2)); break;
    case VXOR:
      output.result = (uint32_t)((uint32_t)input.src1 ^ (uint32_t)input.src2); break;
    case VXNOR:
      output.result = (uint32_t)(~((uint32_t)input.src1 ^ (uint32_t)input.src2)); break;
    case VSLL:
      output.result = (uint32_t)((uint32_t)input.src2 << ((uint32_t)input.src1 & 0x1f)); break;
    case VSRL:
      output.result = (uint32_t)((uint32_t)input.src2 >> ((uint32_t)input.src1 & 0x1f)); break;
    case VSRA:
      output.result = (uint32_t)((int32_t)input.src2 >> ((uint32_t)input.src1 & 0x1f)); break;
    case VSSRL: {
      __uint128_t val_u = input.src2;

      INT_ROUNDING(val_u, input.rm_s, input.src1 & 0x1f);
      output.result = (uint32_t)(val_u >> (input.src1 & 0x1f));
      break;
    }
    case VSSRA: {
      __int128_t val = input.src2;

      INT_ROUNDING(val, input.rm_s, input.src1 & 0x1f);
      output.result = (uint32_t)(val >> (input.src1 & 0x1f));
      break;
    }
    case VRSUB: 
      output.result = (uint32_t)((uint32_t)input.src1 - (uint32_t)input.src2); break;
    
  }
  output.fflags = 0;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMIntegerALU::calculation_e64(ElementInput input) {
  ElementOutput output;
  switch(input.fuOpType) {
    case VADD:
      output.result = (uint64_t)((uint64_t)input.src1 + (uint64_t)input.src2); break;
    case VADC:
      output.result = (uint64_t)((uint64_t)input.src1 + (uint64_t)input.src2 + (uint64_t)input.src4); break;
    case VSUB:
      output.result = (uint64_t)((uint64_t)input.src2 - (uint64_t)input.src1); break;
    case VSBC:
      output.result = (uint64_t)((uint64_t)input.src2 - (uint64_t)input.src1 - (uint64_t)input.src4); break;
    case VMAXU:
      if((uint64_t)input.src1 >= (uint64_t)input.src2) {
        output.result = (uint64_t)input.src1;
      } else {
        output.result = (uint64_t)input.src2;
      }
      break;
    case VMINU:
      if((uint64_t)input.src1 <= (uint64_t)input.src2) {
        output.result = (uint64_t)input.src1;
      } else {
        output.result = (uint64_t)input.src2;
      }
      break;
    case VMAX:
      if((int64_t)input.src1 >= (int64_t)input.src2) {
        output.result = (uint64_t)input.src1;
      } else {
        output.result = (uint64_t)input.src2;
      }
      break;
    case VMIN:
      if((int64_t)input.src1 <= (int64_t)input.src2) {
        output.result = (uint64_t)input.src1;
      } else {
        output.result = (uint64_t)input.src2;
      }
      break;
    case VAND:
      output.result = (uint64_t)((uint64_t)input.src1 & (uint64_t)input.src2); break;
    case VNAND:
      output.result = (uint64_t)(~((uint64_t)input.src1 & (uint64_t)input.src2)); break;
    case VOR:
      output.result = (uint64_t)((uint64_t)input.src1 | (uint64_t)input.src2); break;
    case VNOR:
      output.result = (uint64_t)(~((uint64_t)input.src1 | (uint64_t)input.src2)); break;
    case VXOR:
      output.result = (uint64_t)((uint64_t)input.src1 ^ (uint64_t)input.src2); break;
    case VXNOR:
      output.result = (uint64_t)(~((uint64_t)input.src1 ^ (uint64_t)input.src2)); break;
    case VSLL:
      output.result = (uint64_t)((uint64_t)input.src2 << ((uint64_t)input.src1 & 0x3f)); break;
    case VSRL:
      output.result = (uint64_t)((uint64_t)input.src2 >> ((uint64_t)input.src1 & 0x3f)); break;
    case VSRA:
      output.result = (uint64_t)((int64_t)input.src2 >> ((uint64_t)input.src1 & 0x3f)); break;
    case VSSRL: {
      __uint128_t val_u = input.src2;

      INT_ROUNDING(val_u, input.rm_s, input.src1 & 0x3f);
      output.result = (uint64_t)(val_u >> (input.src1 & 0x3f));
      break;
    }
    case VSSRA: {
      __int128_t val = input.src2;

      INT_ROUNDING(val, input.rm_s, input.src1 & 0x3f);
      output.result = (uint64_t)(val >> (input.src1 & 0x3f));
      break;
    }
    case VRSUB:
      output.result = (uint64_t)((uint64_t)input.src1 - (uint64_t)input.src2); break;

  }
  output.fflags = 0;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}