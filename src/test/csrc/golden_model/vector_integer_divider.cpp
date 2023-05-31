#include "../include/gm_common.h"

ElementOutput VGMIntegerDividier::calculation_e8(ElementInput input) {
    ElementOutput output;
    switch(input.fuOpType) {
        case VIDIV:
            if(((int8_t)input.src2) != 0){
                output.result =(int8_t) ((int8_t)input.src1 / (int8_t)input.src2);
                output.fflags = 0;
            } else {
                output.result = 0xFF;
                output.fflags = 1;
            }; break;            
        case VIDIVU:
            if(((uint8_t)input.src2) != 0){
                output.result = (uint8_t) ((uint8_t)input.src1 / (uint8_t)input.src2);
                output.fflags = 0;
            } else {
                output.result = 0xFF;
                output.fflags = 1;
            }; break;   
        case VIREM:
            if(((int8_t)input.src2) != 0){
                output.result = (int8_t)((int8_t)input.src1 % (int8_t)input.src2);
                output.fflags = 0;
            } else {
                output.result = (int8_t)input.src1;
                output.fflags = 1;
            }; break; 

        case VIREMU:
            if(((uint8_t)input.src2) != 0){
                output.result = (uint8_t) ((uint8_t)input.src1 % (uint8_t)input.src2);
                output.fflags = 0;
            } else {
                output.result = (uint8_t)input.src1;
                output.fflags = 1;
            }; break; 

    }
    if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
    return output;
}
ElementOutput VGMIntegerDividier::calculation_e16(ElementInput input) {
    ElementOutput output;
    switch(input.fuOpType) {
        case VIDIV:
            if(((int16_t)input.src2) != 0){
                output.result = (int16_t) ((int16_t)input.src1 / (int16_t)input.src2);
                output.fflags = 0;
            } else {
                output.result = 0xFFFF;
                output.fflags = 1;
            }; break;            
        case VIDIVU:
            if(((uint16_t)input.src2) != 0){
                output.result = (uint16_t)((uint16_t)input.src1 / (uint16_t)input.src2);
                output.fflags = 0;
            } else {
                output.result = 0xFFFF;
                output.fflags = 1;
            }; break;   
        case VIREM:
            if(((int16_t)input.src2) != 0){
                output.result = (int16_t)((int16_t)input.src1 % (int16_t)input.src2);
                output.fflags = 0;
            } else {
                output.result = (int16_t)input.src1;
                output.fflags = 1;
            }; break; 

        case VIREMU:
            if(((uint16_t)input.src2) != 0){
                output.result = (uint16_t)((uint16_t)input.src1 % (uint16_t)input.src2);
                output.fflags = 0;
            } else {
                output.result = (uint16_t)input.src1;
                output.fflags = 1;
            }; break; 

    }
    if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
    return output;  
}
ElementOutput VGMIntegerDividier::calculation_e32(ElementInput input) {
    ElementOutput output;
    switch(input.fuOpType) {
        case VIDIV:
            if(((int32_t)input.src2) != 0){
                output.result = (int32_t) ((int32_t)input.src1 / (int32_t)input.src2);
                output.fflags = 0;
            } else {
                output.result = 0xFFFFFFFF;
                output.fflags = 1;
            }; break;            
        case VIDIVU:
            if(((uint32_t)input.src2) != 0){
                output.result = (uint32_t) ((uint32_t)input.src1 / (uint32_t)input.src2);
                output.fflags = 0;
            } else {
                output.result = 0xFFFFFFFF;
                output.fflags = 1;
            }; break;   
        case VIREM:
            if(((int32_t)input.src2) != 0){
                output.result = (int32_t)((int32_t)input.src1 % (int32_t)input.src2);
                output.fflags = 0;
            } else {
                output.result = (int32_t)input.src1;
                output.fflags = 1;
            }; break; 

        case VIREMU:
            if(((uint32_t)input.src2) != 0){
                output.result = (uint32_t)((uint32_t)input.src1 % (uint32_t)input.src2);
                output.fflags = 0;
            } else {
                output.result = (uint32_t)input.src1;
                output.fflags = 1;
            }; break; 

    }
    if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
    return output;  
}
ElementOutput VGMIntegerDividier::calculation_e64(ElementInput input) {
    ElementOutput output;

    switch(input.fuOpType) {
        case VIDIV:
            if(((int64_t)input.src2) != 0){
                output.result = (int64_t)((int64_t)input.src1 / (int64_t)input.src2);
                output.fflags = 0;
            } else {
                output.result = 0xFFFFFFFFFFFFFFFF;
                output.fflags = 1;
            }; break;            
        case VIDIVU:
            if(((uint64_t)input.src2) != 0){
                output.result = (uint64_t)((uint64_t)input.src1 / (uint64_t)input.src2);
                output.fflags = 0;
            } else {
                output.result = 0xFFFFFFFFFFFFFFFF;
                output.fflags = 1;
            }; break;   
        case VIREM:
            if(((int64_t)input.src2) != 0){
                output.result = (int64_t)((int64_t)input.src1 % (int64_t)input.src2);
                output.fflags = 0;
            } else {
                output.result = (int64_t)input.src1;
                output.fflags = 1;
            }; break; 

        case VIREMU:
            if(((uint64_t)input.src2) != 0){
                output.result = (uint64_t)((uint64_t)input.src1 % (uint64_t)input.src2);
                output.fflags = 0;
            } else {
                output.result = (uint64_t)input.src1;
                output.fflags = 1;
            }; break; 

    }
    if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
    return output;  
}