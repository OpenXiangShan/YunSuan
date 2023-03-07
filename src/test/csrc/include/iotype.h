#ifndef __IO_TYPE_H
#define __IO_TYPE_H

struct VecInput {
  uint64_t src1[2];
  uint64_t src2[2];
  uint64_t src3[2];
  uint8_t fuType; // only 5bits(or 2bits?)
  uint8_t fuOpType;
  uint8_t sew; // only 2 bits
  bool src_widen;
  bool widen;
  uint8_t rm; // only 3 bits
};

struct VecOutput {
  uint64_t result[2];
  uint32_t fflags[2]; // only 20bits for each op
};

struct VecInputE8 {
  uint8_t src1[16];
  uint8_t src2[16];
  uint8_t src3[16];
  uint8_t fuType; // only 5bits(or 2bits?)
  uint8_t fuOpType;
  uint8_t sew; // only 2 bits
  bool src_widen;
  bool widen;
  uint8_t rm; // only 3 bits
};

struct VecInputE16 {
  uint16_t src1[8];
  uint16_t src2[8];
  uint16_t src3[8];
  uint8_t fuType; // only 5bits(or 2bits?)
  uint8_t fuOpType;
  uint8_t sew; // only 2 bits
  bool src_widen;
  bool widen;
  uint8_t rm; // only 3 bits
};

struct VecInputE32 {
  uint32_t src1[4];
  uint32_t src2[4];
  uint32_t src3[4];
  uint8_t fuType; // only 5bits(or 2bits?)
  uint8_t fuOpType;
  uint8_t sew; // only 2 bits
  bool src_widen;
  bool widen;
  uint8_t rm; // only 3 bits
};

struct VecOutputE8 {
  uint8_t result[16];
  // uint8_t fflags[2]; // only 20bits for each op
};

struct VecOutputE16 {
  uint16_t result[8];
  uint8_t fflags[8]; // only 20bits for each op
};

struct VecOutputE32 {
  uint32_t result[4];
  uint8_t fflags[4]; // only 20bits for each op
};



struct ElementOutput {
  uint64_t result;
  uint8_t fflags;
};

struct ElementInput {
  uint64_t src1;
  uint64_t src2;
  uint64_t src3;
  uint8_t fuOpType;
  bool src_widen;
  bool widen;
  uint8_t rm;
};

#endif