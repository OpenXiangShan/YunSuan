# Makefile for RISC-V In a Nutshell course (RVVIAN) examples

# RISC-V C Compiler
# available options (the version used must support RVV intrinsics)
# clang/llvm
# RISCVCLANG=clang  --target=riscv64
# RISCVCLANG=../../../../../../difftest/xs-env/opt/riscv/bin/riscv64-unknown-elf-gcc  -march=rv64gv -mabi=lp64
# RISCVCC=clang  --target=riscv64
# GNU Compiler Collection (GCC)
RISCVCC=../../../../../../difftest/xs-env/opt/riscv-zfh/bin/riscv64-unknown-elf-gcc -march=rv64g_zvfh -mabi=lp64

CFLAGS+=-mcmodel=medany  
CFLAGS+=-nostdlib 

# architectural parameters for the simulation
# width of vector registers (VLEN)

# path to proxy-kernel (pk)
PK_PATH=pk
# SIMULATOR
# Available options in the Docker (uncomment one)
SIMULATOR=../../../../../../difftest/xs-env/opt/riscv/bin/spike -d --log-commits --isa=rv64gcv_zvfh_zicntr_zihpm_zvl1024b_zve64d
# SIMULATOR=qemu-riscv64 -cpu rv64,v=on,vext_spec=v1.0,vlen=128,rvv_ta_all_1s=on
INCLUDE_DIR ?= ../../../../../../difftest/xs-env/opt/riscv-zfh/lib/gcc/riscv64-unknown-elf/14.2.0/include/

WORK_DIR := $(shell pwd)
BUILD_DIR := $(WORK_DIR)/build
$(shell mkdir -p $(BUILD_DIR))
FILES ?= $(wildcard *.c)
SRCS := $(FILES)  $(wildcard *.S)
OBJS := $(addprefix $(BUILD_DIR)/, $(addsuffix .o, $(basename $(notdir $(SRCS)))))

$(BUILD_DIR)/%.o: %.c
	$(RISCVCC) $(CFLAGS)  -I./ -I$(INCLUDE_DIR)  -O2  -c -o $@ $<

$(BUILD_DIR)/%.o: %.S
	$(RISCVCC) $(CFLAGS)  -I./ -I$(INCLUDE_DIR)  -O2  -c -o $@ $<

$(BUILD_DIR)/softmax.elf: $(OBJS)
	$(RISCVCC) $(CFLAGS) -T default.ld -I./ -I$(INCLUDE_DIR)  -O2  $^ -o $@

$(BUILD_DIR)/softmax.bin: $(BUILD_DIR)/softmax.elf
	riscv64-unknown-elf-objcopy -O binary $< $@

$(BUILD_DIR)/softmax.txt: $(BUILD_DIR)/softmax.elf
	riscv64-unknown-elf-objdump -d $< > $@

run : $(BUILD_DIR)/softmax.elf $(BUILD_DIR)/softmax.txt $(BUILD_DIR)/softmax.bin
	$(SIMULATOR) $(BUILD_DIR)/softmax.elf

clean:
	rm -rf *.o *.txt *.elf *.bin ./build

.PHONY: sim_bench_softmax clean run
