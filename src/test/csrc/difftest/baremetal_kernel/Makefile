# Makefile for RISC-V In a Nutshell course (RVVIAN) examples

# RISC-V C Compiler
# available options (the version used must support RVV intrinsics)
# clang/llvm
# RISCVCLANG=clang  --target=riscv64
RISCVCLANG=../../../../opt/riscv/bin/riscv64-unknown-elf-gcc  -march=rv64gv -mabi=lp64
# RISCVCC=clang  --target=riscv64
# GNU Compiler Collection (GCC)
RISCVCC=../../../../opt/riscv/bin/riscv64-unknown-elf-gcc -mabi=lp64

CFLAGS+=-mcmodel=medany  
CFLAGS+=-nostdlib 

# architectural parameters for the simulation
# width of vector registers (VLEN)

# path to proxy-kernel (pk)
PK_PATH=pk
# SIMULATOR
# Available options in the Docker (uncomment one)
 SIMULATOR=../../../../opt/riscv/bin/spike -d --log-commits --isa=rv64gcv_zicntr_zihpm_zvl1024b_zve64d 
# SIMULATOR=qemu-riscv64 -cpu rv64,v=on,vext_spec=v1.0,vlen=128,rvv_ta_all_1s=on
INCLUDE_DIR ?= ../../../../opt/riscv/lib/gcc/riscv64-unknown-elf/14.2.0/include/

WORK_DIR := $(shell pwd)
BUILD_DIR := $(WORK_DIR)/build
$(shell mkdir -p $(BUILD_DIR))
SRCS := $(wildcard *.c)  $(wildcard *.S)
OBJS := $(addprefix $(BUILD_DIR)/, $(addsuffix .o, $(basename $(notdir $(SRCS)))))

$(BUILD_DIR)/%.o: %.c
	$(RISCVCLANG) $(CFLAGS)  -I./ -I$(INCLUDE_DIR)  -O2 -march=rv64gv -c -o $@ $<

$(BUILD_DIR)/%.o: %.S
	$(RISCVCLANG) $(CFLAGS)  -I./ -I$(INCLUDE_DIR)  -O2 -march=rv64gv -c -o $@ $<

$(BUILD_DIR)/softmax.elf: $(OBJS)
	$(RISCVCC) $(CFLAGS) -T default.ld -I./ -I$(INCLUDE_DIR)  -O2 -march=rv64gcv $^ -o $@

$(BUILD_DIR)/softmax.bin: $(BUILD_DIR)/softmax.elf
	riscv64-unknown-elf-objcopy -O binary $< $@

$(BUILD_DIR)/softmax.txt: $(BUILD_DIR)/softmax.elf
	riscv64-unknown-elf-objdump -d $< > $@

run : $(BUILD_DIR)/softmax.elf $(BUILD_DIR)/softmax.txt $(BUILD_DIR)/softmax.bin
	$(SIMULATOR) $(BUILD_DIR)/softmax.elf

clean:
	rm -rf *.o *.txt *.elf *.bin ./build

.PHONY: sim_bench_softmax clean run
