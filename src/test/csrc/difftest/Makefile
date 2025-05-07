# 编译器
CXX = g++
VERILATOR=verilator
#INCLUDE
INCLUDE_DIR = $(abspath ./include)
# 编译选项
CXXFLAGS = -g -O2 -std=c++14 #-fext-numeric-literals 
LDFLAGS = -ldl -lSDL2 -lpthread  # 根据实际需求调整其他库


VERILATOR_CFLAGS += -MMD -MP --build -cc \
										-O2  --noassert --trace -j 4

#VERILATOR FLAGS
TOPNAME ?= VTopDebug

WORK_DIR=$(shell pwd)
V_FILES=$(abspath ../../../../build/verilog_vpu_debug/load_l2_dpic.sv)  \
		$(abspath ../../../../build/verilog_vpu_debug/VTopDebug.v) \
		$(abspath ../../../../build/verilog_vpu_debug/get_vreg_dpic.sv)

BUILD_DIR = $(abspath $(WORK_DIR)/build/)
VTOP = $(BUILD_DIR)/$(TOPNAME)
OBJ_DIR = $(BUILD_DIR)/obj_dir
TARGET = emulator
$(shell mkdir -p $(BUILD_DIR))

SIM_CSRCS = $(shell find $(abspath ./)   -name "*.cpp") 
HEADER_FILES = $(shell find $(abspath ./) -name '*.h') 

DEPS=$(V_FILES) $(SIM_CSRCS)


# Include path

INCFLAGS = $(addprefix -I, $(INCLUDE_DIR))
CXXFLAGS += $(INCFLAGS) -DTOP_NAME="\"V$(TOPNAME)\"" 

$(VTOP): $(V_FILES)
	@$(VERILATOR)  $(VERILATOR_CFLAGS)\
		--top-module $(TOPNAME) $^ \
		$(addprefix -CFLAGS , $(CXXFLAGS)) $(addprefix -LDFLAGS , $(LDFLAGS)) \
		--Mdir $(OBJ_DIR) --exe

$(TARGET):$(SIM_CSRCS) $(HEADER_FILES) $(VTOP)
	$(CXX) $(CXXFLAGS) \
	-I$(OBJ_DIR)  $(SIM_CSRCS) $(OBJ_DIR)/V$(TOPNAME)__ALL.a  -o $(TARGET)  -I$(INCLUDE_DIR) $(LDFLAGS)



# 清理生成的文件
clean:
	rm -f $(OBJS) $(TARGET)
	rm -rf $(BUILD_DIR)
	rm -rf $(abspath ./vec_ops.o) $(abspath ./vec_ops.bin)

sim: $(TARGET)
	$(TARGET)

gdb: $(BIN)
	gdb --args $(BIN)

vec_ops.o:vec_ops.s
	riscv64-unknown-elf-as -march=rv64gcv vec_ops.s -o vec_ops.o
assembly: vec_ops.o
	riscv64-unknown-elf-objdump -d vec_ops.o  > $(abspath ./vec_ops.txt)

# 伪目标
.PHONY: all clean sim assembly