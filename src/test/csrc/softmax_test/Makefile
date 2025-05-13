# 编译器
CXX = g++
#INCLUDE
INCLUDE_DIR = $(abspath ./include)
# 编译选项
CXXFLAGS = -g -O0 -std=c++14 #-fext-numeric-literals 
# 目标可执行文件
TARGET = golden_model
# 源文件
SRCS = golden_model.cpp softmax_rvv.cpp softmax_baseline.cpp rvv.cpp
# 目标文件
OBJS = $(SRCS:.cpp=.o)


#VERILATOR FLAGS
TOPNAME ?= VTopDebug

WORK_DIR=$(shell pwd)
V_FILES=$(abspath ../../../../build/verilog_vpu_debug/load_l2_dpic.sv)  \
		$(abspath ../../../../build/verilog_vpu_debug/VTopDebug.v) \
		$(abspath ../../../../build/verilog_vpu_debug/get_vreg_dpic.sv)

BUILD_DIR = $(abspath $(WORK_DIR)/build/load_test)
BIN = $(BUILD_DIR)/$(TOPNAME)
OBJ_DIR = $(BUILD_DIR)/obj_dir
HEADER_FILES = $(shell find $(INCLUDE_DIR) -name '*.h') 
$(shell mkdir -p $(BUILD_DIR))

SIM_CSRCS = $(shell find -L $(abspath ./)  -name "*.cc" -or -name "*.cpp") 


DEPS=$(V_FILES) $(SIM_CSRCS)

VERILATOR_CFLAGS += -MMD -MP --build -cc \
										-O3 --x-assign fast --x-initial fast --noassert --trace
VERILATOR_CFLAGS += -j 33

# Include path

LMUL ?= 1
INCFLAGS = $(addprefix -I, $(INCLUDE_DIR))
CXXFLAGS += $(INCFLAGS) -DTOP_NAME="\"V$(TOPNAME)\"" 


$(BIN): $(DEPS)
	@verilator $(VERILATOR_CFLAGS) \
		--top-module $(TOPNAME) $^ \
		$(addprefix -CFLAGS , $(CXXFLAGS)) $(addprefix -LDFLAGS , $(LDFLAGS)) \
		--Mdir $(OBJ_DIR) --exe -o $(abspath $(BIN))

# 默认目标
all: $(TARGET)

# 生成可执行文件
$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(OBJS)

# 生成目标文件
%.o: %.cpp softmax_bench.h reg.h rvv.h
	$(CXX) $(CXXFLAGS) -c $< -o $@

# 清理生成的文件
clean:
	rm -f $(OBJS) $(TARGET)
	rm -rf $(BUILD_DIR)
	rm -rf $(abspath ./vec_ops.o) $(abspath ./vec_ops.bin)

sim: $(BIN)
	@$(BIN)

gdb: $(BIN)
	gdb --args $(BIN)

vec_ops.o:vec_ops.s
	riscv64-unknown-elf-as -march=rv64gcv vec_ops.s -o vec_ops.o
assembly: vec_ops.o
	riscv64-unknown-elf-objdump -d vec_ops.o  > $(abspath ./vec_ops.txt)

# 伪目标
.PHONY: all clean sim assembly