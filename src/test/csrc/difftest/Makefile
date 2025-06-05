# 编译器参数定义
CXX 		:= g++
VERILATOR 	:=verilator
TOPNAME 	:= VTopDebug

WORK_DIR	:=$(shell pwd)
BUILD_DIR 	:= $(abspath $(WORK_DIR)/build/)
OBJ_DIR 	:= $(BUILD_DIR)/obj_dir
TARGET 		:= $(BUILD_DIR)/obj_dir/emulator
VTOP 		:= $(BUILD_DIR)/$(TOPNAME)

# 源文件路径
SIM_CSRCS 		= $(shell find $(abspath ./) -maxdepth 1   -name "*.cpp") 
HEADER_FILES 	= $(shell find $(abspath ./) -maxdepth 1 -name '*.h') 
V_FILES			=$(abspath ../../../../build/verilog_vpu_debug/load_l2_dpic.sv)  \
                 $(abspath ../../../../build/verilog_vpu_debug/store_l2_dpic.sv)  \
				$(abspath ../../../../build/verilog_vpu_debug/VTopDebug.v) \
				$(abspath ../../../../build/verilog_vpu_debug/get_vreg_dpic.sv)
DEPS			=$(V_FILES) $(SIM_CSRCS)

#结果输出控制
RESULT := result.txt
COLOR_RED    := \033[0;31m
COLOR_GREEN  := \033[0;32m
COLOR_NONE   := \033[0m

# 编译选项
VERILATOR_ROOT := $(shell verilator --getenv VERILATOR_ROOT)
CXXFLAGS = -g -O2 -std=c++17 -I$(OBJ_DIR)
CXXFLAGS += -I$(VERILATOR_ROOT)/include -I$(VERILATOR_ROOT)/include/vltstd
LDFLAGS = -ldl -lSDL2 -lpthread -no-pie -latomic -lz# 根据实际需求调整其他库
INCLUDE_DIR = $(abspath ./include)
include $(VERILATOR_ROOT)/include/verilated.mk

#VERILATOR FLAGS
VERILATOR_CFLAGS += -MMD -MP --build -cc \
					-threads 4 -O2  --noassert --trace-fst -j 30
# Include path
INCFLAGS = $(addprefix -I, $(INCLUDE_DIR))
CXXFLAGS += $(INCFLAGS) -DTOP_NAME="\"V$(TOPNAME)\"" 


$(OBJ_DIR)/V$(TOPNAME)__ALL.a: $(V_FILES)
	@mkdir -p $(OBJ_DIR)
	@$(VERILATOR) $(VERILATOR_CFLAGS) \
		--top-module $(TOPNAME) \
		--Mdir $(OBJ_DIR) \
		$^
#调用 obj_dir 下的 Makefile 编译出 .o 和 .a
	$(MAKE) -C $(OBJ_DIR) -f V$(TOPNAME).mk

$(TARGET):$(SIM_CSRCS) $(HEADER_FILES) $(OBJ_DIR)/V$(TOPNAME)__ALL.a
	@$(CXX) $(CXXFLAGS) \
	-I$(OBJ_DIR)  $(SIM_CSRCS) \
	$(VERILATOR_ROOT)/include/verilated.cpp \
	$(VERILATOR_ROOT)/include/verilated_vcd_c.cpp \
	$(VERILATOR_ROOT)/include/verilated_threads.cpp \
	$(VERILATOR_ROOT)/include/verilated_fst_c.cpp \
	$(OBJ_DIR)/V$(TOPNAME).cpp \
	$(OBJ_DIR)/V$(TOPNAME)__ALL.a  -I$(INCLUDE_DIR) $(LDFLAGS) -o $@  

#测试程序路径设置
BAREMETAL_DIR := $(abspath ./baremetal_kernel)
BIN_DIR := $(BAREMETAL_DIR)/build
CASES := $(notdir $(basename $(wildcard $(BAREMETAL_DIR)/softmax*.c)))
PROG ?= $(firstword $(notdir $(basename $(wildcard $(BAREMETAL_DIR)/softmax*.c))))

$(BIN_DIR)/%.bin:
	$(MAKE) -C $(BAREMETAL_DIR) FILES=$*.c  $@

# 清理生成的文件
clean:
	rm -f  $(TARGET)
	rm -rf $(BUILD_DIR)/*.fst  $(BUILD_DIR)/*.log


sim: $(TARGET) $(addprefix $(BIN_DIR)/,$(addsuffix .bin,$(PROG)))
	@ if $(TARGET) -w $(BUILD_DIR)/top.fst -l $(BUILD_DIR)/top.log $(addprefix $(BIN_DIR)/,$(addsuffix .bin,$(PROG))) ; then \
		printf "[%s] $(COLOR_GREEN)PASS$(COLOR_NONE)\n" $(PROG) | tee -a $(RESULT); \
	else \
			printf "[%s] $(COLOR_RED)FAIL$(COLOR_NONE)\n" $(PROG) | tee -a $(RESULT); \
	fi 

sim-all: $(TARGET) $(addprefix $(BIN_DIR)/,$(addsuffix .bin,$(CASES)))
	@echo "==== Softmax Test STARTS ====" > $(RESULT)
	@for bin in $(addprefix $(BIN_DIR)/,$(addsuffix .bin,$(CASES))); do \
		name=$$(basename $$bin .bin); \
		echo "==> Program: $$bin"; \
		if $(TARGET) -w $(BUILD_DIR)/$$name.fst -l $(BUILD_DIR)/$$name.log $$bin ; then \
			printf "[%s] $(COLOR_GREEN)PASS$(COLOR_NONE)\n" $$name | tee -a $(RESULT); \
		else \
			printf "[%s] $(COLOR_RED)FAIL$(COLOR_NONE)\n" $$name | tee -a $(RESULT); \
		fi \
	done
	@echo "==== Softmax Test ENDS ====" >> $(RESULT)
	@rm $(RESULT)

gdb: $(TARGET)
	gdb --args $(TARGET)

vec_ops.o:vec_ops.s
	riscv64-unknown-elf-as -march=rv64gcv vec_ops.s -o vec_ops.o
assembly: vec_ops.o
	riscv64-unknown-elf-objdump -d vec_ops.o  > $(abspath ./vec_ops.txt)

# 伪目标
.PHONY: all clean sim assembly