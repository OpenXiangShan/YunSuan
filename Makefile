include Makefile.softfloat

TOP ?= SimTop
BUILD_DIR = ./build
TOP_V = $(BUILD_DIR)/$(TOP).v

SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')

$(TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	mill YunSuan.test.runMain yunsuan.top.$(TOP) -td $(@D) --full-stacktrace --output-file $(@F)

.DEFUALT_GOAL = emu

EMU_CSRC_DIR = $(abspath ./src/test/csrc)
# EMU_VSRC_DIR = $(abspath ./src/test/vsrc)
EMU_CXXFILES += $(shell find $(EMU_CSRC_DIR) -name "*.cpp")
# EMU_VFILES = $(shell find $(EMU_VSRC_DIR) -name "*.v" -or -name "*.sv")


EMU_DEPS :=  $(EMU_CXXFILES)
# $(EMU_VFILES)
EMU_HEADERS += $(shell find $(EMU_CSRC_DIR) -name "*.h")

# Verilator trace support
EMU_TRACE ?=
ifneq (,$(filter $(EMU_TRACE),1 vcd VCD))
VEXTRA_FLAGS += --trace
endif

# Verilator multi-thread support
EMU_THREADS  ?= 0
ifneq ($(EMU_THREADS),0)
VEXTRA_FLAGS += --threads $(EMU_THREADS) --threads-dpi all
EMU_CXXFLAGS += -DEMU_THREAD=$(EMU_THREADS)
endif

EMU_CXXFLAGS += -O3 -static -Wall
EMU_LDFLAGS  += -lpthread -ldl

EMU_LDFLAGS  += $(SOFTFLOAT)

VEXTRA_FLAGS += -LDFLAGS "$(EMU_LDFLAGS)"
VEXTRA_FLAGS += -CFLAGS "-I$(EMU_CSRC_DIR) $(SOFTFLOAT_HEADER) $(EMU_CXXFLAGS)"

VERILATOR_FLAGS = --top-module $(TOP) \
	--assert \
	--output-split 500 \
	--output-split-cfuncs 500 \
	-I$(abspath $(BUILD_DIR)) \
	$(VEXTRA_FLAGS)


EMU_MK := $(BUILD_DIR)/emu-compile/V$(TOP).mk
EMU = $(BUILD_DIR)/emu

$(EMU_MK): $(TOP_V) | $(EMU_DEPS)
	@mkdir -p $(@D)
	verilator --cc --exe $(VERILATOR_FLAGS)  \
		-o $(abspath $(EMU)) -Mdir $(@D) $^ $(EMU_DEPS)

$(EMU): $(EMU_MK) $(EMU_DEPS) $(EMU_HEADERS)
	$(MAKE) VM_PARRLLEL_BUILDS=1 -C $(dir $(EMU_MK)) -j33 -f $(abspath $(EMU_MK))

clean-softfloat:
	$(MAKE) -s -C $(SOFTFLOAT_BUILD_PATH) clean

emu: $(SOFTFLOAT) $(EMU)

clean:
	rm -rf build

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.scalalib.GenIdea/idea

test:
	mill YunSuan.test.test
