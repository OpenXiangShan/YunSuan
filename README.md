# YunSuan
This repo includes XiangShan's function units


## Unit-Test with Verilator

* Prepare environment with verilator/mill.
* `make emu [EMU_TRACE=1]`
* `./build/emu [-C MAX_CYCLES] [-O MAX_OPERATIONS] [--dump-wave] [-b begin_dump] [-e end_dump]`

Others:

* `make clean` to clean build dir.
* `make clean-softfloat` to clean softfloat compile dir.