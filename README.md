# YunSuan
This repo includes XiangShan's function units


## Unit-Test with Verilator

* Prepare environment with verilator/mill.
* `make emu [EMU_TRACE=1]`
* `./build/emu [-C MAX_CYCLES] [-O MAX_OPERATIONS] [--dump-wave] [-b begin_dump] [-e end_dump]`
* `./build/emu --lowp-only=FMT` restricts the random test to the custom low precision conversions,
  FMT is one of `mixed|mxfp8|mxfp4|xx8-int8|xx8-e4m3|xx8-e5m2`.

Others:

* `make clean` to clean build dir.
* `make clean-softfloat` to clean softfloat compile dir.

## Scripts

* `make emu EMU_TRACE=1`
* `cd scripts`
* Run emu parallelly, logs are in ./log, for exeample, run 10 emu with 100 oprations each.
* `python3 emu_parallel.py 10 100`
* Analyse logs, the number of error logs or empty logs, all error logs are summarized in ./log/logs.log.
* `python3 log_analyse.py`