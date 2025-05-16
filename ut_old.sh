#!/bin/bash
set -e
mill YunSuan.runMain race.vpu.debug.VerilogVTopDebug
cd src/test/csrc/softmax_test/
make clean
make sim
cd ../../../../