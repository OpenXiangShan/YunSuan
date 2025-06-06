#!/bin/bash
set -e
mill YunSuan.runMain race.vpu.debug.VerilogVTopDebug
cd src/test/csrc/difftest/
make sim-all
cd ../../../../