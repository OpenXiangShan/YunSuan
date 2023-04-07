#!/bin/python
# encoding: utf-8
import os
import sys
import random
import time

os.system('mkdir -p ../log')
random.seed()

for i in range(int(sys.argv[1])):
    seed_int = random.randint(1,(1<<31)-1)
    time.sleep(0.01)
    os.system('../build/emu -s {seed} -O {ops} --dump-wave -b 0 -e 0 2>../log/std_err_{seed}.log 1>../log/std_out_{seed}.log &'.format(seed = seed_int, ops = sys.argv[2]))
