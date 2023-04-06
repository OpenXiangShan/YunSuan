#!/bin/python
# encoding: utf-8
import os
import sys
import random
import time

std_out_files = os.popen('ls ../log | grep std_out').read().strip().split('\n')
logs = ''
num = 0
num_err = 0
for std_out_log in std_out_files:
    with open('../log/'+std_out_log) as file:
        content = file.read()
        if 'Error' in content:
            logs += content+'\n'
            num_err += 1
        elif content=='':
            print(std_out_log+' is empty, not finished or check std_err_log')
        else:
            num += 1
if (logs == ''):
    print(str(num)+' logs are right')
else:
    print(str(num_err)+' error logs, please check it in ../log/logs.log')
    with open('../log/logs.log','w') as file:
        file.write(logs)
