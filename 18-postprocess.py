#!/usr/bin/env python3

import sys

for line in eval(sys.stdin.read().replace('i32', '')):
    print(''.join(map(chr,line)))
