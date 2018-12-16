#!/usr/bin/env python3

import sys
import re

part1, part2 = sys.stdin.read().split('\n\n\n\n')

reg_re = re.compile('(?:Before|After): *(\\[[0-9], [0-9], [0-9], [0-9]\\])')

def on_example(e):
    bef, op, aft = e.splitlines()
    bef_regs = reg_re.search(bef).group(1)
    aft_regs = reg_re.search(aft).group(1)
    return '[' + ', '.join([bef_regs, '[' + ', '.join(op.split(' ')) + ']', aft_regs]) + ']'

print('[', ', '.join(on_example(e) for e in part1.split('\n\n')), ']')

def on_code_line(line):
    return '[' + ', '.join(line.split(' ')) + ']'

print('[', ', '.join(on_code_line(line) for line in part2.split('\n')), ']')
