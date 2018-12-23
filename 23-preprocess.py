#!/usr/bin/env python3

import sys

import re
line_re=re.compile('pos=<(-?[0-9]+),(-?[0-9]+),(-?[0-9]+)>, r=([0-9]+)')

def on_line(line):
    match = line_re.search(line)
    return '[' + ','.join(match.groups()) + ']'

print('[')
print(',\n'.join(on_line(line) for line in sys.stdin))
print(']')
