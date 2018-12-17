#!/usr/bin/env python3

import sys
import re

x_re = re.compile('x=([0-9]+), y=([0-9]+)\\.\\.([0-9]+)')
y_re = re.compile('y=([0-9]+), x=([0-9]+)\\.\\.([0-9]+)')

def on_line(line):
    xm = x_re.match(line)
    if xm:
        x = int(xm.group(1))
        return [ [x,y] for y in range(int(xm.group(2)), int(xm.group(3))+1) ]
    else:
        ym = y_re.match(line)
        y = int(ym.group(1))
        return [ [x,y] for x in range(int(ym.group(2)), int(ym.group(3))+1) ]

print('[', ', '.join(str(x) for l in sys.stdin.read().splitlines() for x in on_line(l)), ']')
