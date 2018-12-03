#!/usr/bin/env python3

import sys
import re

def on_line(line):
    line_re = re.compile('#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)')
    match = line_re.search(line)
    id, left, top, width, height = match.group(1), match.group(2), match.group(3), match.group(4), match.group(5)
    return '[' + ', '.join([id, left, top, width, height])+ ']'

print('[')
print(',\n'.join(on_line(line) for line in sys.stdin))
print(']')
