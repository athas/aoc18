#!/usr/bin/env python3

import sys
import re

def on_line(line):
    line_re = re.compile('position=< *(-?[0-9]+), *(-?[0-9]+)> velocity=< *(-?[0-9]+), *(-?[0-9]+)>')
    match = line_re.search(line)
    return '[' + ', '.join([match.group(1), match.group(2), match.group(3), match.group(4)]) + ']'

print('[')
print(',\n'.join(on_line(line) for line in sys.stdin))
print(']')
