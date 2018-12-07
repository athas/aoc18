#!/usr/bin/env python3

import sys
import re

def on_line(line):
    line_re = re.compile('Step ([A-Z]) must be finished before step ([A-Z]) can begin.')
    match = line_re.search(line)
    x = ord(match.group(1)) - ord('A')
    y = ord(match.group(2)) - ord('A')
    return '[' + str(x) + ', '+ str(y) + ']'

print('[')
print(',\n'.join(on_line(line) for line in sys.stdin))
print(']')
