#!/usr/bin/env python3

import sys

i = 0
for state in eval(sys.stdin.read().replace('false', 'False').replace('true', 'True')):
    print('%.2d:' % i, ''.join('.' if c == False else '#' for c in state))
    i += 1
