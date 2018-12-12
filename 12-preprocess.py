#!/usr/bin/env python3

import sys

def pot(c):
    return '1' if c == '#' else '0'

lines = sys.stdin.read().splitlines()
print('[', ', '.join(pot(c) for c in lines[0].split(' ')[2]), ']')

def on_line(line):
    a, _, b = line.split(' ')
    return '[' + ', '.join(pot(c) for c in a + b) + ']'

print('[', ', '.join(on_line(line) for line in lines[2:]), ']')
