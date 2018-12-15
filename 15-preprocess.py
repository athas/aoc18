#!/usr/bin/env python3

import sys

def on_line(line):
    return '[' + ', '.join(str(ord(c)) for c in line) + ']'

print('[', ', '.join(map(on_line, sys.stdin.read().splitlines())), ']')
