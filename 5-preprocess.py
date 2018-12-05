#!/usr/bin/env python3

import sys

print('[', ', '.join(str(ord(c))+'i8' for c in sys.stdin.read()), ']')
