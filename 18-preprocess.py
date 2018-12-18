#!/usr/bin/env python3

import sys

print('[', ', '.join('[' + ', '.join(str(ord(c)) for c in line) + ']'
                     for line in sys.stdin.read().splitlines()), ']')
