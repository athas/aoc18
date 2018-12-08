#!/usr/bin/env python3

import sys

print('[', ', '.join(word for word in sys.stdin.read().split(' ')), ']')
