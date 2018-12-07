#!/usr/bin/env python3

import sys
import re

print(''.join(chr(c+ord('A')) for c in eval(sys.stdin.read().replace('i32', ''))))
