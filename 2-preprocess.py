#!/usr/bin/env python3

import sys

print('[')
print(',\n'.join('[' + ', '.join(map(lambda c: str(ord(c))+"i8",list(line))) + ']'
                 for line in sys.stdin))
print(']')
