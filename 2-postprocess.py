#!/usr/bin/env python3
#
# Convert Futhark array back into string.

import sys

print(''.join(map(lambda x: chr(int(x)),
                  sys.stdin.read().
                  replace('i8', '').
                  replace('[','').
                  replace(']','').
                  replace(',','').split())))
