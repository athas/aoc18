#!/usr/bin/env python3

import sys
import re

def on_line(line):
    line_re = re.compile('\[([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)\] (.*)')
    event_re = re.compile('Guard #([0-9]+) begins shift')
    match = line_re.search(line)
    event = match.group(6)
    shift = event_re.search(event)

    if shift:
        event_num = shift.group(1)
    elif event == 'wakes up':
        event_num = str(2<<16)
    else:
        event_num = str(1<<16)
    return '[' + ', '.join(map(lambda s: str(int(s)),
                               [match.group(1), match.group(2), match.group(3),
                                match.group(4), match.group(5), event_num])) + ']'

print('[')
print(',\n'.join(on_line(line) for line in sys.stdin))
print(']')
