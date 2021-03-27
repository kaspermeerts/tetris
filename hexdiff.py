#!/usr/bin/env python
"""
Hex diff
Usage:
    hexdiff <FILE> <FILE>
"""

import sys

try:
    f1 = open(sys.argv[1], 'rb')
    f2 = open(sys.argv[2], 'rb')
except IndexError:
    print(__doc__.strip())
    sys.exit(1)
except IOError as e:
    print(e)
    sys.exit(1)
chunk_size = 16
offset = 0
while True:
    c1 = f1.read(chunk_size)
    c2 = f2.read(chunk_size)
    if c1 != c2:
        for i in range(chunk_size):
            if c1[i] != c2[i]:
                offset += i
                break
        hex1 = c1.hex().upper()
        hex1 = " ".join([ hex1[i:i+2] for i in range(0, len(hex1), 2) ])
        hex2 = c2.hex().upper()
        hex2 = " ".join([ hex2[i:i+2] for i in range(0, len(hex2), 2) ])
        diff = ''.join(x == y and '   ' or '^^ ' for x, y in zip(c1, c2))
        offset_str = "0x{:X}".format(offset)
        pad = ' ' * (len(offset_str) + 2)
        print('{}{}\n{}> {}\n{}{}'.format(pad, hex1, offset_str, diff, pad, hex2))
        sys.exit(1)
    if c1 == b'' or c2 == b'':
        break
    offset += chunk_size
f1.close()
f2.close()
