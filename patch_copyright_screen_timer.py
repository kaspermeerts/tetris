#!/usr/bin/env python

with open("tetris.gb", "r+b") as f:
    # First copyright screen timer
    f.seek(0x38B)
    f.write(bytes([0x05]))
    # Second copyright screen timer
    f.seek(0x39A)
    f.write(bytes([0x05]))
    # Global checksum, to prevent BGB from complaining
    f.seek(0x14E)
    f.write(bytes([0x15, 0x29]))
