#!/usr/bin/env python
import binascii
from math import log2

NOTES = ['C ', 'C#', 'D ', 'D#', 'E ', 'F ', 'F#', 'G ', 'G#', 'A ', 'A#', 'B ']

with open("baserom.gb", "rb") as f:
    f.seek(0x6E02)
    NOTE_DATA = f.read(0x6E94 - 0x6E02)

def closest_note(hex_freq):
    freq = 0x20000 / (0x800 - hex_freq)
    note = 9 + round(12 * log2(freq/440)) # A is 9 semitones above C
    octave = 4 + note // 12 # A440 is the first A above C4
    scale_degree = note % 12
    cents_off = 1200 * (log2(freq/440) - (note - 9)/12)

    return freq, scale_degree, octave, cents_off

for i in range(0, len(NOTE_DATA), 2):
    hex_freq = NOTE_DATA[i] + NOTE_DATA[i+1]*0x100

    try:
        freq, degree, octave, cents_off = closest_note(hex_freq)
    except ValueError:
        print("\tdw $%03X ; " % (hex_freq))
        continue

    print("\tdw $%03X ; %6.1f Hz %s%d (%+.0f)" % (hex_freq, freq, NOTES[degree], octave, cents_off), end='')

    _, _, _, cents_up = closest_note(hex_freq + 1)
    _, _, _, cents_down = closest_note(hex_freq - 1)
    if abs(cents_up) < abs(cents_off):
        print(" Bug! %03X is closer" % (hex_freq + 1))
    elif abs(cents_down) < abs(cents_off):
        print(" Bug! %03X is closer" % (hex_freq - 1))
    else:
        print("")

while True:
    try:
        num = input()
    except EOFError:
        break

    freq = 0x20000 / (0x800 - int(num, 16))
    closest_note = 9 + round(12*log2(freq/440))
    octave = 4 + closest_note // 12
    scale_degree = closest_note % 12
    cents_off = 1200 * (log2(freq/440) - (closest_note - 9)/12)

    print("%.2f Hz %s%d %.1f cents" % (freq, NOTES[scale_degree], octave, cents_off))

