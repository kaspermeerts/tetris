def dump_pointers(start, number):

    for index in range(number):
        addr = rom[start + index*2] + (rom[start + index*2 + 1] << 8)
        print("    dw ${:04X}".format(addr))

if __name__ == "__main__":
    with open("baserom.gb", "rb") as f:
        rom = f.read()

    dump_pointers(0x6480, (0x64D2 - 0x6480)//2)
