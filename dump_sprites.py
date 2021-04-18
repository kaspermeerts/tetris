def dump_spritelist(start, number):

    matrices = set()

    for index in range(number):
        addr = rom[start + index*2] + (rom[start + index*2 + 1] << 8)
        addr2 = rom[addr] + (rom[addr+1] << 8)
        addr3 = rom[addr2] + (rom[addr2+1] << 8)
        print("    dw ${:04X}".format(rom[addr2] + (rom[addr2+1] << 8)))
        matrices.add(addr3)

    for addr in sorted(list(matrices)):
        print("${:04X}".format(addr))

def dump_sprite_data(name, start, number):
    offset = start
    y = 0

    print("{}::".format(name))
    for _ in range(number * 6):
        byte = rom[offset]
        offset += 1

        if y == 0:
            print("    db ".format(byte), end='')
        else:
            print(", ".format(byte), end='')
        print("${:02X}".format(byte), end='')

        y += 1
        if y == 6:
            y = 0
            print()


if __name__ == "__main__":
    with open("baserom.gb", "rb") as f:
        rom = f.read()

    dump_spritelist(0x2B64, 0x5E)

    dump_sprite_data("dancers", 0x2735, 10)
    dump_sprite_data("buran", 0x2771, 3)
