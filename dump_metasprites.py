def dump_metasprites(name, start, number):
    offset = start
    y = 0

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

    dump_metasprites("dancers", 0x2735, 10)
    dump_metasprites("buran", 0x2771, 3)
