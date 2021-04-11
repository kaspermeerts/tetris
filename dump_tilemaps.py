def dump_tilemap(name, start, stride, rows=0):
    offset = start
    row = 0
    column = 0

    print("{}::".format(name))
    while True:
        byte = rom[offset]
        offset += 1
        if rows == 0 and byte == 0xFF:
            if column != 0:
                print()
            print("    db $FF")
            break
        elif row != 0 and row == rows:
            break

        if column == 0:
            print("    db ".format(byte), end='')
        else:
            print(", ".format(byte), end='')
        print("${:02X}".format(byte), end='')

        column += 1
        if column == stride:
            row += 1
            column = 0
            print()
    print()


if __name__ == "__main__":
    with open("baserom.gb", "rb") as f:
        rom = f.read()

    dump_tilemap("scoreboard", 0x510F, 10)
    dump_tilemap("buranbackground", 0x51C4, 16, 4)
    dump_tilemap("lefttowerleftside", 0x141B, 7, 1)
    dump_tilemap("lefttowerrightside", 0x1422, 7, 1)
    dump_tilemap("righttowerleftside", 0x1429, 7, 1)
    dump_tilemap("righttowerrightside", 0x1430, 7, 1)
    dump_tilemap("congratulations", 0x12F5, 16, 1)
