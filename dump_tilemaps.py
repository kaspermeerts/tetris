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

    dump_tilemap("scoreboard",               0x510F, 10)
    dump_tilemap("buranbackground",          0x51C4, 20, 4)
    dump_tilemap("lefttowerleftside",        0x141B, 7, 1)
    dump_tilemap("lefttowerrightside",       0x1422, 7, 1)
    dump_tilemap("righttowerleftside",       0x1429, 7, 1)
    dump_tilemap("righttowerrightside",      0x1430, 7, 1)
    dump_tilemap("congratulations",          0x12F5, 16, 1)
    dump_tilemap("titlescreen",              0x4B6F, 20, 18)
    dump_tilemap("gameplay",                 0x4CD7, 20, 18)
    dump_tilemap("typeAdifficulty",          0x4E3F, 20, 18)
    dump_tilemap("typeBdifficulty",          0x4FA7, 20, 18)
    dump_tilemap("multiplayerdifficulty",    0x5214, 20, 18)
    dump_tilemap("multiplayergameplay",      0x537C, 20, 18)
    dump_tilemap("multiplayervictorytop",    0x54E4, 20, 4)
    dump_tilemap("multiplayervictorybottom", 0x5534, 20, 6)
    dump_tilemap("typeagameplay",            0x3E8F, 20, 18)
    dump_tilemap("typebgameplay",            0x3FF7, 20, 18)
