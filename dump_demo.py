def dump_demo(name, start, length):

    print("{}::".format(name))
    for i in range(length):
        byte = rom[start + i]

        if i % 0x10 == 0:
            print("    db ".format(byte), end='')
        else:
            print(", ".format(byte), end='')
        print("${:02X}".format(byte), end='')

        if i % 0x10 == 0xF:
            print()
    print()


if __name__ == "__main__":
    with open("baserom.gb", "rb") as f:
        rom = f.read()

    dump_demo("DemoTypeAData", 0x62B0, 0x100)
    dump_demo("DemoTypeBData", 0x63B0, 0xA0)
