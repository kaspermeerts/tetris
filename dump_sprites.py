matrices = {
    0x31A9 : [
        (0, 0), (0, 1), (0, 2), (0, 3),
        (1, 0), (1, 1), (1, 2), (1, 3),
        (2, 0), (2, 1), (2, 2), (2, 3),
        (3, 0), (3, 1), (3, 2), (3, 3)],

    0x31C9 : [
        (0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7)],

    0x31D9 : [
        (0, 0), (0, 1),
        (1, 0), (1, 1),
        (2, 0), (2, 1),
        (3, 0), (3, 1),
        (4, 0), (4, 1),
        (5, 0), (5, 1),
        (6, 0), (6, 1)],

    0x31F5 : [
                (0, 1), (0, 2),
                (1, 1), (1, 2),
        (2, 0), (2, 1), (2, 2), (2,3),
        (3, 0), (3, 1), (3, 2), (3,3),
        (4, 0), (4, 1), (4, 2), (4,3),
        (5, 0), (5, 1), (5, 2), (5,3),
        (6, 0), (6, 1), (6, 2), (6,3),
        (7, 0), (7, 1), (7, 2), (7,3)],

    0x322D : [
        (0, 0), (0, 1), (0, 2),
        (1, 0), (1, 1), (1, 2),
        (2, 0), (2, 1), (2, 2),]
}

def dump_spritelist(start, number):

    sprite_pointers = set()
    sprite_tile_pointers = set()

    for index in range(number):
        addr = rom[start + index*2] + (rom[start + index*2 + 1] << 8)
        sprite_pointers.add(addr)
        addr2 = rom[addr] + (rom[addr+1] << 8)
        sprite_tile_pointers.add(addr2)

    print('SECTION "Sprite data", ROM0[$2C20]\n')
    for addr in sorted(sprite_pointers | sprite_tile_pointers):
        if addr in sprite_pointers:
            print(f"Sprite_{addr:04X}::")
            addr2 = rom[addr] + (rom[addr+1] << 8)
            print(f"    dw SpriteTiles_{addr2:04X}")
            offx, offy = rom[addr + 2], rom[addr + 3]
            offxs = f"${offx:02X}" if offx < 0x80 else f"-${256-offx:02X}"
            offys = f"${offy:02X}" if offy < 0x80 else f"-${256-offy:02X}"
            print(f"    db {offxs}, {offys}")
        elif addr in sprite_tile_pointers:
            print(f"SpriteTiles_{addr:04X}::")
            matrixaddr = rom[addr] + (rom[addr+1] << 8)
            assert matrixaddr in matrices
            print(f"    dw Matrix_{matrixaddr:04X}")
            x = 0
            y = -1
            tile_index = 0
            byte_index = 0
            start = addr + 2
            byte = rom[start]
            while byte != 0xFF:
                (y_off, x_off) = matrices[matrixaddr][tile_index]
                if y_off > y:
                    print()
                    print("    db ", end='')
                    y = y_off
                    x = 0
                if x_off > x:
                    print("     " * (x_off - x), end='')
                    x = x_off
                print(f"${byte:02X}, ", end='')
                byte_index += 1
                if byte != 0xFD:    # TODO?
                    tile_index += 1
                byte = rom[start + byte_index]
                x += 1

            print(f"$FF")
        else:
            raise AssertionError

        print()

if __name__ == "__main__":
    with open("baserom.gb", "rb") as f:
        rom = f.read()

    dump_spritelist(0x2B64, 0x5E)

    #dump_sprite_data("dancers", 0x2735, 10)
    #dump_sprite_data("buran", 0x2771, 3)
