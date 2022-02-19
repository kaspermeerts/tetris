def print_pattern(addr):
    pattern = [0] * 32
    for i in range(32):
        if i % 2 == 0:
            pattern[i] = rom[addr + i // 2] >> 4
        else:
            pattern[i] = rom[addr + i // 2] % 16

    for j in reversed(range(8)):
        print(';', end='')
        for i in range(32):
            if pattern[i] == 2*j + 1:
                print("▀",end='')
            elif pattern[i] == 2*j:
                print("▄",end='')
            else:
                print("░", end='')
        print()
    print()

with open("baserom.gb", "rb") as f:
    rom = f.read()

print_pattern(0x6EA9)
print_pattern(0x6EB9)
print_pattern(0x6EC9)
print_pattern(0x6ED9)
print_pattern(0x6EE9)

