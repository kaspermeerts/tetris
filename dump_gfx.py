import png

# Decode a 2bpp encoded tile to a list of 8*8 pixels
def tile_2bpp_to_pixels(tile):
    assert len(tile) == 16

    pixels = []

    for row in range(8):
        hi = tile[row*2 + 1]
        lo = tile[row*2]

        for col in reversed(range(8)):
            pixels += [3 - (((hi*2) >> col & 0b10) + (lo >> col & 0b01))]

    return pixels

def tile_1bpp_to_pixels(tile):
    assert len(tile) == 8

    pixels = []

    for row in range(8):
        byte = tile[row]

        for col in reversed(range(8)):
            pixels += [1 - ((byte >> col) & 0b1)]

    return pixels

def dump_tiles(name, address, tile_count, oneBPP=False):
    off = address

    if oneBPP == False:
        image = rom[off: off + tile_count*16]
    else:
        image = rom[off: off + tile_count*8]

    tile_width = 16
    # Round up
    tile_height = (tile_count + tile_width - 1) // tile_width

    width = tile_width * 8
    height = tile_height * 8

    pixelmap = [[1 if oneBPP else 3 for _ in range(width)] for _ in range(height)]

    for i in range(tile_count):
        x = (i % tile_width) * 8
        y = (i // tile_width) * 8
        if oneBPP == False:
            pixels = tile_2bpp_to_pixels(image[i*16: i*16 + 16])
        else:
            pixels = tile_1bpp_to_pixels(image[i* 8: i* 8 +  8])

        for py in range(8):
            for px in range(8):
                pixelmap[y+py][x+px] = pixels[py*8 + px]


    f = open("gfx/%s.png" % name, "wb")
    w = png.Writer(tile_width * 8, tile_height * 8, greyscale=True, bitdepth=1 if oneBPP else 2)
    w.write(f, pixelmap)

if __name__ == "__main__":
    with open("baserom.gb", "rb") as f:
        rom = f.read()

    dump_tiles("font", 0x415F, 39, oneBPP=True)
    dump_tiles("copyrightandtitlescreen", 0x415F + 39 * 8, 119, oneBPP=False)
    dump_tiles("configandgameplay", 0x323F, 197, oneBPP=False)
    dump_tiles("multiplayerandburan", 0x55AC, 207, oneBPP=False)
