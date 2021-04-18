# Tetris Disassembly

This is a disassembly for the Game Boy game Tetris

It is based on the ROM `Tetris (JUE) (V1.1) [!].gb` with SHA1 checksum `74591cc9501af93873f9a5d3eb12da12c0723bbc`

As the disassembly is not complete yet, it requires a copy of the original ROM named `baserom.gb` to fill in sections which have not been disassembled yet.

## Requirements

* RGBDS 0.4.2

## Trivia

* Some routines are very similar to ones in [Super Mario Land](https://github.com/kaspermeerts/supermarioland), even at the exact same memory address, e.g. the `LookupTile` routine at `$153`.
  * The routine at `$153` isn't even used in the game.
  * The (as of yet undocumented) music and animation in particular are identical in their "API", and are somewhat separated from the rest of the code. Were these external libraries?
  * Moreover, some (harmless) bugs are identical as well, e.g. when copying the DMA routine to High RAM, two bytes too many are copied over.

* During initialization, Tetris writes to an address in ROM that is usually used to select a ROM bank, for cartridges that have a Memory Bank Controller, which Tetris does not. I don't know if this is a relic from development, or if the game used to be larger, was supposed to come with an MBC and the developers only managed to cut the size down to below 32 kB later. Curiously, the game only barely fits in 32 kB, with only a few dozen bytes to spare

* Speaking of keeping the size low, there are a few routines that are unused, some duplicated code, as well as some suboptimal assembly, e.g. using `jr` where are `jp` would work. What development tools did they use in the late eighties, did those not have dead code elimination?

* A whole quarter of work RAM is dedicated to just the high scores (not that the memory is in short supply). It's curious how much work they put into this, yet the scores disappear once you turn the Game Boy off, or the batteries die. Did they intend to include some non-volatile cartridge RAM? To be seen.

* Tetris does not use the `halt` instruction during its execution. This is strange, because the game uses so little CPU that this could have significantly extended battery life. Perhaps it interferes with the serial connection? Was Tetris known to be power-hungry?

* There are facilities to record demos left in the code. They are technically not even unused, the routine `RecordDemo` is called once every frame during normal gameplay, but the recording isn't active unless `$FFE9` contains `$FF`. Although some code exists to set that value, it is never called anywhere else, unless you remove the `ret` at the end of the `StartDemo` routine. The default addresses for writing demo data are also in ROM, so those would have to be patched too, or the writes would have had to have been intercepted somehow.

* More than a few times way too much data is copied, e.g. when the tileset for the Buran and multiplayer mode is loaded into VRAM, a chunk of demo data comes along with it. This is harmless.
