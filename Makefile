.PHONY: all clean check
.SUFFIXES:

objects := tetris.o audio.o wram.o hram.o music.o sprites.o

graphics := gfx/font.1bpp \
            gfx/configandgameplay.2bpp \
            gfx/copyrightandtitlescreen.2bpp \
            gfx/multiplayerandburan.2bpp

all: tetris.gb check

clean:
	rm -f tetris.gb $(objects) $(graphics)

check: tetris.gb
ifneq (,$(wildcard ./baserom.gb))
	@./hexdiff.py baserom.gb tetris.gb
else
	@sha1sum --check --quiet rom.sha1
endif

gfx/font.1bpp: gfx/font.png
	@echo " GFX	$@"
	@rgbgfx --depth 1 --trim-end 9 --output $@ $<

gfx/configandgameplay.2bpp: gfx/configandgameplay.png
	@echo " GFX	$@"
	@rgbgfx --trim-end 11 --output $@ $<

gfx/copyrightandtitlescreen.2bpp: gfx/copyrightandtitlescreen.png
	@echo " GFX	$@"
	@rgbgfx --trim-end 9 --output $@ $<

gfx/multiplayerandburan.2bpp: gfx/multiplayerandburan.png
	@echo " GFX	$@"
	@rgbgfx --trim-end 1 --output $@ $<

tetris.o: $(graphics)

%.o: %.asm
	@echo " ASM	$@"
	@rgbasm --export-all --halt-without-nop --preserve-ld --output $@ $<

tetris.gb: $(objects)
	@echo " LINK	$@"
	@rgblink --dmg --tiny --sym $(basename $@).sym --map $(basename $@).map --output $@ $^
	@rgbfix --validate --title TETRIS --old-licensee 1 --rom-version 1 $@
