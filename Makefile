.PHONY: all clean check
.SUFFIXES:

objects := tetris.o audio.o wram.o hram.o music.o

all: tetris.gb check

clean:
	rm -f tetris.gb $(objects)

check: tetris.gb
ifneq (,$(wildcard ./baserom.gb))
	@./hexdiff.py baserom.gb tetris.gb
else
	@sha1sum --check --quiet rom.sha1
endif

%.o: %.asm
	@echo " ASM	$@"
	@rgbasm --export-all --halt-without-nop --preserve-ld --output $@ $<

tetris.gb: $(objects)
	@echo " LINK	$@"
	@rgblink --dmg --tiny --sym $(basename $@).sym --map $(basename $@).map --output $@ $^
	@rgbfix --validate --title TETRIS --old-licensee 1 --rom-version 1 $@
