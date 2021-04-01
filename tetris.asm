INCLUDE "hardware.inc"
INCLUDE "charmap.asm"

; RST vectors (unused?)
SECTION "RST 0", ROM0[$0000]
    jp Init
SECTION "RST 8", ROM0[$0008]
    jp Init
    ds $28 - @, $FF     ; RST 10, 18, 20 are just $FF
SECTION "RST 28", ROM0[$0028]
; Immediately following the return address is a jump table
; A - index into table
TableJump::
    add a               ; Double A, as addresses are 16 bit
    pop hl
    ld e, a
    ld d, 0
    add hl, de          ; Add offset to base address
    ld e, [hl]          ; Load address into DE
    inc hl
    ld d, [hl]
    push de             ; Load DE into HL and jump to target
    pop hl
    jp hl

    ds $40 - @, $FF     ; Pad with $FF

SECTION "Interrupt VBlank", ROM0[$0040]
    jp VBlank
    ds $48 - @, $FF

SECTION "Interrupt LCD STAT", ROM0[$0048]
    jp $26BE
    ds $50 - @, $FF

SECTION "Interrupt Timer", ROM0[$0050]
    jp $26BE
    ds $58 - @, $FF

; TODO All this multiplayer stuff
SECTION "Interrupt Serial", ROM0[$0058]
    jp _Serial          ; Why??
_Serial::
    push af
    push hl
    push de
    push bc
    call SerialTableJump; To prepare the return address for the upcoming RST $28
    ld a, 1
    ldh [hSerialInterruptTriggered], a
    pop bc
    pop de
    pop hl
    pop af
    reti

SerialTableJump::
    ldh a, [$CD]
    rst $28             ; TableJump
    dw Call_78
    dw Call_9F
    dw Call_A4
    dw Call_BA
    dw $27EA            ; XXX Is this used? The target is a simple ret

Call_78:
    ldh a, [hGameState]
    cp a, 7
    jr z, .label_86
    cp a, 6
    ret z
    ld a, 6             ; If not at or going to the title screen, go back to
    ldh [hGameState], a ; the title screen
    ret

.label_86:
    ldh a, [rSB]
    cp a, $55
    jr nz, .label_94
    ld a, $29
    ldh [$CB], a
    ld a, 1
    jr .label_9C

.label_94:
    cp a, $29
    ret nz
    ld a, $55
    ldh [$CB], a
    xor a
.label_9C:
    ldh [rSC], a
    ret

Call_9F:
    ldh a, [rSB]
    ldh [$D0], a
    ret

Call_A4:
    ldh a, [rSB]
    ldh [$D0], a
    ldh a, [$CB]
    cp a, $29
    ret z
    ldh a, [$CF]
    ldh [rSB], a
    ld a, $FF
    ldh [$CF], a
    ld a, $80
    ldh [rSC], a
    ret

Call_BA:
    ldh a, [rSB]
    ldh [$D0], a
    ldh a, [$CB]
    cp a, $29
    ret z
    ldh a, [$CF]
    ldh [rSB], a
    ei
    call DelayMillisecond
    ld a, $80
    ldh [rSC], a
    ret

Label_D0:
    ldh a, [$CD]
    cp a, 2
    ret nz
    xor a
    ldh [rIF], a
    ei
    ret

    ds $100 - @, $FF

SECTION "Entry point", ROM0[$0100]
    nop
    jp _Start
SECTION "Header", ROM0[$0104]
; The cartridge header will be filled in by rgbfix
    ds $150 - @

_Start::
    jp Init

; This is the exact same routine as in Super Mario Land
; XXX Is this unused?
LookupTile::
    call Call_29E3
.wait1
    ldh a, [rSTAT]
    and a, STATF_VBL | STATF_OAM
    jr nz, .wait1
    ld b, [hl]
.wait2
    ldh a, [rSTAT]
    and a, STATF_VBL | STATF_OAM
    jr nz, .wait2
    ld a, [hl]
    and b
    ret

; Add BCD encoded DE to the six digit number at [HL]
; Very similar to the Super Mario Land version
AddScore::
    ld a, e
    add [hl]            ; Add E to the ones and tens
    daa
    ldi [hl], a
    ld a, d
    adc [hl]            ; Add D to the hundreds and thousands
    daa
    ldi [hl], a
    ld a, 0
    adc [hl]            ; Add 0 to propagate the carry
    daa
    ld [hl], a
    ld a, 1
    ldh [$E0], a        ; TODO Update displayed score?
    ret nc
    ld a, $99           ; Saturate score at 999 999
    ldd [hl], a
    ldd [hl], a
    ld [hl], a
    ret

VBlank::
    push af
    push bc
    push de
    push hl
    ldh a, [$CE]        ; Probably smth to do with multiplayer?
    and a
    jr z, Label_199
    ldh a, [$CB]
    cp a, $29
    jr nz, Label_199
    xor a
    ldh [$CE], a
    ldh a, [$CF]
    ldh [rSB], a
    ld hl, rSC          ; Why?
    ld [hl], $81        ; Missing in hardware.inc -_-
Label_199:
    call $21E0
    call $23CC
    call $23B7
    call $239E
    call $238C
    call $237D
    call $236E
    call $235F
    call $2350
    call $2341
    call $2332
    call $2323
    call $22F8
    call $22E9
    call $22DA
    call $22CB
    call $22BC
    call $22AD
    call $229E
    call $1ED7
    call hDMARoutine
    call $18CA
    ld a, [$C0CE]       ; Score needs updating?
    and a
    jr z, Label_1FB
    ldh a, [$98]
    cp a, 3
    jr nz, Label_1FB
    ld hl, _SCRN0 + SCRN_VX_B * 3 + 13
    call $243B
    ld a, 1
    ldh [$E0], a
    ld hl, _SCRN1 + SCRN_VX_B * 3 + 13
    call $243B
    xor a
    ld [$C0CE], a
Label_1FB:
    ld hl, hFrameCounter
    inc [hl]
    xor a
    ldh [rSCX], a
    ldh [rSCY], a
    inc a
    ldh [hVBlankInterruptTriggered], a
    pop hl
    pop de
    pop bc
    pop af
    reti

Init::
    xor a
    ld hl, $D000 + $1000 - 1    ; End of non-existent cartridge RAM, Bug?
    ld c, $10
    ld b, $00
.clearWRAM1loop
    ldd [hl], a
    dec b
    jr nz, .clearWRAM1loop
    dec c
    jr nz, .clearWRAM1loop

.softReset:
    ld a, IEF_VBLANK
    di
    ldh [rIF], a
    ldh [rIE], a
    xor a
    ldh [rSCY], a
    ldh [rSCX], a
    ldh [$A4], a
    ldh [rSTAT], a
    ldh [rSB], a
    ldh [rSC], a
    ld a, LCDCF_ON
    ldh [rLCDC], a
.wait
    ldh a, [rLY]
    cp a, $94           ; TODO define LY_VBLank
    jr nz, .wait
    ld a, LCDCF_BGON | LCDCF_OBJON
    ldh [rLCDC], a
    ld a, %11100100     ; 3210
    ldh [rBGP], a
    ldh [rOBP0], a
    ld a, %11000100     ; 3010
    ldh [rOBP1], a
    ld hl, rNR52
    ld a, $80
    ldd [hl], a         ; Turn the sound on
    ld a, $FF
    ldd [hl], a         ; Output all sounds to both terminals
    ld [hl], $77        ; Maximum volume
    ld a, 1
    ld [$2000], a       ; Bug? Tries to enable non-existent MBC Rom Bank
    ld sp, $CFFF

    xor a
    ld hl, $DFFF        ; Bug? Clears the upper 256 bytes of the non-existent
    ld b, 0             ; cartridge RAM a second time...
.clearWRAM1loop2
    ldd [hl], a
    dec b
    jr nz, .clearWRAM1loop2

    ld hl, $C000 + $1000 - 1    ; End of work RAM
    ld c, $10
    ld b, $00
.clearWRAM0loop
    ldd [hl], a
    dec b
    jr nz, .clearWRAM0loop
    dec c
    jr nz, .clearWRAM0loop

    ld hl, $8000 + $2000 - 1    ; End of VRAM
    ld c, $20
    xor a               ; Unnecessary, bug?
    ld b, $00
.clearVRAMloop
    ldd [hl], a
    dec b
    jr nz, .clearVRAMloop
    dec c
    jr nz, .clearVRAMloop

    ld hl, $FEFF        ; End of OAM (+ bytes FFA0 to FFEF, bug?)
    ld b, 0
.clearOAMloop
    ldd [hl], a
    dec b
    jr nz, .clearOAMloop

    ld hl, $FFFE
    ld b, $80           ; Off by one, bug?
.clearHRAMloop
    ldd [hl], a
    dec b
    jr nz, .clearHRAMloop

; No memory other than HRAM can be accessed during DMA, so the routine is copied
; and executed there
    ld c, LOW(hDMARoutine)
    ld b, DMARoutineEnd - DMARoutine + 2 ; Exact same bug as in Super Mario Land
    ld hl, DMARoutine
.copyDMAroutine
    ldi a, [hl]
    ldh [c], a
    inc c
    dec b
    jr nz, .copyDMAroutine

    call ClearTilemap9800
    call $7FF3          ; Init music?

    ld a, IEF_SERIAL | IEF_VBLANK
    ldh [rIE], a
    ld a, $37
    ldh [hGameType], a
    ld a, $1C
    ldh [hMusicType], a
    ld a, $24
    ldh [hGameState], a
    ld a, LCDCF_ON
    ldh [rLCDC], a
    ei
    xor a
    ldh [rIF], a
    ldh [rWY], a
    ldh [rWX], a
    ldh [rTMA], a

MainLoop::
    call ReadJoypad
    call .dispatch
    call $7FF0          ; Update sound

    ldh a, [hJoyHeld]
    and a, PADF_START | PADF_SELECT | PADF_B | PADF_A
    cp a, PADF_START | PADF_SELECT | PADF_B | PADF_A
    jp z, Init.softReset
    ld hl, hTimer1
    ld b, 2
.decrementTimer
    ld a, [hl]
    and a
    jr z, .skip
    dec [hl]
.skip
    inc l
    dec b
    jr nz, .decrementTimer
    ldh a, [hIsMultiplayer]
    and a
    jr z, .wait
    ld a, IEF_SERIAL | IEF_VBLANK
    ldh [rIE], a
.wait
    ldh a, [hVBlankInterruptTriggered]
    and a
    jr z, .wait
    xor a
    ldh [hVBlankInterruptTriggered], a
    jp MainLoop

.dispatch
    ldh a, [hGameState]
    rst $28

dw $1BCE ; 0x00
dw $1CE2 ; 0x01
dw $1244 ; 0x02
dw $127B ; 0x03
dw $1D06 ; 0x04
dw $1D26 ; 0x05
dw GameState_06 ; Load title screen
dw GameState_07 ; Title screen
dw GameState_08 ; Load Game Type/Music Type Selection Screen
dw $148C ; 0x09
dw $1A07 ; 0x0A
dw $1DC0 ; 0x0B
dw $1F16 ; 0x0C
dw $1F1F ; 0x0D
dw GameState_0E ; Select Game Type
dw GameState_0F ; Select Music Type
dw $157B ; 0x10
dw $15BF ; 0x11
dw $1629 ; 0x12
dw $167A ; 0x13
dw $16EB ; 0x14
dw $1913 ; 0x15
dw $0677 ; 0x16
dw $072C ; 0x17
dw $0825 ; 0x18
dw $08E4 ; 0x19
dw $0B31 ; 0x1A
dw $0CEB ; 0x1B
dw $0AD2 ; 0x1C
dw $0D32 ; 0x1D
dw $0E23 ; 0x1E
dw $1112 ; 0x1F
dw $0D99 ; 0x20
dw $0E8A ; 0x21
dw $1DCE ; 0x22
dw $1E41 ; 0x23
dw GameState_24 ; Load the copyright screen
dw GameState_25 ; Copyright screen
dw $1167 ; 0x26
dw $11E6 ; 0x27
dw $11FC ; 0x28
dw $121C ; 0x29
dw $05C7 ; 0x2A
dw $05F7 ; 0x2B
dw $12B3 ; 0x2C
dw $1305 ; 0x2D
dw $1324 ; 0x2E
dw $1351 ; 0x2F
dw $1367 ; 0x30
dw $137E ; 0x31
dw $13B5 ; 0x32
dw $13E5 ; 0x33
dw $131B ; 0x34
dw GameState_35 ; Copyright screen, but skippable
dw $27EA ; 0x36

GameState_24::
    call DisableLCD
    call LoadCopyrightScreenTileset
    ld de, CopyrightScreenTilemap
    call LoadTilemap9800
    call ClearSprites
    ld hl, $C300        ; Demo data
    ld de, $6450
.loop
    ld a, [de]
    ldi [hl], a
    inc de
    ld a, h
    cp a, $C4
    jr nz, .loop
    ld a, $D3           ; Turn on LCD, background and sprites
    ldh [rLCDC], a
    ld a, 4 * 60 + 10   ; 250 frames, 4⅙ seconds (TODO constant)
    ldh [hTimer1], a
    ld a, $25
    ldh [hGameState], a
    ret

GameState_25::
    ldh a, [hTimer1]
    and a
    ret nz
    ld a, 4 * 60 + 10   ; another 4 seconds TODO
    ldh [hTimer1], a
    ld a, $35
    ldh [hGameState], a
    ret

GameState_35::
    ldh a, [hJoyPressed]
    and a
    jr nz, .skip
    ldh a, [hTimer1]
    and a
    ret nz
.skip
    ld a, $06
    ldh [hGameState], a
    ret

GameState_06::
    call DisableLCD
    xor a
    ldh [$E9], a
    ldh [$98], a
    ldh [$9C], a
    ldh [$9B], a
    ldh [$FB], a
    ldh [$9F], a
    ldh [$E3], a
    ldh [$C7], a
    call $2293
    call $2651
    call LoadCopyrightScreenTileset
    ld hl, $C800
.loop1
    ld a, $2F
    ldi [hl], a
    ld a, h
    cp a, $CC
    jr nz, .loop1
    ld hl, $C801
    call $26A9
    ld hl, $C80C
    call $26A9
    ld hl, $CA41
    ld b, $0C
    ld a, $8E
.loop2
    ldi [hl], a
    dec b
    jr nz, .loop2
    ld de, $4B6F
    call LoadTilemap9800
    call ClearSprites
    ld hl, wOAMBuffer
    ld [hl], $80        ; TODO sets up the cursor sprite
    inc l
    ld [hl], $10
    inc l
    ld [hl], $58
    ld a, $03
    ld [$DFE8], a
    ld a, $D3           ; TODO
    ldh [rLCDC], a
    ld a, $07
    ldh [hGameState], a
    ld a, $7D
    ldh [hTimer1], a
    ld a, $04
    ldh [$C6], a
    ldh  a, [$FFE4]
    and a
    ret nz
    ld a, $13
    ldh [$C6], a
    ret

StartDemo::
    ld a, $37
    ldh [hGameType], a  ; TODO 37 = A-Type, 77 = B-Type
    ld a, 9             ; Demos go fast
    ldh [hTypeALevel], a
    xor a
    ldh [hIsMultiplayer], a
    ldh [$B0], a
    ldh [$ED], a
    ldh [$EA], a
    ld a, $62
    ldh [$EB], a
    ld a, $B0
    ldh [$EC], a
    ldh a, [hDemoNumber]
    cp a, 2
    ld a, 2
    jr nz, .skip        ; First load demo 2, then demo 1
    ld a, $77           ; B-Type
    ldh [hGameType], a
    ld a, 9
    ldh [hTypeBLevel], a
    ld a, 2
    ldh [hTypeBHigh], a
    ld a, $63
    ldh [$EB], a        ; Some type of random seed?
    ld a, $B0
    ldh [$EC], a
    ld a, $11
    ldh [$B0], a        ; Number of blocks played, probably important for determinism
    ld a, 1
.skip
    ldh [hDemoNumber], a
    ld a, $0A
    ldh [hGameState], a
    call DisableLCD
    call Call_27AD
    ld de, $4CD7
    call LoadTilemap9800
    call ClearSprites
    ld a, $D3           ; TODO
    ldh [rLCDC], a
    ret

; Unused?
Call_474::
    ld a, $FF
    ldh [$E9], a
    ret

GameState_07::
    ldh a, [hTimer1]
    and a
    jr nz, .skip
    ld hl, $FFC6
    dec [hl]
    jr z, StartDemo
    ld a, 125           ; 2 and one twelfth of a second
    ldh [hTimer1], a
.skip
    call DelayMillisecond
    ld a, $55
    ldh [rSB], a
    ld a, $80
    ldh [rSC], a
    ldh a, [hSerialInterruptTriggered]
    and a
    jr z, Label_4A2
    ldh a, [$CB]
    and a
    jr nz, Label_4A2.launchMultiplayer
    xor a
    ldh [hSerialInterruptTriggered], a
    jr UpdateCursor.initCursor

Label_4A2:
    ldh a, [hJoyPressed]
    ld b, a
    ldh a, [hIsMultiplayer] ; (Ab)used here to keep track of the pointer
    bit 2, b                ; Select TODO
    jr nz, UpdateCursor.pressedSelect
    bit 4, b                ; Right
    jr nz, UpdateCursor.pressedRight
    bit 5, b                ; Left
    jr nz, UpdateCursor.pressedLeft
    bit 3, b                ; Start
    ret z
    and a
    ld a, $08
    jr z, .pressedStart     ; Launch game
    ld a, b
    cp a, 8
    ret nz
    ldh a, [$CB]
    cp a, $29
    jr z, .launchMultiplayer
    ld a, $29
    ldh [rSB], a
    ld a, $81
    ldh [rSC], a
.wait
    ldh a, [hSerialInterruptTriggered]
    and a
    jr z, .wait
    ldh a, [$CB]
    and a
    jr z, UpdateCursor.initCursor
.launchMultiplayer
    ld a, $2A
.nextState
    ldh [hGameState], a
    xor a
    ldh [hTimer1], a
    ldh [hTypeALevel], a
    ldh [hTypeBLevel], a
    ldh [hTypeBHigh], a
    ldh [hDemoNumber], a
    ret

.pressedStart
    push af
    ldh a, [hJoyHeld]
    bit 7, a            ; Down button. TODO
    jr z, .normalMode
    ldh [hHeartMode], a
.normalMode
    pop af
    jr .nextState

UpdateCursor::
.pressedSelect
    xor a, 1
.updateCursor
    ldh [hIsMultiplayer], a
    and a
    ld a, $10
    jr z, .skip
    ld a, $60               ; TODO
.skip
    ld [wOAMBuffer + 1], a  ; X coordinate
    ret

.pressedRight
    and a
    ret nz
    xor a
    jr .pressedSelect

.pressedLeft
    and a
    ret z
.initCursor
    xor a
    jr .updateCursor


; called every frame, serves to exit the demo if it ended or start is pressed
Call_50C::
    ldh a, [hDemoNumber]
    and a
    ret z
    call DelayMillisecond
    xor a
    ldh [rSB], a
    ld a, $80
    ldh [rSC], a
    ldh a, [hJoyPressed]
    bit PADB_START, a
    jr z, .checkDemoEnded
    ld a, $33
    ldh [rSB], a
    ld a, $81
    ldh [rSC], a
    ld a, $06
    ldh [hGameState], a
    ret

.checkDemoEnded
    ld hl, $FFB0        ; Number of blocks played in demos or multiplayer TODO
    ldh a, [hDemoNumber]
    cp a, $02
    ld b, $10
    jr z, .skip
    ld b, $1D
.skip
    ld a, [hl]
    cp b
    ret nz
    ld a, $06
    ldh [hGameState], a
    ret

INCBIN "baserom.gb", $542, $A98 - $542

; Todo name, TODO has something to do with serial communication?
DelayMillisecond::
    push bc
    ld b, $FA           ; Spin 240 times 4 cycli, about a millisecond
.loop
    ld b, b
    dec b
    jr nz, .loop
    pop bc
    ret

INCBIN "baserom.gb", $AA1, $1444 - $AA1

GameState_08::
    ld a, IEF_VBLANK
    ldh [rIE], a
    xor a
    ldh [rSB], a
    ldh [rSC], a
    ldh [rIF], a
    call DisableLCD
    call Call_27AD
    ld de, $4CD7
    call LoadTilemap9800
    call ClearSprites
    ld hl, $C200
    ld de, Data_26CF
    ld c, 2
    call Call_1776
    ld de, $C201        ; Metasprite Y-coordinate
    call PositionMusicTypeMetasprite
    ldh a, [hGameType]
    ld e, $12           ; LOW(C212), metasprite X-coordinate
    ld [de], a
    inc de
    cp a, $37
    ld a, $1C           ; A-Type metasprite
    jr z, .skip
    ld a, $1D           ; B-Type metasprite
.skip
    ld [de], a
    call $2671          ; TODO XXX This sets up the metasprites
    call SwitchMusic
    ld a, $D3
    ldh [rLCDC], a
    ld a, $0E
    ldh [hGameState], a
    ret

PositionMusicTypeMetasprite::
    ld a, $01           ; Menu selection SFX
    ld [$DFE0], a       ; TODO I don't think this plays for some reason though
    ldh a, [hMusicType]
    push af
    sub a, $1C          ; The four music types have consecutive metasprite numbers
    add a
    ld c, a
    ld b, $00
    ld hl, MusicTypeMetaspriteCoordinates
    add hl, bc
    ldi a, [hl]
    ld [de], a
    inc de
    ld a, [hl]
    ld [de], a
    inc de
    pop af
    ld [de], a
    ret

MusicTypeMetaspriteCoordinates::
    db $70, $37
    db $70, $77
    db $80, $37
    db $80, $77

; Todo comment on the conditional jumps
GameState_0F::
    ld de, $C200        ; Music Type metasprite
    call Call_1766      ; Read joypad into B and blink cursor
    ld hl, hMusicType
    ld a, [hl]
    bit PADB_START, b
    jp nz, GameState_0E.pressedStart
    bit PADB_A, b       ; TODO name
    jp nz, GameState_0E.pressedStart
    bit PADB_B, b
    jr nz, .pressedB
.handleDPad
    inc e
    bit PADB_RIGHT, b
    jr nz, .pressedRight
    bit PADB_LEFT, b
    jr nz, .pressedLeft
    bit PADB_UP, b
    jr nz, .pressedUp
    bit PADB_DOWN, b
    jp z, GameState_0E.setupMetasprite  ; Bug! A jump to .out is exactly the
    cp a, $1E                           ; same and would have saved 2 bytes
    jr nc, .out
    add a, $02
.updateCursor
    ld [hl], a
    call PositionMusicTypeMetasprite
    call SwitchMusic
.out
    call $2671
    ret

.pressedUp
    cp a, $1E
    jr c, .out
    sub a, $02
    jr .updateCursor

.pressedRight
    cp a, $1D
    jr z, .out
    cp a, $1F
    jr z, .out
    inc a
    jr .updateCursor

.pressedLeft
    cp a, $1C
    jr z, .out
    cp a, $1E
    jr z, .out
    dec a
    jr .updateCursor

.pressedB
    push af
    ldh a, [hIsMultiplayer] ; In multiplayer there is no game type to select
    and a
    jr z, .returnToGameTypeSelection
    pop af
    jr .handleDPad

.returnToGameTypeSelection
    pop af
    ld a, $0E
    jr GameState_0E.nextState

SwitchMusic::
    ldh a, [hMusicType]
    sub a, $17          ; Based on metasprite number
    cp a, $08
    jr nz, .skip
    ld a, $FF           ; Disable music
.skip
    ld [$DFE8], a
    ret

GameState_0E::
    ld de, $C210        ; Game type metasprite
    call Call_1766      ; Read joypad into B and blink cursor
    ld hl, hGameType
    ld a, [hl]
    bit PADB_START, b
    jr nz, .pressedStart
    bit PADB_A, b
    jr nz, .pressedA
    inc e
    inc e               ; Metasprite X coordinate
    bit PADB_RIGHT, b
    jr nz, .pressedRight
    bit PADB_LEFT, b
    jr z, .setupMetasprite
    cp a, $37           ; A-Type
    jr z, .setupMetasprite
    ld a, $37
    ld b, $1C           ; A-Type metasprite
    jr .switchGameType

.pressedRight
    cp a, $77           ; B-Type
    jr z, .setupMetasprite
    ld a, $77
    ld b, $1D           ; B-Type metasprite
.switchGameType
    ld [hl], a
    push af
    ld a, $01
    ld [$DFE0], a       ; SFX TODO
    pop af
    ld [de], a
    inc de
    ld a, b
.updateCursorPosition
    ld [de], a
.setupMetasprite        ; TODO name
    call $2671
    ret

.pressedStart
    ld a, $02
    ld [$DFE0], a
    ldh a, [hGameType]
    cp a, $37
    ld a, $10
    jr z, .nextState
    ld a, $12
.nextState
    ldh [hGameState], a
    xor a
    jr .updateCursorPosition

.pressedA
    ld a, $0F
    jr .nextState

INCBIN "baserom.gb", $157B, $1766 - $157B

Call_1766::
    ldh a, [hJoyPressed]
    ld b, a
    ldh a, [hTimer1]
    and a
    ret nz
    ld a, $10
    ldh [hTimer1], a
    ld a, [de]
    xor a, $80
    ld [de], a
    ret

; Init C metasprites from adresses starting at DE to HL
Call_1776::
.loop
    push hl
    ld b, 6             ; 6 datapoints per metasprite, but what are they...
.innerLoop
    ld a, [de]
    ldi [hl], a
    inc de
    dec b
    jr nz, .innerLoop
    pop hl
    ld a, $10
    add l
    ld l, a
    dec c
    jr nz, .loop
    ld [hl], $80        ; Make the next sprite invisible?
    ret

ClearSprites::
    xor a
    ld hl, wOAMBuffer
    ld b, 4 * 40
.loop
    ldi [hl], a
    dec b
    jr nz, .loop
    ret

INCBIN "baserom.gb", $1795, $26CF - $1795
; First config screen metasprites
Data_26CF::
    db $00, $70, $37, $1C, $00, $00
    db $00, $38, $37, $1C, $00, $00

INCBIN "baserom.gb", $26DB, $2795 - $26DB

ClearTilemap9800::
    ld hl, _SCRN0 + $400 - 1    ; TODO constants
    ld bc, $400
.loop
    ld a, " "
    ldd [hl], a
    dec bc
    ld a, b
    or c
    jr nz, .loop
    ret

; Copy BC bytes from HL to DE
CopyData::
.loop
    ldi a, [hl]
    ld [de], a
    inc de
    dec bc
    ld a, b
    or c
    jr nz, .loop
    ret

Call_27AD::
    call LoadFontTileset
    ld bc, $00A0
    call CopyData
    ld hl, $323F
    ld de, $8300
    ld bc, $0D00
    call CopyData
    ret

LoadFontTileset::
    ld hl, $415F        ; Todo
    ld bc, $0138
    ld de, $8000
.loop
    ldi a, [hl]
    ld [de], a
    inc de
    ld [de], a          ; Expand 1BPP image into 2BPP
    inc de
    dec bc
    ld a, b
    or c
    jr nz, .loop
    ret

LoadCopyrightScreenTileset::
    call LoadFontTileset
    ld bc, $0DA0        ; Bug? This seems way too much, copying garbage data
    call CopyData
    ret

Call_27E1::
    ld bc, $1000
    ld de, $8000
    call CopyData
    ret

; Todo name
LoadTilemap9800::
    ld hl, $9800
    ld b, SCRN_Y_B
.loop
    push hl
    ld c, SCRN_X_B
.innerLoop
    ld a, [de]
    ldi [hl], a
    inc de
    dec c
    jr nz, .innerLoop
    pop hl
    push de
    ld de, SCRN_VX_B
    add hl, de
    pop de
    dec b
    jr nz, .loop
    ret

Call_2804::
    ld b, $0A
    push hl
.loop
    ld a, [de]
    cp a, $FF
    jr z, .skip
    ldi [hl], a
    inc de
    dec b
    jr nz, .loop
    pop hl
    push de
    ld de, $0020
    add hl, de
    pop de
    jr Call_2804

.skip
    pop hl
    ld a, $02
    ldh [$E3], a
    ret

; Disabling the LCD *must* be performed during VBlank only, to prevent damaging
; the hardware
DisableLCD::
    ldh a, [rIE]
    ldh [$A1], a
    res 0, a            ; Bit 0 is VBlank
    ldh [rIE], a
.wait
    ldh a, [rLY]
    cp a, $91           ; TODO
    jr nz, .wait
    ldh a, [rLCDC]
    and a, $FF ^ LCDCF_ON
    ldh [rLCDC], a
    ldh a, [$A1]
    ldh [rIE], a
    ret

INCBIN "baserom.gb", $2839, $29A6 - $2839

ReadJoypad::
    ld a, P1F_GET_DPAD
    ldh [rP1], a
REPT 4
    ldh a, [rP1]        ; Read multiple times to mitigate switch bounce
ENDR
    cpl                 ; 0 = pressed
    and a, $0F
    swap a              ; Store the directional keys in the upper nibble
    ld b, a
    ld a, P1F_GET_BTN
    ldh [rP1], a
REPT 10
    ldh a, [rP1]
ENDR
    cpl
    and a, $0F
    or b
    ld c, a
    ldh a, [hJoyHeld]
    xor c
    and c
    ldh [hJoyPressed], a
    ld a, c
    ldh [hJoyHeld], a
    ld a, P1F_GET_NONE
    ldh [rP1], a
    ret

Call_29E3::
    ldh a, [$FFB2]
    sub a, $10
    srl a
    srl a
    srl a
    ld de, $0000
    ld e, a
    ld hl, $9800        ; TODO
    ld b, $20
.loop
    add hl, de
    dec b
    jr nz, .loop
    ldh a, [$B3]
    sub a, $08
    srl a
    srl a
    srl a
    ld de, $0000
    ld e, a
    add hl, de
    ld a, h
    ldh [$FFB5], a
    ld a, l
    ldh [$FFB4], a
    ret

; This exactly mirrors some curious code in Super Mario Land
; TODO 
; Unused?
Call_2A10::
    ldh a, [$B5]
    ld d, a
    ldh a, [$B4]
    ld e, a
    ld b, $04
.loop
    rr d
    rr e
    dec b
    jr nz, .loop
    ld a, e
    sub a, $84
    and a, $FE
    rlca
    rlca
    add a, $08
    ldh [$B2], a
    ldh a, [$B4]
    and a, $1F
    rla
    rla
    rla
    add a, $08
    ldh [$B3], a
    ret

; Draw the number at DE to the tilemap at HL
; Here, FFE0 doubles as a Boolean indicating the number has started
PrintScore::
    ldh a, [$E0]
    and a
    ret z
    ld c, 3             ; Maximum 3 number pairs
    xor a
    ldh [$E0], a
.printDigitPair
    ld a, [de]
    ld b, a
    swap a
    and a, $0F          ; More significant digit of the pair
    jr nz, .startNumber1
    ldh a, [$E0]        ; If zero, check if the number has already started
    and a
    ld a, "0"           ; Print a zero if it has started
    jr nz, .printFirstDigit
    ld a, " "           ; Otherwise, print a space
.printFirstDigit
    ldi [hl], a
    ld a, b
    and a, $0F          ; Again for the lesser significant digit
    jr nz, .startNumber2
    ldh a, [$E0]
    and a
    ld a, "0"
    jr nz, .printSecondDigit
    ld a, 1             ; If we're at the ones, print a zero even if the number
    cp c                ; hasn't started yet
    ld a, "0"
    jr z, .printSecondDigit
    ld a, " "
.printSecondDigit
    ldi [hl], a
    dec e
    dec c               ; Next (less significant) digit pair
    jr nz, .printDigitPair
    xor a
    ldh [$E0], a
    ret

.startNumber1
    push af
    ld a, 1
    ldh [$E0], a
    pop af
    jr .printFirstDigit

.startNumber2
    push af
    ld a, 1
    ldh [$E0], a
    pop af
    jr .printSecondDigit

; TODO Use rgbds's LOAD block
DMARoutine::
    ld a, HIGH(wOAMBuffer)
    ldh [rDMA], a
    ld a, $28
.wait
    dec a
    jr nz, .wait
    ret
DMARoutineEnd:

INCBIN "baserom.gb", $2A89, $4000 - $2A89
INCBIN "baserom.gb", $4000, $4A07 - $4000

; TODO
CopyrightScreenTilemap::
db "                    "
db "”tm and ©1987 elorg,"
db " tetris licensed to "
db "    bullet-proof    "
db "    software and    "
db "   sub-licensed to  "
db "      nintendo.     "
db "                    "
db " ©1989 bullet-proof "
db "      software.     "
db "   ©", $30, $31, $32, $31, " ", $34, $35, $36, $37, $38, $39, "     "
db "                    "
db "all rights reserved."
db "                    "
db "  original concept, "
db " design and program "
db "by alexey pazhitnov.”"
db "                    "

INCBIN "baserom.gb", $4B6F, $8000 - $4B6F

; vim: set expandtab tabstop=4 shiftwidth=4 
