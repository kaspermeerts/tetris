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
    call .tableJump ; To prepare the return address for the upcoming RST $28
    ld a, 1
    ldh [hSerialInterruptTriggered], a
    pop bc
    pop de
    pop hl
    pop af
    reti

.tableJump
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
    jr z, .label_199
    ldh a, [$CB]
    cp a, $29
    jr nz, .label_199
    xor a
    ldh [$CE], a
    ldh a, [$CF]
    ldh [rSB], a
    ld hl, rSC          ; Why?
    ld [hl], $81        ; Missing in hardware.inc -_-
.label_199
    call $21E0
    call PlayingFieldWipe19 ; What on God's green earth is this???
    call PlayingFieldWipe18
    call PlayingFieldWipe17
    call PlayingFieldWipe16
    call PlayingFieldWipe15
    call PlayingFieldWipe14
    call PlayingFieldWipe13
    call PlayingFieldWipe12
    call PlayingFieldWipe11
    call PlayingFieldWipe10
    call PlayingFieldWipe09
    call PlayingFieldWipe08
    call PlayingFieldWipe07
    call PlayingFieldWipe06
    call PlayingFieldWipe05
    call PlayingFieldWipe04
    call PlayingFieldWipe03
    call PlayingFieldWipe02
    call $1ED7
    call hDMARoutine
    call Call_18CA
    ld a, [$C0CE]       ; Score needs updating?
    and a
    jr z, .label_1FB
    ldh a, [$98]
    cp a, 3
    jr nz, .label_1FB
    ld hl, _SCRN0 + SCRN_VX_B * 3 + 13
    call Call_243B
    ld a, 1
    ldh [$E0], a
    ld hl, _SCRN1 + SCRN_VX_B * 3 + 13
    call Call_243B
    xor a
    ld [$C0CE], a
.label_1FB
    ld hl, hFrameCounter
    inc [hl]
    xor a
    ldh [rSCX], a       ; Unnecessary, but whatever
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
    ld hl, $D000 + $1000 - 1    ; End of upper work RAM bank
    ld c, $10                   ; Why only the upper bank? Bug? Or is this so
    ld b, $00                   ; soft-resetting doesn't erase the top scores?
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
    ld sp, $CFFF        ; TODO top of RAM

    xor a
    ld hl, $DFFF        ; Bug? Clears the upper 256 bytes of the upper work
    ld b, 0             ; RAM bank a second time...
.clearWRAM1loop2
    ldd [hl], a
    dec b
    jr nz, .clearWRAM1loop2

    ld hl, $C000 + $1000 - 1    ; End of work RAM bank 0
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
    call .dispatch      ; This sets up the return address for the upcoming jumptable
    call $7FF0          ; Update sound

    ldh a, [hJoyHeld]
    and a, PADF_START | PADF_SELECT | PADF_B | PADF_A
    cp a, PADF_START | PADF_SELECT | PADF_B | PADF_A
    jp z, Init.softReset
    ld hl, hTimer1
    ld b, 2             ; Two timers, just like Super Mario Land, but only one
.decrementTimer         ; is ever used, just like Super Mario Land
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
.wait                   ; Bug? There should be a HALT instruction here
    ldh a, [hVBlankInterruptTriggered]
    and a
    jr z, .wait
    xor a
    ldh [hVBlankInterruptTriggered], a
    jp MainLoop

.dispatch
    ldh a, [hGameState]
    rst $28

dw $1BCE ; 0x00 Normal gameplay
dw GameState_01 ; Init game over
dw $1244 ; 0x02 Buran liftoff
dw $127B ; 0x03 Buran main engine ignition
dw $1D06 ; 0x04 Game over screen
dw $1D26 ; 0x05 Type B victory jingle
dw GameState_06 ; Init title screen
dw GameState_07 ; Title screen
dw GameState_08 ; Init Game Type/Music Type Selection Screen
dw $148C ; 0x09 just points to a random RET... what?
dw GameState_0A ; Init game?
dw $1DC0 ; 0x0B Init Type B scoreboard
dw $1F16 ; 0x0C
dw GameState_0D ; Game over curtain
dw GameState_0E ; Select Game Type
dw GameState_0F ; Select Music Type
dw GameState_10 ; Init Type A difficulty selection
dw GameState_11 ; Type A level selection
dw GameState_12 ; Init Type B difficulty selection
dw GameState_13 ; Type B level selection
dw GameState_14 ; Type B high selection
dw GameState_15 ; Entering topscore for either game type
dw $0677 ; 0x16 Init 2P game difficulty selection
dw $072C ; 0x17 Select 2P game high
dw $0825 ; 0x18 Init 2P game
dw $08E4 ; 0x19 Init 2P game (2x)
dw $0B31 ; 0x1A 2P game
dw $0CEB ; 0x1B 2P end of game jingle?
dw $0AD2 ; 0x1C Prepare garbage?
dw $0D32 ; 0x1D Init 2P victory screen?
dw $0E23 ; 0x1E Init 2P defeat screen?
dw $1112 ; 0x1F Init 2P game (3x)
dw $0D99 ; 0x20 2P victory screen
dw $0E8A ; 0x21 2P defeat screen
dw $1DCE ; 0x22 Type B victory
dw $1E41 ; 0x23 Dancers
dw GameState_24 ; Init copyright screen
dw GameState_25 ; Copyright screen
dw $1167 ; 0x26 End of dance
dw $11E6 ; 0x27 Prepare Buran launch
dw $11FC ; 0x28 Buran ignition
dw $121C ; 0x29 Buran ignition for real this time
dw $05C7 ; 0x2A Init 2P music selection?
dw $05F7 ; 0x2B 2P Select music
dw $12B3 ; 0x2C Print congratulations
dw $1305 ; 0x2D Congratulations
dw $1324 ; 0x2E Init rocket?
dw $1351 ; 0x2F Rocket
dw $1367 ; 0x30 Rocket ignition
dw $137E ; 0x31 Rocket liftoff
dw $13B5 ; 0x32 Rocket main engine fire
dw $13E5 ; 0x33 End of bonus scene
dw $131B ; 0x34 Game over screen leading to bonus ending
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
    ldh [hTopScorePointer], a   ; TODO
    ldh [$9F], a
    ldh [hWipeCounter], a
    ldh [hNewTopScore], a
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
    ldh  a, [$E4]
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
    call ReadJoypadAndBlinkCursor
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
    call ReadJoypadAndBlinkCursor
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
    cp a, $37           ; Type A
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

; Init Type A difficulty selection screen
GameState_10::
    call DisableLCD
    ld de, $4E3F
    call LoadTilemap9800
    call Call_18FC
    call ClearSprites
    ld hl, $C200
    ld de, Data_26DB
    ld c, 1
    call Call_1776
    ld de, $C201
    ldh a, [hTypeALevel]
    ld hl, Data_1615
    call $174E
    call $2671
    call Call_1795
    call Call_18CA
    ld a, $D3
    ldh [rLCDC], a
    ld a, $11
    ldh [hGameState], a
    ldh a, [hNewTopScore]
    and a
    jr nz, .enterTopScoreState
    call SwitchMusic
    ret

.enterTopScoreState
    ld a, $15
.nextState
    ldh [hGameState], a
    ret

GameState_11::
    ld de, $C200
    call ReadJoypadAndBlinkCursor
    ld hl, hTypeALevel
    ld a, $0A           ; Init gameplay state
    bit PADB_START, b
    jr nz, GameState_10.nextState
    bit PADB_A, b
    jr nz, GameState_10.nextState
    ld a, $08           ; Init Game/Music Type selection screen
    bit PADB_B, b
    jr nz, GameState_10.nextState
    ld a, [hl]
    bit PADB_RIGHT, b
    jr nz, .pressedRight
    bit PADB_LEFT, b
    jr nz, .pressedLeft
    bit PADB_UP, b
    jr nz, .pressedUp
    bit PADB_DOWN, b
    jr z, .setupMetasprite
    cp a, 5             ; Levels 0-4 make up the top row
    jr nc, .setupMetasprite
    add a, 5
    jr .updateCursor

.pressedRight
    cp a, 9             ; 9 is the highest selectable level
    jr z, .setupMetasprite
    inc a
.updateCursor
    ld [hl], a
    ld de, $C201        ; Metasprite 0's Y-coordinate
    ld hl, Data_1615
    call Call_174E
    call Call_1795
.setupMetasprite        ; Name?
    call $2671
    ret

.pressedLeft
    and a
    jr z, .setupMetasprite
    dec a
    jr .updateCursor

.pressedUp
    cp a, 5
    jr c, .setupMetasprite
    sub a, 5
    jr .updateCursor

; Y and X coordinates of the cursor for various levels
Data_1615::
    db $40, $30
    db $40, $40
    db $40, $50
    db $40, $60
    db $40, $70
    db $50, $30
    db $50, $40
    db $50, $50
    db $50, $60
    db $50, $70

; Init Type B difficulty selection screen
GameState_12::
    call DisableLCD
    ld de, $4FA7
    call LoadTilemap9800
    call ClearSprites
    ld hl, $C200
    ld de, Data_26E1
    ld c, 2
    call Call_1776
    ld de, $C201
    ldh a, [hTypeBLevel]
    ld hl, $16D2
    call Call_174E
    ld de, $C211
    ldh a, [hTypeBHigh]
    ld hl, Data_1741
    call Call_174E
    call $2671
    call $17AF
    call Call_18CA
    ld a, $D3
    ldh [rLCDC], a
    ld a, $13
    ldh [hGameState], a
    ldh a, [hNewTopScore]
    and a
    jr nz, .enterTopScoreState
    call SwitchMusic
    ret

.enterTopScoreState
    ld a, $15
    ldh [hGameState], a
    ret

; TODO XXX
Call_1675::
    ldh [hGameState], a
    xor a
    ld [de], a
    ret

GameState_13::
    ld de, $C200
    Call ReadJoypadAndBlinkCursor
    ld hl, hTypeBLevel
    ld a, $0A           ; Init gameplay state
    bit PADB_START, b
    jr nz, Call_1675
    ld a, $14           ; Select Type B high
    bit PADB_A, b
    jr nz, Call_1675
    ld a, $08           ; Back to Type/Music selection screen
    bit PADB_B, b
    jr nz, Call_1675
    ld a, [hl]
    bit PADB_RIGHT, b
    jr nz, .pressedRight
    bit PADB_LEFT, b
    jr nz, .pressedLeft
    bit PADB_UP, b
    jr nz, .pressedUp
    bit PADB_DOWN, b
    jr z, .setupMetasprite
    cp a, $05
    jr nc, .setupMetasprite
    add a, $05
    jr .updateCursor

.pressedRight
    cp a, 9
    jr z, .setupMetasprite
    inc a
.updateCursor
    ld [hl], a
    ld de, $C201
    ld hl, Data_16D2
    call Call_174E
    call $17AF
.setupMetasprite
    call $2671
    ret

.pressedLeft
    and a
    jr z, .setupMetasprite
    dec a
    jr .updateCursor

.pressedUp
    cp a, 5
    jr c, .setupMetasprite
    sub a, 5
    jr .updateCursor

Data_16D2::
    db $40, $18
    db $40, $28
    db $40, $38
    db $40, $48
    db $40, $58
    db $50, $18
    db $50, $28
    db $50, $38
    db $50, $48
    db $50, $58

; TODO XXX Seriously!?
Call_16E6::
    ldh [hGameState], a
    xor a
    ld [de], a
    ret

GameState_14::
    ld de, $C210
    call ReadJoypadAndBlinkCursor
    ld hl, hTypeBHigh
    ld a, $0A           ; Init gameplay state
    bit PADB_START, b
    jr nz, Call_16E6
    bit PADB_A, b
    jr nz, Call_16E6
    ld a, $13           ; Back to level selection
    bit PADB_B, b
    jr nz, Call_16E6
    ld a, [hl]
    bit PADB_RIGHT, b
    jr nz, .pressedRight
    bit PADB_LEFT, b
    jr nz, .pressedLeft
    bit PADB_UP, b
    jr nz, .pressedUp
    bit PADB_DOWN, b
    jr z, .setupMetasprite
    cp a, 3
    jr nc, .setupMetasprite
    add a, 3
    jr .updateCursor

.pressedRight
    cp a, $05
    jr z, .setupMetasprite
    inc a
.updateCursor
    ld [hl], a
    ld de, $C211
    ld hl, Data_1741
    call Call_174E
    call $17AF
.setupMetasprite
    call $2671
    ret

.pressedLeft
    and a
    jr z, .setupMetasprite
    dec a
    jr .updateCursor

.pressedUp
    cp a, 3
    jr c, .setupMetasprite
    sub a, 3
    jr .updateCursor

Data_1741::
    db $40, $70
    db $40, $80
    db $40, $90
    db $50, $70
    db $50, $80
    db $50, $90

    db $00              ; XXX TODO

Call_174E::
    push af
    ld a, $01
    ld [$DFE0], a
    pop af
    push af
    add a
    ld c, a
    ld b, $00
    add hl, bc
    ldi a, [hl]
    ld [de], a
    inc de
    ld a, [hl]
    ld [de], a
    inc de
    pop af
    add a, $20
    ld [de], a
    ret

ReadJoypadAndBlinkCursor::
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

Call_1795::
    call Call_18FC
    ldh a, [hTypeALevel]
    ld hl, $D654
    ld de, 3 * (6 + 3)  ; 3 bytes for the score, 6 for the name, 3 scores per level
.loopLevel
    and a
    jr z, .label_17A7
    dec a
    add hl, de
    jr .loopLevel

.label_17A7
    inc hl
    inc hl
    push hl
    pop de
    call Call_1800
    ret

Call_17AF::
    call Call_18FC
    ldh a, [hTypeBLevel]
    ld hl, $D000
    ld de, 3 * 6 * (6 + 3)
.loopLevel
    and a
    jr z, .high
    dec a
    add hl, de
    jr .loopLevel

.high
    ldh a, [hTypeBHigh]
    ld de, $001B
.loopHigh
    and a
    jr z, .label_17CD
    dec a
    add hl, de
    jr .loopHigh

.label_17CD
    inc hl
    inc hl
    push hl
    pop de
    call Call_1800
    ret

; Print a 3 digit BCD number from HL to DE
; Different from PrintScore in that it doesn't print leading spaces, but
; simply skips over those tiles. Bug
PrintTopScore::
    ld b, 3             ; 3 digit pairs
.loop
    ld a, [hl]
    and a, $F0
    jr nz, .printFirstDigit
    inc e
    ldd a, [hl]
    and a, $0F
    jr nz, .printSecondDigit
    inc e
    dec b
    jr nz, .loop        ; Loop until past the non-printed leading zeroes
    ret

.printFirstDigit
    ld a, [hl]
    and a, $F0
    swap a
    ld [de], a
    inc e
    ldd a, [hl]
    and a, $0F
.printSecondDigit
    ld [de], a
    inc e
    dec b
    jr nz, .printFirstDigit
    ret

; Copy 3 (three) bytes from HL to DE
CopyThreeBytes::        ; Lol. Bug. TODO, name?
    ld b, 3
CopyBBytes::            ; TODO
.loop
    ldd a, [hl]
    ld [de], a
    dec de
    dec b
    jr nz, .loop
    ret

Call_1800::
    ld a, d
    ldh [hTopScorePointer], a       ; Big-endian? Even though the Game Boy is
    ld a, e                         ; itself little-endian? Bug
    ldh [hTopScorePointer + 1], a
    ld c, 3             ; Three scores to check against
.checkScoreAgainstTopScores
    ld hl, wScore + 2
    push de
    ld b, 3             ; Three digit pairs
.checkDigitPair
    ld a, [de]
    sub [hl]
    jr c, .newTopScore
    jr nz, .tryNextTopScore
    dec l               ; Should be DEC HL? Bug? Could this ever be the case?
    dec de
    dec b
    jr nz, .checkDigitPair
.tryNextTopScore
    pop de
    inc de              ; Three bytes (digit pairs) per score
    inc de
    inc de
    dec c
    jr nz, .checkScoreAgainstTopScores
    jr .printTopScores

.newTopScore
    pop de
    ldh a, [hTopScorePointer]
    ld d, a
    ldh a, [hTopScorePointer + 1]
    ld e, a
    push de
    push bc
    ld hl, 6
    add hl, de
    push hl
    pop de
    dec hl
    dec hl
    dec hl
.shiftScoresLoop
    dec c
    jr z, .insertNewTopScore
    call CopyThreeBytes ; To make room for the new top score, shift the lower
    jr .shiftScoresLoop  ; score one down.

.insertNewTopScore
    ld hl, wScore + 2
    ld b, 3
.copyScoreLoop
    ldd a, [hl]
    ld [de], a
    dec e
    dec b
    jr nz, .copyScoreLoop
    pop bc
    pop de
    ld a, c
    ldh [$C8], a
    ld hl, $12
    add hl, de
    push hl
    ld de, 6
    add hl, de
    push hl
    pop de
    pop hl
.shiftNamesLoop
    dec c
    jr z, .label_1862
    ld b, 6
    Call CopyBBytes     ; Similarly, shift the name one down
    jr .shiftNamesLoop

.label_1862
    ld a, $60           ; TODO charmap
    ld b, 5
.label_1866
    ld [de], a
    dec de
    dec b
    jr nz, .label_1866
    ld a, "a"           ; Initial value?
    ld [de], a
    ld a, d
    ldh [$C9], a
    ld a, e
    ldh [$CA], a
    xor a
    ldh [$9C], a
    ldh [$C6], a
    ld a, $01
    ld [$DFE8], a
    ldh [hNewTopScore], a
.printTopScores
    ld de, $C9AC
    ldh a, [hTopScorePointer]
    ld h, a
    ldh a, [hTopScorePointer + 1]
    ld l, a
    ld b, 3
.printNextScore
    push hl             ; What the fuck is this garbage
    push de
    push bc
    call PrintTopScore
    pop bc
    pop de
    ld hl, $20          ; TODO
    add hl, de
    push hl
    pop de
    pop hl
    push de
    ld de, 3            ; Three bytes per score
    add hl, de
    pop de
    dec b
    jr nz, .printNextScore
    dec hl
    dec hl
    ld b, 3
    ld de, $C9A4
.printNextName
    push de
    ld c, 6             ; Six characters per name
.printNameLoop
    ldi a, [hl]
    and a               ; Names are zero-delimited
    jr z, .nextName
    ld [de], a
    inc de
    dec c
    jr nz, .printNameLoop
.nextName
    pop de
    push hl
    ld hl, $20
    add hl, de
    push hl
    pop de
    pop hl
    dec b
    jr nz, .printNextName
    call $2651
    ld a, $01
    ldh [$E8], a
    ret

; Draw the top scores from the buffer?
Call_18CA::
    ldh a, [$E8]        ; Signals something needs to be redrawn? Or just this?
    and a
    ret z
    ld hl, $99A4
    ld de, $C9A4
    ld c, 6             ; 3 columns with two fields
.columnLoop
    push hl
.fieldLoop
    ld b, 6
.rowLoop
    ld a, [de]
    ldi [hl], a
    inc e
    dec b
    jr nz, .rowLoop
    inc e               ; Two spaces between name and score
    inc l
    inc e
    inc l
    dec c
    jr z, .out
    bit 0, c
    jr nz, .fieldLoop
    pop hl
    ld de, $0020        ; TODO constant
    add hl, de
    push hl
    pop de
    ld a, $30
    add d
    ld d, a
    jr .columnLoop

.out
    pop hl
    xor a
    ldh [$E8], a
    ret

Call_18FC::
    ld hl, $C9A4
    ld de, $0020
    ld a, $60
    ld c, 3
.columnLoop
    ld b, 14            ; Two fields of 6 characters, with 2 spaces in between
    push hl
.rowLoop
    ldi [hl], a
    dec b
    jr nz, .rowLoop
    pop hl
    add hl, de
    dec c
    jr nz, .columnLoop
    ret

GameState_15::
    ldh a, [$C8]        ; Something to do with topscores?
    ld hl, $9800 + $20 * 15 + 4 ; TODO
    ld de, -$20
.loop
    dec a
    jr z, .break
    add hl, de
    jr .loop

.break
    ldh a, [$C6]        ; X-coordinate of topscore cursor, reused for timer
    ld e, a             ; purposes?
    ld d, $00
    add hl, de
    ldh a, [$C9]        ; Hi and Lo byte of topscore address?
    ld d, a
    ldh a, [$CA]
    ld e, a
    ldh a, [hTimer1]
    and a
    jr nz, .readJoypad
    ld a, 7
    ldh [hTimer1], a
    ldh a, [$9C]        ; Used for blinking the cursor
    xor a, 1
    ldh [$9C], a
    ld a, [de]
    jr z, .printCharacterAtCursor
    ld a, " "           ; When blinking, print an empty space
.printCharacterAtCursor
    call PrintCharacter
.readJoypad
    ldh a, [hJoyPressed]
    ld b, a
    ldh a, [hJoyHeld]
    ld c, a
    ld a, $17
    bit PADB_UP, b
    jr nz, .pressedUp
    bit PADB_UP, c
    jr nz, .holdingUp
    bit PADB_DOWN, b
    jr nz, .pressedDown
    bit PADB_DOWN, c
    jr nz, .holdingDown
    bit PADB_A, b
    jr nz, .pressedA
    bit PADB_B, b
    jp nz, .pressedB
    bit PADB_START, b
    ret z
.submitName             ; TODO
    ld a, [de]
    call PrintCharacter
    call SwitchMusic
    xor a
    ldh [hNewTopScore], a
    ldh a, [hGameType]
    cp a, $37           ; TODO Name, A-Type
    ld a, $11
    jr z, .nextState
    ld a, $13           ; B-Type
.nextState
    ldh [hGameState], a
    ret

.holdingUp
    ldh a, [hKeyRepeatTimer]
    dec a
    ldh [hKeyRepeatTimer], a
    ret nz
    ld a, 9             ; Repeat key every 9 frames
.pressedUp
    ldh [hKeyRepeatTimer], a
    ld b, "×"           ; Either skip to space at this weird ×
    ldh a, [hHeartMode]
    and a
    jr z, .checkSpaceSkip
    ld b, "♥"           ; Or, in heart mode, at the ♥
.checkSpaceSkip
    ld a, [de]          ; Load the current character at the cursor
    cp b
    jr nz, .checkOverflow
    ld a, " " - 1       ; -1 because we fall through to the increment
.nextCharacter
    inc a
.loadCharacter
    ld [de], a
    ld a, $01
    ld [$DFE0], a       ; TODO
    ret

.checkOverflow
    cp a, " "           ; Space is the last selectable character
    jr nz, .nextCharacter
    ld a, "a"           ; Start at the beginning of the alphabet
    jr .loadCharacter

.holdingDown
    ldh a, [hKeyRepeatTimer]
    dec a
    ldh [hKeyRepeatTimer], a
    ret nz
    ld a, 9
.pressedDown
    ldh [hKeyRepeatTimer], a
    ld b, "×"
    ldh a, [hHeartMode]
    and a
    jr z, .checkUnderflow
    ld b, "♥"
.checkUnderflow
    ld a, [de]
    cp a, "a"           ; A is the first character
    jr nz, .wrapToSpace
    ld a, " " + 1       ; +1 because we fall through to the decrement
.previousCharacter
    dec a
    jr .loadCharacter

.wrapToSpace
    cp a, " "
    jr nz, .previousCharacter
    ld a, b
    jr .loadCharacter

.pressedA
    ld a, [de]
    call PrintCharacter ; Print to avoid leaving a space behind when mid-blink
    ld a, $02           ; TODO SFX names
    ld [$DFE0], a
    ldh a, [$C6]
    inc a               ; Move cursor one to the right
    cp a, 6             ; Max 6 letters
    jr z, .submitName
    ldh [$C6], a
    inc de
    ld a, [de]
    cp a, "…"
    jr nz, .movePointer
    ld a, "a"           ; If no letter had been entered yet at this position,
    ld [de], a          ; as evidenced by the ellipsis, start with "A"
.movePointer
    ld a, d
    ldh [$C9], a
    ld a, e
    ldh [$CA], a
    ret

.pressedB
    ldh a, [$C6]
    and a
    ret z               ; Don't go left of the beginning
    ld a, [de]
    call PrintCharacter
    ldh a, [$C6]
    dec a               ; Move cursor one to the left
    ldh [$C6], a
    dec de
    jr .movePointer

PrintCharacter::        ; TODO name? Is this ever reused?
    ld b, a
.waitForHBlank          ; Macro?
    ldh a, [rSTAT]
    and a, %11
    jr nz, .waitForHBlank
    ld [hl], b
    ret

GameState_0A::
    call DisableLCD
    xor a
    ld [$C210], a
    ldh [$98], a
    ldh [$9C], a
    ldh [$9B], a
    ldh [hTopScorePointer], a   ; Why not the upper byte too?
    ldh [$9F], a
    ld a, " "
    call Call_1FD7
    call Call_1FF2
    call $2651
    xor a
    ldh [hWipeCounter], a
    call ClearSprites
    ldh a, [hGameType]
    ld de, $3FF7
    ld hl, hTypeBLevel
    cp a, $77
    ld a, $50
    jr z, .label_1A3F
    ld a, $F1
    ld hl, hTypeALevel
    ld de, $3E8F
.label_1A3F
    push de
    ldh [$E6], a
    ld a, [hl]
    ldh [hLevel], a
    call LoadTilemap9800
    pop de
    ld hl, $9C00        ; TODO
    call $27EE
    ld de, Data_2839
    ld hl, $9C63
    ld c, 10
    call Call_1F7D
    ld h, $98
    ldh a, [$E6]
    ld l, a
    ldh a, [hLevel]
    ld [hl], a
    ld h, $9C
    ld [hl], a
    ldh a, [hHeartMode]
    and a
    jr z, .label_1A71
    inc hl
    ld [hl], "♥"
    ld h, $98
    ld [hl], "♥"
.label_1A71
    ld hl, $C200
    ld de, $26BF
    call $26B6
    ld hl, $C210
    ld de, $26C7
    call $26B6
    ld hl, $9951
    ldh a, [hGameType]
    cp a, $77           ; Type B
    ld a, $25
    jr z, .label_1A8F
    xor a
.label_1A8F
    ldh [hLines], a
    and a, $0F
    ldd [hl], a
    jr z, .label_1A98
    ld [hl], $02
.label_1A98
    call $1AE8
    ld a, [$C0DE]
    and a
    jr z, .label_1AA6
    ld a, $80
    ld [$C210], a
.label_1AA6
    call $2007
    call $2007
    call $2007
    call $2683
    xor a
    ldh [$A0], a
    ldh a, [hGameType]
    cp a, $77
    jr nz, .turnOnLCDAndReturn
    ld a, $34
    ldh [hDropTimer], a
    ldh a, [hTypeBHigh]
    ld hl, $98B0
    ld [hl], a          ; Print the height number somewhere on the right
    ld h, $9C
    ld [hl], a
    and a
    jr z, .turnOnLCDAndReturn
    ld b, a
    ldh a, [hDemoNumber]
    and a
    jr z, .label_1AD6
    call InitTypeBDemoGarbage
    jr .turnOnLCDAndReturn

.label_1AD6
    ld a, b
    ld de, -2 * $20     ; Two rows of garbage per height
    ld hl, $9A02        ; Top left of second row of playing field TODO
    call InitTypeBGarbage
.turnOnLCDAndReturn
    ld a, $D3           ; Urgh todo
    ldh [rLCDC], a
    xor a
    ldh [hGameState], a
    ret

Call_1AE8::
    ldh a, [hLevel]
    ld e, a
    ldh a, [hHeartMode]
    and a
    jr z, .label_1AFA
    ld a, 10
    add e
    cp a, 21
    jr c, .label_1AF9
    ld a, 20            ; Gravity tops out at level 20
.label_1AF9
    ld e, a
.label_1AFA
    ld hl, FramesPerDropTable
    ld d, $00
    add hl, de
    ld a, [hl]
    ldh [hDropTimer], a
    ldh [hFramesPerDrop], a
    ret

FramesPerDropTable::
    db 52               ; Level 0
    db 48
    db 44
    db 40
    db 36
    db 32
    db 27
    db 21
    db 16
    db 10
    db 9                ; Level 10
    db 8
    db 7
    db 6
    db 5
    db 5
    db 4
    db 4
    db 3
    db 3
    db 2                ; Level 20

; For the demo, the garbage can't be random of course
InitTypeBDemoGarbage::
    ld hl, $99C2        ; TODO coordinates macro
    ld de, TypeBDemoGarbage
    ld c, 4             ; 4 rows of garbage
.nextRow
    ld b, 10            ; Playfield width
    push hl
.nextColumn
    ld a, [de]
    ld [hl], a
    push hl
    ld a, h
    add a, $30          ; Switch to playfield buffer?
    ld h, a
    ld a, [de]
    ld [hl], a
    pop hl
    inc l
    inc de
    dec b
    jr nz, .nextColumn
    pop hl
    push de
    ld de, $0020        ; TODO bg map width constant?
    add hl, de
    pop de
    dec c
    jr nz, .nextRow
    ret

TypeBDemoGarbage::
    db $85, $2F, $82, $86, $83, $2F, $2F, $80, $82, $85
    db $2F, $82, $84, $82, $83, $2F, $83, $2F, $87, $2F
    db $2F, $85, $2F, $83, $2F, $86, $82, $80, $81, $2F
    db $83, $2F, $86, $83, $2F, $85, $2F, $85, $2F, $2F

; I'm not being rude here, I think garbage is the official term
InitTypeBGarbage::
    ld b, a
.rowLoop
    dec b
    jr z, .fillLoop
    add hl, de          ; DE is a negative offset, raising the position of the 
    jr .rowLoop         ; HL pointer in the playing field

.fillLoop
    ldh a, [rDIV]       ; The DIV register increments at 16384 Hz, so it's
    ld b, a             ; basically an RNG. The following ridiculous loop
.chooseBlock            ; switches between $80 (meaning a random tetromino
    ld a, $80           ; block) and $2F (meaning an empty space), decrementing
.loop                   ; the random value to zero. Of course, this is equi-
    dec b               ; valent to a 50-50 random chance for either, which
    jr z, .label_1B7F   ; could have been done much more easily with a BIT
    cp a, $80           ; test or an AND, without wasting on average ~1300
    jr nz, .chooseBlock ; cycles every time a new block is picked. As no
    ld a, " "           ; interrupts can fire during this loop, it's completely
    jr .loop            ; deterministic as well.

.label_1B7F
    cp a, " "
    jr z, .drawEmptySpace
    ldh a, [rDIV]       ; Again, the DIV register is used as an RNG
    and a, $07
    or a, $80           ; Tetromino tile numbers range from $80 to $87
    jr .ensureAtLeastOneHole

.drawEmptySpace
    ldh [$A0], a
.ensureAtLeastOneHole
    push af
    ld a, l
    and a, $0F
    cp a, $0B           ; The rightmost cell of the playing field has $B as lowest nibble
    jr nz, .popAndDrawTile
    ldh a, [$A0]
    cp a, " "
    jr z, .popAndDrawTile
    pop af              ; There's a 1 in 512 chance we picked no empty blocks
    ld a, " "           ; at all for this line. In that case, make the rightmost
    jr .drawTile        ; block empty

.popAndDrawTile
    pop af
.drawTile
    ld [hl], a
    push hl
    push af
    ldh a, [hIsMultiplayer]
    and a
    jr nz, .label_1BAD  ; TODO Is the WRAM buffer not used in multiplayer?
    ld de, $3000
    add hl, de
.label_1BAD
    pop af
    ld [hl], a
    pop hl
    inc hl
    ld a, l
    and a, $0F
    cp a, $0C
    jr nz, .fillLoop
    xor a
    ldh [$A0], a
    ld a, h
    and a, $0F          ; The second row from the bottom of the playing field
    cp a, $0A           ; starts at $9A00
    jr z, .secondOrFirstRow
.nextRow
    ld de, $20 - 10
    add hl, de
    jr .fillLoop

.secondOrFirstRow
    ld a, l
    cp a, $2C
    jr nz, .nextRow
    ret

; 1BCE is the normal gameplay gamestate. I'm keeping this for later
INCBIN "baserom.gb", $1BCE, $1CE2 - $1BCE

GameState_01::
    ld a, $80
    ld [$C200], a       ; Active block 
    ld [$C210], a       ; Next block
    call $2683          ; TODO smth to do with metasprites?
    call $2696
    xor a
    ldh [$98], a
    ldh [$9C], a
    call $2293
    ld a, $87           ; This block isn't used in any tetromino
    call Call_1FD7
    ld a, 70            ; 70 frames is 1⅙ seconds
    ldh [hTimer1], a
    ld a, $0D
    ldh [hGameState], a
    ret

INCBIN "baserom.gb", $1D06, $1F1F - $1D06

GameState_0D::
    ldh a, [hTimer1]
    and a
    ret nz
    ld a, $04           ; Game over jingle
    ld [$DFE8], a
    ldh a, [hIsMultiplayer]
    and a
    jr z, .singleplayerGameOver
    ld a, 63            ; Just over a second
    ldh [hTimer1], a
    ld a, $1B
    ldh [hSerialInterruptTriggered], a  ; XXX
    jr .nextState

.singleplayerGameOver
    ld a, " "
    call Call_1FD7      ; Erase playing field
    ld hl, $C843
    ld de, Data_293E
    ld c, 7
    call Call_1F7D      ; Prints game over screen to buffer?
    ld hl, $C983
    ld de, Data_2976
    ld c, 6
    call Call_1F7D
    ldh a, [hGameType]
    cp a, $37           ; Type A
    jr nz, .noBonusEnding
    ld hl, wScore + 2   ; Upper 2 digits of the 6 digit score
    ld a, [hl]
    ld b, $58
    cp a, $20           ; At least 200k points
    jr nc, .bonusEnding
    inc b
    cp a, $15           ; At least 150k
    jr nc, .bonusEnding
    inc b
    cp a , $10          ; At least 100k
    jr nc, .bonusEnding
.noBonusEnding
    ld a, $04           ; Stay on game over screen until START is pressed
.nextState
    ldh [hGameState], a
    ret

.bonusEnding
    ld a, b
    ldh [$F3], a
    ld a, 144           ; 2.4 seconds
    ldh [hTimer1], a
    ld a, $34
    ldh [hGameState], a
    ret

; Auxiliary routine to print text to the playing field
Call_1F7D::
.columnLoop
    ld b, $08
    push hl
.rowLoop
    ld a, [de]
    ldi [hl], a
    inc de
    dec b
    jr nz, .rowLoop
    pop hl
    push de
    ld de, $0020        ; TODO constant
    add hl, de
    pop de
    dec c
    jr nz, .columnLoop
    ret

AddLineClearScore::
    ldh a, [hGameType]
    cp a, $37
    ret nz
    ldh a, [hGameState]
    and a
    ret nz
    ldh a, [hWipeCounter]
    cp a, $05
    ret nz
    ld hl, $C0AC        ; If non-zero, a single was scored, if C0B1 is non-zero
    ld bc, $0005        ; a double was scored, if C0B6 a triple etc...
    ld a, [hl]          ; Very curious...
    ld de, $0040        ; 40 points for a Single
    and a
    jr nz, .awardScore
    add hl, bc
    ld a, [hl]
    ld de, $0100        ; 100 points for a Double
    and a
    jr nz, .awardScore
    add hl, bc
    ld a, [hl]
    ld de, $0300        ; 300 points for a Triple
    and a
    jr nz, .awardScore
    add hl, bc
    ld de, $1200        ; 1200 points for a Tetris
    ld a, [hl]
    and a
    ret z
.awardScore
    ld [hl], $00
    ldh a, [hLevel]
    ld b, a
    inc b               ; Multiply score with level+1
.scoreLoop
    push bc
    push de
    ld hl, wScore
    call AddScore
    pop de
    pop bc
    dec b
    jr nz, .scoreLoop
    ret

Call_1FD7:: ; Fill playing field buffer with A?
    push af
    ld a, $02
    ldh [hWipeCounter], a
    pop af
    ld hl, $C802        ; Top left of playing field, in buffer?
    ld c, 18            ; Playing field height
    ld de, $0020
.columnLoop
    push hl
    ld b, 10            ; Playing field width
.rowLoop
    ldi [hl], a
    dec b
    jr nz, .rowLoop
    pop hl
    add hl, de
    dec c
    jr nz, .columnLoop
    ret

Call_1FF2::             ; no idea
    ld hl, $CBC2
    ld de, $0016
    ld c, 2
    ld a, " "
.columnLoop
    ld b, 10
.rowLoop
    ldi [hl], a
    dec b
    jr nz, .rowLoop
    add hl, de
    dec c
    jr nz, .columnLoop
    ret

Call_2007::
INCBIN "baserom.gb", $2007, $229E - $2007

; Absolute garbage. I wonder if they used a macro...
PlayingFieldWipe02::
    ldh a, [hWipeCounter]
    cp a, 2
    ret nz
    ld hl, $9A22
    ld de, $CA22
    call WipePlayingFieldRow
    ret

PlayingFieldWipe03::
    ldh a, [hWipeCounter]
    cp a, 3
    ret nz
    ld hl, $9A02
    ld de, $CA02
    call WipePlayingFieldRow
    ret

PlayingFieldWipe04::
    ldh a, [hWipeCounter]
    cp a, 4
    ret nz
    ld hl, $99E2
    ld de, $C9E2
    call WipePlayingFieldRow
    ret

PlayingFieldWipe05::
    ldh a, [hWipeCounter]
    cp a, 5
    ret nz
    ld hl, $99C2
    ld de, $C9C2
    call WipePlayingFieldRow
    ret

PlayingFieldWipe06::
    ldh a, [hWipeCounter]
    cp a, 6
    ret nz
    ld hl, $99A2
    ld de, $C9A2
    call WipePlayingFieldRow
    ret

PlayingFieldWipe07::
    ldh a, [hWipeCounter]
    cp a, 7
    ret nz
    ld hl, $9982
    ld de, $C982
    call WipePlayingFieldRow
    ret

; This one also plays the sound of the block tower falling, or when in 2P mode
; a line is added to the field. So not the place to do this, bug
PlayingFieldWipe08::
    ldh a, [hWipeCounter]
    cp a, 8
    ret nz
    ld hl, $9962
    ld de, $C962
    call WipePlayingFieldRow
    ldh a, [hIsMultiplayer]
    and a
    ldh a, [hGameState]
    jr nz, .multiplayer
    and a
    ret nz
.sfxOut
    ld a, $01
    ld [$DFF8], a
    ret

.multiplayer
    cp a, $1A           ; smth smth 2p game
    ret nz
    ldh a, [$D4]
    and a
    jr z, .sfxOut
    ld a, $05
    ld [$DFE0], a
    ret

PlayingFieldWipe09::
    ldh a, [hWipeCounter]
    cp a, 9
    ret nz
    ld hl, $9942
    ld de, $C942
    call WipePlayingFieldRow
    ret

PlayingFieldWipe10::
    ldh a, [hWipeCounter]
    cp a, 10
    ret nz
    ld hl, $9922
    ld de, $C922
    call WipePlayingFieldRow
    ret

PlayingFieldWipe11::
    ldh a, [hWipeCounter]
    cp a, 11
    ret nz
    ld hl, $9902
    ld de, $C902
    call WipePlayingFieldRow
    ret

PlayingFieldWipe12::
    ldh a, [hWipeCounter]
    cp a, 12
    ret nz
    ld hl, $98E2
    ld de, $C8E2
    call WipePlayingFieldRow
    ret

PlayingFieldWipe13::
    ldh a, [hWipeCounter]
    cp a, 13
    ret nz
    ld hl, $98C2
    ld de, $C8C2
    call WipePlayingFieldRow
    ret

PlayingFieldWipe14::
    ldh a, [hWipeCounter]
    cp a, 14
    ret nz
    ld hl, $98A2
    ld de, $C8A2
    call WipePlayingFieldRow
    ret

PlayingFieldWipe15::
    ldh a, [hWipeCounter]
    cp a, 15
    ret nz
    ld hl, $9882
    ld de, $C882
    call WipePlayingFieldRow
    ret

PlayingFieldWipe16::
    ldh a, [hWipeCounter]
    cp a, 16
    ret nz
    ld hl, $9862
    ld de, $C862
    call WipePlayingFieldRow
    call Call_244B
    ret

PlayingFieldWipe17::
    ldh a, [hWipeCounter]
    cp a, 17
    ret nz
    ld hl, $9842
    ld de, $C842
    call WipePlayingFieldRow
    ld hl, $9C6D        ; the score is here in the tilemap visible when paused
    call Call_243B      ; However, for some reason the number of lines is only
    ld a, $01           ; updated when the pause button is actually pressed. Bug?
    ldh [$E0], a        ; Why here of all places as well?
    ret

PlayingFieldWipe18::
    ldh a, [hWipeCounter]
    cp a, 18
    ret nz
    ld hl, $9822
    ld de, $C822
    call WipePlayingFieldRow
    ld hl, $986D
    call Call_243B
    ret

PlayingFieldWipe19::
    ldh a, [hWipeCounter]
    cp a, 19
    ret nz
    ld [$C0C7], a
    ld hl, $9802
    ld de, $C802
    call WipePlayingFieldRow
    xor a
    ldh [hWipeCounter], a
    ldh a, [hIsMultiplayer]
    and a
    ldh a, [hGameState]
    jr nz, .multiplayer
    and a
    ret nz
.printLines
    ld hl, $994E
    ld de, hLines + 1
    ld c, 2             ; Maximum 2 two digit pairs, 9999 lines
    ldh a, [hGameType]
    cp a, $37           ; Type A
    jr z, .print
    ld hl, $9950
    ld de, hLines
    ld c, 1
.print
    call PrintNumber
    ldh a, [hGameType]
    cp a, $37
    jr z, .label_242B
    ldh a, [hLines]
    and a
    jr nz, .label_242B
    ld a, $64
    ldh [hTimer1], a
    ld a, $02
    ld [$DFE8], a
    ldh a, [hIsMultiplayer]
    and a
    jr z, .typeBDone
    ldh [$D5], a
    ret

.typeBDone
    ldh a, [hTypeBLevel]
    cp a, 9             ; Completing level 9 gives a bonus ending
    ld a, $05
    jr nz, .nextState
    ld a, $22
.nextState
    ldh [hGameState], a
    ret

.label_242B
    call $2007
    ret

.multiplayer
    cp a, $1A           ; Normal 2P gameplay
    ret nz
    ldh a, [$D4]
    and a
    jr z, .printLines
    xor a
    ldh [$D4], a
    ret


Call_243B::
    ldh a, [hGameState]
    and a
    ret nz
    ldh a, [hGameType]
    cp a, $37
    ret nz
    ld de, wScore + 2
    call PrintScore
    ret

Call_244B::
    ldh a, [hGameState]
    and a
    ret nz
    ldh a, [hGameType]
    cp a, $37
    ret nz
    ld hl, hLevel
    ld a, [hl]
    cp a, $14
    ret z
    call $249D
    ldh a, [$9F]
    ld d, a
    and a, $F0
    ret nz
    ld a, d
    and a, $0F
    swap a
    ld d, a
    ldh a, [hLines]
    and a, $F0
    swap a
    or d
    cp b
    ret c
    ret z
    inc [hl]
    call Call_249D
    and a, $0F
    ld c, a
    ld hl, $98F1        ; level
.label_247E
    ld [hl], c
    ld h, $9C
    ld [hl], c
    ld a, b
    and a, $F0
    jr z, .label_2494
    swap a
    ld c, a
    ld a, l
    cp a, $F0
    jr z, .label_2494
    ld hl, $98F0
    jr .label_247E

.label_2494
    ld a, $08
    ld [$DFE0], a
    call Call_1AE8
    ret

Call_249D::
    ld a, [hl]
    ld b, a
    and a
    ret z
    xor a
.label_24A2
    or a
    inc a
    daa
    dec b
    jr z, .label_24AA
    jr .label_24A2

.label_24AA
    ld b, a
    ret

WipePlayingFieldRow::
    ld b, 10
.loop
    ld a, [de]
    ld [hl], a          ; Bug? This could definitely be a LDI [HL], A
    inc l
    inc e
    dec b
    jr nz, .loop
    ldh a, [hWipeCounter]
    inc a
    ldh [hWipeCounter], a
    ret

INCBIN "baserom.gb", $24BB, $26CF - $24BB

; First config screen metasprites
Data_26CF::
    db $00, $70, $37, $1C, $00, $00
    db $00, $38, $37, $1C, $00, $00

; Type A difficulty selection metasprite
Data_26DB::
    db $00, $40, $34, $20, $00, $00

; Type B difficulty selection metasprites
Data_26E1::
    db $00, $40, $1C, $20, $00, $00
    db $00, $40, $74, $20, $00, $00

INCBIN "baserom.gb", $26ED, $2795 - $26ED

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
    ld b, 10
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
    ldh [hWipeCounter], a
    ret

; Disabling the LCD *must* be performed during VBlank only,
; to prevent damaging the hardware
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

Data_2839::
    db "  hit   "
    db "  ⋯⋯⋯   "
    db " start  "
    db " ⋯⋯⋯⋯⋯  "
    db "   to   "
    db "   ⋯⋯   "
    db "continue"
    db "⋯⋯⋯⋯⋯⋯⋯⋯"
    db "  game  "
    db "  ⋯⋯⋯⋯  "

INCBIN "baserom.gb", $2889, $293E - $2889
Data_293E::
    db $61, $62, $62, $62, $62, $62, $62, $63
    db $64, "      ", $65
    db $64, " game ", $65
    db $64, " ", $AD, $AD, $AD, $AD, " ", $65
    db $64, " over ", $65
    db $64, " ", $AD, $AD, $AD, $AD, " ", $65
    db $66, $69, $69, $69, $69, $69, $69, $6A

Data_2976::
    db "please  "
    db "⋯⋯⋯⋯⋯⋯  "
    db " try    "
    db " ⋯⋯⋯    "
    db "  again♥"
    db "  ⋯⋯⋯⋯⋯ "

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

PrintScore::            ; Very ugly to do this here
    ldh a, [$E0]
    and a
    ret z
    ld c, 3             ; Score caps out at 999999
; Print C digit pairs, with DE pointing to the most significant pair, to HL
PrintNumber::
    xor a               ; Todo?
    ldh [$E0], a        ; E0 doubles as a boolean we're past the leading zeros?
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

Call_2A89::
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
