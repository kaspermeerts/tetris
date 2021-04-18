INCLUDE "hardware.inc"
INCLUDE "charmap.asm"
INCLUDE "constants.asm"
INCLUDE "macros.asm"

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
    jp InterruptHandlerStub
    ds $50 - @, $FF

SECTION "Interrupt Timer", ROM0[$0050]
    jp InterruptHandlerStub
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
    dw Handshake        ; 0
    dw Call_9F          ; 1
    dw Call_A4          ; 2
    dw Call_BA          ; 3
    dw $27EA            ; XXX Is this used? The target is a simple ret

; A transfer has occurred. If we received the slave code, we're the master, and
; vice versa. If we get back zero, we've kicked the other Game Boy out of the
; demo and it's now back at the title screen
Handshake::
    ldh a, [hGameState]
    cp a, $07           ; Title screen
    jr z, .tryBecomeMaster
    cp a, $06
    ret z
    ld a, $06           ; If not at or going to the title screen, go back to
    ldh [hGameState], a ; the title screen
    ret

.tryBecomeMaster
    ldh a, [rSB]
    cp a, SLAVE
    jr nz, .tryBecomeSlave
    ld a, MASTER        ; Become the master
    ldh [hSerialRole], a
    ld a, $01           ; TODO constant?
    jr .out

.tryBecomeSlave
    cp a, MASTER
    ret nz
    ld a, SLAVE         ; Become the slave
    ldh [hSerialRole], a
    xor a
.out
    ldh [rSC], a
    ret

Call_9F::
    ldh a, [rSB]
    ldh [hSerialRx], a
    ret

Call_A4::
    ldh a, [rSB]
    ldh [hSerialRx], a
    ldh a, [hSerialRole]
    cp a, MASTER
    ret z
    ldh a, [hSerialTx]
    ldh [rSB], a
    ld a, $FF
    ldh [hSerialTx], a
    ld a, SERIAL_TRANSFER_EXTERNAL_CLOCK
    ldh [rSC], a
    ret

Call_BA::
    ldh a, [rSB]
    ldh [hSerialRx], a
    ldh a, [hSerialRole]
    cp a, MASTER
    ret z
    ldh a, [hSerialTx]
    ldh [rSB], a
    ei
    call DelayMillisecond
    ld a, SERIAL_TRANSFER_EXTERNAL_CLOCK
    ldh [rSC], a
    ret

Call_D0::               ; Unused?
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
    jr z, .skip
    ldh a, [hSerialRole]
    cp a, MASTER
    jr nz, .skip
    xor a
    ldh [$CE], a
    ldh a, [hSerialTx]
    ldh [rSB], a
    ld hl, rSC          ; Why?
    ld [hl], $81        ; Missing in hardware.inc -_-
.skip
    call Call_21E0
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
    call Call_1ED7
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
    ldh [$A4], a        ; Unused?
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

    ld hl, $FFFE        ; TODO
    ld b, $80           ; Off by one, bug?
.clearHRAMloop
    ldd [hl], a
    dec b
    jr nz, .clearHRAMloop

; No memory other than HRAM can be accessed during DMA, so the routine is copied
; and executed there
    ld c, LOW(hDMARoutine)
    ld b, DMARoutine.end - DMARoutine + 2 ; Exact same bug as in Super Mario Land
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
    ld b, 2             ; Two timers, just like Super Mario Land. Timer 2 is 
.decrementTimer         ; rarely used.
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
dw GameState_02 ; Buran liftoff
dw GameState_03 ; Buran rising
dw GameState_04 ; Game over screen
dw GameState_05 ; Type B victory jingle
dw GameState_06 ; Init title screen
dw GameState_07 ; Title screen
dw GameState_08 ; Init Game Type/Music Type Selection Screen
dw GameState_09 ; just points to a random RET... what?
dw GameState_0A ; Init game?
dw GameState_0B ; Init Type B scoreboard
dw GameState_0C ; ?
dw GameState_0D ; Game over curtain
dw GameState_0E ; Select Game Type
dw GameState_0F ; Select Music Type
dw GameState_10 ; Init Type A difficulty selection
dw GameState_11 ; Type A level selection
dw GameState_12 ; Init Type B difficulty selection
dw GameState_13 ; Type B level selection
dw GameState_14 ; Type B start height selection
dw GameState_15 ; Entering topscore for either game type
dw GameState_16 ; Init 2P game difficulty selection
dw GameState_17 ; Select 2P game start height
dw GameState_18 ; Init 2P game
dw GameState_19 ; Init 2P game (2x)
dw $0B31 ; 0x1A 2P game
dw $0CEB ; 0x1B 2P end of game jingle?
dw $0AD2 ; 0x1C Prepare garbage?
dw $0D32 ; 0x1D Init 2P victory screen?
dw $0E23 ; 0x1E Init 2P defeat screen?
dw $1112 ; 0x1F Init 2P game (3x)
dw $0D99 ; 0x20 2P victory screen
dw $0E8A ; 0x21 2P defeat screen
dw GameState_22 ; Init Type B bonus ending
dw GameState_23 ; Dancers
dw GameState_24 ; Init copyright screen
dw GameState_25 ; Copyright screen
dw GameState_26 ; Init Buran
dw GameState_27 ; Prepare Buran launch
dw GameState_28 ; Buran ignition
dw GameState_29 ; Buran ignition for real this time
dw GameState_2A ; Init 2P music selection?
dw GameState_2B ; 2P Select music
dw GameState_2C ; Print congratulations
dw GameState_2D ; Congratulations
dw GameState_2E ; Init rocket launch
dw GameState_2F ; Rocket
dw GameState_30 ; Rocket ignition
dw GameState_31 ; Rocket liftoff
dw GameState_32 ; Rocket main engine fire
dw GameState_33 ; End of bonus scene
dw GameState_34 ; Game over screen leading to bonus ending
dw GameState_35 ; Copyright screen, but skippable
dw $27EA ; 0x36

GameState_24::
    call DisableLCD
    call LoadCopyrightAndTitleScreenTiles
    ld de, CopyrightScreenTilemap
    call LoadTilemap.to9800
    call ClearObjects
    ld hl, wPieceList
    ld de, DemoPieceList
.loop
    ld a, [de]
    ldi [hl], a
    inc de
    ld a, h
    cp a, $C4           ; TODO, might copy way, way too much? Including some music?
    jr nz, .loop
    ld a, $D3           ; Turn on LCD, background and objects
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
    ldh [hDemoRecording], a
    ldh [$98], a
    ldh [$9C], a
    ldh [$9B], a
    ldh [hTopScorePointerHi], a   ; TODO
    ldh [$9F], a
    ldh [hWipeCounter], a
    ldh [hNewTopScore], a
    call Call_2293
    call $2651
    call LoadCopyrightAndTitleScreenTiles
    ld hl, $C800
.loop1
    ld a, $2F
    ldi [hl], a
    ld a, h
    cp a, $CC
    jr nz, .loop1
    ld hl, $C801
    call Call_26A9      ; This sets up the walls of the playing field?
    ld hl, $C80C
    call Call_26A9
    ld hl, $CA41
    ld b, $0C
    ld a, $8E
.loop2
    ldi [hl], a
    dec b
    jr nz, .loop2
    ld de, TitleScreenTilemap
    call LoadTilemap.to9800
    call ClearObjects
    ld hl, wOAMBuffer
    ld [hl], $80        ; TODO sets up the cursor object
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
    ldh [hNumPiecesPlayed], a
    ldh [$ED], a
    ldh [hDemoJoypadTimer], a
    ld a, HIGH(TypeADemoData)
    ldh [hDemoJoypadDataHi], a
    ld a, LOW(TypeADemoData)
    ldh [hDemoJoypadDataLo], a
    ldh a, [hDemoNumber]
    cp a, 2
    ld a, 2
    jr nz, .skip        ; First load demo 2, then demo 1
    ld a, $77           ; B-Type
    ldh [hGameType], a
    ld a, 9
    ldh [hTypeBLevel], a
    ld a, 2
    ldh [hTypeBStartHeight], a
    ld a, HIGH(TypeBDemoData)
    ldh [hDemoJoypadDataHi], a
    ld a, LOW(TypeBDemoData)
    ldh [hDemoJoypadDataLo], a
    ld a, 17
    ldh [hNumPiecesPlayed], a
    ld a, 1
.skip
    ldh [hDemoNumber], a
    ld a, $0A
    ldh [hGameState], a
    call DisableLCD
    call LoadGameplayTiles
    ld de, ConfigScreenTilemap
    call LoadTilemap.to9800
    call ClearObjects
    ld a, $D3           ; TODO
    ldh [rLCDC], a
    ret

; Unused?
StartRecordingDemo::
    ld a, $FF
    ldh [hDemoRecording], a
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
    ld a, SLAVE
    ldh [rSB], a
    ld a, SERIAL_TRANSFER_EXTERNAL_CLOCK
    ldh [rSC], a
    ldh a, [hSerialInterruptTriggered]
    and a
    jr z, .readJoypad
    ldh a, [hSerialRole]
    and a
    jr nz, .launchMultiplayer
    xor a
    ldh [hSerialInterruptTriggered], a
    jr .initCursor

.readJoypad
    ldh a, [hJoyPressed]
    ld b, a
    ldh a, [hIsMultiplayer] ; (Ab)used here to keep track of the pointer
    bit PADB_SELECT, b
    jr nz, .pressedSelect
    bit PADB_RIGHT, b
    jr nz, .pressedRight
    bit PADB_LEFT, b
    jr nz, .pressedLeft
    bit PADB_START, b
    ret z
    and a
    ld a, $08
    jr z, .pressedStart1P
    ld a, b
    cp a, PADF_START    ; At this point, we already know start has been pressed
    ret nz              ; So what's the point?
    ldh a, [hSerialRole]
    cp a, MASTER
    jr z, .launchMultiplayer
    ld a, MASTER
    ldh [rSB], a
    ld a, SERIAL_TRANSFER_INTERNAL_CLOCK
    ldh [rSC], a
    WAIT_FOR_SERIAL_INTERRUPT
    ldh a, [hSerialRole]
    and a
    jr z, .initCursor
.launchMultiplayer
    ld a, $2A
.nextState
    ldh [hGameState], a
    xor a
    ldh [hTimer1], a
    ldh [hTypeALevel], a
    ldh [hTypeBLevel], a
    ldh [hTypeBStartHeight], a
    ldh [hDemoNumber], a
    ret

.pressedStart1P
    push af
    ldh a, [hJoyHeld]
    bit PADB_DOWN, a
    jr z, .normalMode
    ldh [hHeartMode], a
.normalMode
    pop af
    jr .nextState

.pressedSelect
    xor a, 1
.updateCursor
    ldh [hIsMultiplayer], a
    and a
    ld a, $10
    jr z, .out
    ld a, $60               ; TODO
.out
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
    ld a, SERIAL_TRANSFER_EXTERNAL_CLOCK
    ldh [rSC], a
    ldh a, [hJoyPressed]
    bit PADB_START, a
    jr z, .checkDemoEnded
    ld a, $33           ; TODO
    ldh [rSB], a
    ld a, SERIAL_TRANSFER_INTERNAL_CLOCK
    ldh [rSC], a
    ld a, $06
    ldh [hGameState], a
    ret

.checkDemoEnded
    ld hl, hNumPiecesPlayed
    ldh a, [hDemoNumber]
    cp a, 2
    ld b, 16            ; Demo 2 goes from piece 0 to 15
    jr z, .skip
    ld b, 29            ; Demo 1 goes from piece 17(!) to piece 29
.skip
    ld a, [hl]
    cp b
    ret nz
    ld a, $06
    ldh [hGameState], a
    ret

; Demos work by simply running the game as normal and simulating keypresses.
; The saved data uses a very rudimentary form of run-length encoding, where
; data is only recorded for when keys are pressed or released, together with
; a timer indicating the number of frames until such a change
DemoSimulateJoypad::
    ldh a, [hDemoNumber]
    and a
    ret z
    ldh a, [hDemoRecording]
    cp a, $FF           ; TODO constant
    ret z
    ldh a, [hDemoJoypadTimer]
    and a
    jr z, .newKeys
    dec a
    ldh [hDemoJoypadTimer], a
    jr .noNewKeys

.newKeys
    ldh a, [hDemoJoypadDataHi]
    ld h, a
    ldh a, [hDemoJoypadDataLo]
    ld l, a
    ldi a, [hl]
    ld b, a
    ldh a, [hDemoJoypadHeld]
    xor b
    and b
    ldh [hJoyPressed], a
    ld a, b
    ldh [hDemoJoypadHeld], a
    ldi a, [hl]
    ldh [hDemoJoypadTimer], a
    ld a, h
    ldh [hDemoJoypadDataHi], a
    ld a, l
    ldh [hDemoJoypadDataLo], a
    jr .saveRealKeypresses

.noNewKeys
    xor a
    ldh [hJoyPressed], a
.saveRealKeypresses
    ldh a, [hJoyHeld]
    ldh [hSavedJoyHeld], a
    ldh a, [hDemoJoypadHeld]
    ldh [hJoyHeld], a
    ret

.releaseKeys            ; Unused. Maybe an earlier attempt at RLE?
    xor a
    ldh [hDemoJoypadHeld], a
    jr .noNewKeys
    ret                 ; Twice unreachable?

RecordDemo::
    ldh a, [hDemoNumber]
    and a
    ret z
    ldh a, [hDemoRecording]
    cp a, $FF
    ret nz
    ldh a, [hJoyHeld]
    ld b, a
    ldh a, [hDemoJoypadHeld]
    cp b
    jr z, .noNewKeys
    ldh a, [hDemoJoypadDataHi]
    ld h, a
    ldh a, [hDemoJoypadDataLo]
    ld l, a
    ldh a, [hDemoJoypadHeld]
    ldi [hl], a
    ldh a, [hDemoJoypadTimer]
    ldi [hl], a
    ld a, h
    ldh [hDemoJoypadDataHi], a
    ld a, l
    ldh [hDemoJoypadDataLo], a
    ld a, b
    ldh [hDemoJoypadHeld], a
    xor a
    ldh [hDemoJoypadTimer], a
    ret

; If no new keys were pressed or released since the last frame, extend the
; timer and return
.noNewKeys
    ldh a, [hDemoJoypadTimer]
    inc a
    ldh [hDemoJoypadTimer], a
    ret

; Button presses can otherwise be overridden during a demo
RestoreDemoSavedJoypad::
    ldh a, [hDemoNumber]
    and a
    ret z
    ldh a, [hDemoRecording]
    and a
    ret nz
    ldh a, [hSavedJoyHeld]
    ldh [hJoyHeld], a
    ret

Label_5C0::
    ld hl, rSC
    set 7, [hl]         ; TODO
    jr GameState_2A.label_5D1

GameState_2A::
    ld a, $03
    ldh [$CD], a
    ldh a, [hSerialRole]
    cp a, MASTER
    jr nz, Label_5C0    ; TODO Namespace?
.label_5D1
    call GameState_08.loadTiles
    ld a, $80
    ld [$C210], a       ; There is no selecting the game type during multiplayer
    call Call_2671
    ldh [$CE], a        ; This line should be exchanged with the next one. Bug.
    xor a               ; Luckily, A is guaranteed to be zero after Call_2671?
    ldh [rSB], a
    ldh [hSerialTx], a
    ldh [$DC], a
    ldh [$D2], a
    ldh [$D3], a
    ldh [$D4], a
    ldh [$D5], a
    ldh [hWipeCounter], a
    call $7FF3
    ld a, $2B
    ldh [hGameState], a
    ret

GameState_2B::
    ldh a, [hSerialRole]
    cp a, MASTER
    jr z, .label_613
    ldh a, [$F0]
    and a
    jr z, .label_620
    xor a
    ldh [$F0], a
    ld de, $C201
    call PositionMusicTypeSprite.positionSprite ; Avoids the sfx
    call SwitchMusic
    call Call_2671
    jr .label_620

.label_613
    ldh a, [hJoyPressed]
    bit PADB_A, a
    jr nz, .label_620
    bit PADB_START, a
    jr nz, .label_620
    call GameState_0F   ; Read directional keys and update sprite position
.label_620
    ldh a, [hSerialRole]
    cp a, MASTER
    jr z, .label_644
    ldh a, [hSerialInterruptTriggered]
    and a
    ret z
    xor a
    ldh [hSerialInterruptTriggered], a
    ld a, $39
    ldh [hSerialTx], a
    ldh a, [hSerialRx]
    cp a, $50
    jr z, .label_664
    ld b, a
    ldh a, [hMusicType]
    cp b
    ret z
    ld a, b
    ldh [hMusicType], a
    ld a, $01
    ldh [$F0], a
    ret

.label_644
    ldh a, [hJoyPressed]
    bit PADB_START, a
    jr nz, .label_66C
    bit PADB_A, a
    jr nz, .label_66C
    ldh a, [hSerialInterruptTriggered]
    and a
    ret z
    xor a
    ldh [hSerialInterruptTriggered], a
    ldh a, [hSerialTx]
    cp a, $50
    jr z, .label_664
    ldh a, [hMusicType]
.label_65D
    ldh [hSerialTx], a
    ld a, $01
    ldh [$CE], a
    ret

.label_664
    call ClearObjects
    ld a, $16
    ldh [hGameState], a
    ret

.label_66C
    ld a, $50
    jr .label_65D

Label_670::             ; Not again...
    ld hl, rSC          ; Signal readiness for transfer, after the transfer
    set 7, [hl]         ; this bit will be set to zero, but is it ever checked?
    jr GameState_16.skip

GameState_16::
    ld a, $03
    ldh [$CD], a
    ldh a, [hSerialRole]
    cp a, MASTER
    jr nz, Label_670
    call PickRandomPiece
    call PickRandomPiece
    call PickRandomPiece
    ld b, 0             ; Wraps around, pick 256 random pieces
    ld hl, wPieceList
.loop
    call PickRandomPiece
    ldi [hl], a
    dec b
    jr nz, .loop
.skip
    call DisableLCD
    call LoadGameplayTiles
    ld de, MultiplayerDifficultyTilemap
    call LoadTilemap.to9800
    call ClearObjects
    ld a, " "
    call FillPlayingFieldAndWipe.fill
    ld a, $03
    ldh [$CE], a
    xor a
    ldh [rSB], a
    ldh [hSerialTx], a
    ldh [$DC], a
    ldh [$D2], a
    ldh [$D3], a
    ldh [$D4], a
    ldh [$D5], a
    ldh [hWipeCounter], a
    ldh [hSerialInterruptTriggered], a
    ld hl, $C400
    ld b, 10
    ld a, $28           ; Prepare a line of 10 bricks?
.brickLoop
    ldi [hl], a
    dec b
    jr nz, .brickLoop
    ldh a, [$D6]
    and a
    jp nz, $76D
    call SwitchMusic
    ld a, $D3
    ldh [rLCDC], a
    ld hl, wOAMBuffer + 4 * 32
    ld de, MarioLuigiFaceObjects
    ld b, 4 * 8         ; Copy 8 objects
    call Call_725
    ld hl, $C200
    ld de, Data_26ED
    ld c, 2
    call LoadSprites
    call UpdatePlayerStartHeightCursors
    call Call_2671
    xor a
    ldh [hMarioWins], a
    ldh [hLuigiWins], a
    ldh [$D9], a
    ldh [$DA], a
    ldh [$DB], a
    ld a, $17
    ldh [hGameState], a
    ret

MarioLuigiFaceObjects::
db $40, $28, $AE, $00, $40, $30, $AE, $20
db $48, $28, $AF, $00, $48, $30, $AF, $20
db $78, $28, $C0, $00, $78, $30, $C0, $20
db $80, $28, $C1, $00, $80, $30, $C1, $20

Call_725::              ; Absolutely ridiculous. Bug
.loop
    ld a, [de]
    ldi [hl], a
    inc de
    dec b
    jr nz, .loop
    ret

GameState_17::
    ldh a, [hSerialRole]
    cp a, MASTER
    jr z, .checkStart
    ldh a, [hSerialInterruptTriggered]
    and a
    jr z, .moveLuigiCursor
    ldh a, [hSerialRx]
    cp a, $60
    jr z, .startGameplay
    cp a, 6
    jr nc, .sendLuigiStartHeight
    ldh [hMarioStartHeight], a
.sendLuigiStartHeight
    ldh a, [hLuigiStartHeight]
    ldh [hSerialTx], a
    xor a
    ldh [hSerialInterruptTriggered], a
.moveLuigiCursor
    ld de, $C210
    call ReadJoypadAndBlinkCursor
    ld hl, hLuigiStartHeight
    jr .moveCursor

.checkStart
    ldh a, [hJoyPressed]
    bit PADB_START, a
    jr z, .receiveData
    ld a, $60           ; $60 means the game has started
    jr .sendA

.receiveData
    ldh a, [hSerialInterruptTriggered]
    and a
    jr z, .moveMarioCursor
    ldh a, [hSerialTx]  ; If we've previously sent $60, we're planning to start
    cp a, $60           ; the game. Brittle logic...
    jr nz, .receiveLuigiStartingHeight
.startGameplay
    call ClearObjects
    ldh a, [$D6]        ; Non-zero if this isn't the first round
    and a
    jr nz, .nextRound
    ld a, $18
    ldh [hGameState], a
    ldh a, [hSerialRole]
    cp a, MASTER
    ret nz
    xor a
    ldh [$A0], a
    ld a, 6
    ld de, -$20
    ld hl, $C9A2
    call InitGarbage
    ret

.nextRound
    ldh a, [hSerialRole]
    cp a, MASTER
    jp nz, $828
    xor a
    ldh [$A0], a
    ld a, 6
    ld de, -$20
    ld hl, $C9A2
    call InitGarbage
    jp $828

.receiveLuigiStartingHeight
    ldh a, [hSerialRx]
    cp a, 6
    jr nc, .sendStartingHeight
    ldh [hLuigiStartHeight], a
.sendStartingHeight
    ldh a, [hMarioStartHeight]
.sendA
    ldh [hSerialTx], a
    xor a
    ldh [hSerialInterruptTriggered], a
    inc a
    ldh [$CE], a
.moveMarioCursor
    ld de, $C200
    call ReadJoypadAndBlinkCursor
    ld hl, hMarioStartHeight
.moveCursor
    ld a, [hl]
    bit PADB_RIGHT, b
    jr nz, .pressedRight
    bit PADB_LEFT, b
    jr nz, .pressedLeft
    bit PADB_UP, b
    jr nz, .pressedUp
    bit PADB_DOWN, b
    jr z, .updateAndRenderCursors
    cp a, 3
    jr nc, .updateAndRenderCursors
    add a, 3
    jr .playSFX

.pressedRight
    cp a, 5
    jr z, .updateAndRenderCursors
    inc a
.playSFX
    ld [hl], a
    ld a, $01
    ld [$DFE0], a
.updateAndRenderCursors
    call UpdatePlayerStartHeightCursors
    call Call_2671
    ret

.pressedLeft
    and a
    jr z, .updateAndRenderCursors
    dec a
    jr .playSFX

.pressedUp
    cp a, 3
    jr c, .updateAndRenderCursors
    sub a, 3
    jr .playSFX

MarioStartHeightCursorCoordinates::
    db $40, $60, $40, $70, $40, $80
    db $50, $60, $50, $70, $50, $80

LuigiStartHeightCursorCoordinates:: ; TODO Name
    db $78, $60, $78, $70, $78, $80
    db $88, $60, $88, $70, $88, $80

UpdatePlayerStartHeightCursors::    ; TODO name
    ldh a, [hMarioStartHeight]
    ld de, $C201
    ld hl, MarioStartHeightCursorCoordinates
    call UpdateDigitCursor.afterSFX
    ldh a, [hLuigiStartHeight]
    ld de, $C211
    ld hl, LuigiStartHeightCursorCoordinates
    call UpdateDigitCursor.afterSFX
    ret

GameState_18::
    call DisableLCD
    xor a
    ld [$C210], a
    ldh [$98], a
    ldh [$9C], a
    ldh [$9B], a
    ldh [hTopScorePointerHi], a ; TODO
    ldh [$9F], a
    ldh [hSerialInterruptTriggered], a
    ldh [rSB], a
    ldh [$CE], a
    ldh [hSerialRx], a
    ldh [hSerialTx], a
    ldh [$D1], a
    call $2651
    call Call_2293
    call Call_1FF2
    xor a
    ldh [hWipeCounter], a
    call ClearObjects
    ld de, MultiplayerGameplayTilemap
    push de
    ld a, 1             ; Why? Bug
    ldh [hLevel], a
    ldh [hIsMultiplayer], a
    call LoadTilemap.to9800
    pop de
    ld hl, $9C00
    call LoadTilemap.toHL
    ld de, PauseMessageTilemap
    ld hl, $9C63
    ld c, 10
    call Call_1F7D
    ld hl, $C200
    ld de, ActivePieceSprite
    call CopyUntilFF
    ld hl, $C210
    ld de, PreviewPieceSprite
    call CopyUntilFF
    ld hl, $9951        ; Lines
    ld a, $30
    ldh [hLines], a
    ld [hl], "0"
    dec l
    ld [hl], "3"
    call LookupGravity
    xor a
    ldh [$A0], a
    ldh a, [hSerialRole]
    cp a, MASTER
    ld de, MarioFaceObjects
    ldh a, [hMarioStartHeight]
    jr z, .skip
    ld de, LuigiFaceObjects
    ldh a, [hLuigiStartHeight]
.skip
    ld hl, $98B0        ; High
    ld [hl], a
    ld h, $9C
    ld [hl], a
    ld hl, wOAMBuffer + 4*32
    ld b, $10
    call Call_725
    ld a, $77
    ldh [hGameType], a
    ld a, $D3
    ldh [rLCDC], a
    ld a, $19
    ldh [hGameState], a
    ld a, $01
    ldh [$CD], a
    ret

LuigiFaceObjects::
    db $18, $84, $C0, $00, $18, $8C, $C0, $20
    db $20, $84, $C1, $00, $20, $8C, $C1, $20
MarioFaceObjects::
    db $18, $84, $AE, $00, $18, $8C, $AE, $20
    db $20, $84, $AF, $00, $20, $8C, $AF, $20

; Can this be cleaned up with a macro?
GameState_19::
    ld a, IEF_SERIAL    ; Disables VBlank interrupt
    ldh [rIE], a
    xor a
    ldh [rIF], a
    ldh a, [hSerialRole]
    cp a, MASTER
    jp nz, .slave
.waitForSlave
    call DelayMillisecond   ; Interrupts are disabled, so this is pointless?
    call DelayMillisecond
    xor a
    ldh [hSerialInterruptTriggered], a
    ld a, MASTER        ; Assert dominance
    ldh [rSB], a
    ld a, SERIAL_TRANSFER_INTERNAL_CLOCK
    ldh [rSC], a
    WAIT_FOR_SERIAL_INTERRUPT
    ldh a, [rSB]
    cp a, SLAVE
    jr nz, .waitForSlave

    ld de, $20 - 10     ; The playing field's staging area is filled with 10
    ld c, 10            ; rows of garbage, which are sent top to bottom
    ld hl, $C902        ; 10th row from the bottom
.sendGarbageRowLoop
    ld b, 10
.sendGarbageColumnLoop
    xor a
    ldh [hSerialInterruptTriggered], a
    call DelayMillisecond
    ldi a, [hl]
    ldh [rSB], a
    ld a, SERIAL_TRANSFER_INTERNAL_CLOCK
    ldh [rSC], a
    WAIT_FOR_SERIAL_INTERRUPT
    dec b
    jr nz, .sendGarbageColumnLoop
    add hl, de
    dec c
    jr nz, .sendGarbageRowLoop

    ldh a, [hMarioStartHeight]
    cp a, 5
    jr z, .sendPieceList
    ld hl, $CA22        ; bottom row, left
    ld de, $20 * 2
.loop
    add hl, de
    inc a
    cp a, 5
    jr nz, .loop
    ld de, $CA22        ; Shift the garbage down 10 - 2*hMarioStartHeight rows,
    ld c, 10            ; leaving 2*hMarioStartHeight rows visible
.rowLoop2
    ld b, 10
.columnLoop2
    ld a, [de]
    ldi [hl], a
    inc e
    dec b
    jr nz, .columnLoop2
    push de
    ld de, -$20 - 10    ; $20 between rows, playing field is 10 blocks wide
    add hl, de
    pop de
    push hl
    ld hl, -$20 - 10
    add hl, de
    push hl
    pop de
    pop hl
    dec c
    jr nz, .rowLoop2

    ld de, -$20 - 10    ; Now overwrite the remaining rows on the top with
.rowLoop3               ; empty space
    ld b, 10
    ld a, h
    cp a, $C8           ; The highest possible row of garbage starts at $C902
    jr z, .sendPieceList
    ld a, " "
.columnLoop3
    ldi [hl], a
    dec b
    jr nz, .columnLoop3
    add hl, de
    jr .rowLoop3

.sendPieceList
.waitForSlave2
    call DelayMillisecond
    call DelayMillisecond
    xor a
    ldh [hSerialInterruptTriggered], a
    ld a, MASTER
    ldh [rSB], a
    ld a, SERIAL_TRANSFER_INTERNAL_CLOCK
    ldh [rSC], a
    WAIT_FOR_SERIAL_INTERRUPT
    ldh a, [rSB]
    cp a, SLAVE
    jr nz, .waitForSlave2

    ld hl, wPieceList
    ld b, 0             ; Wraps around, sends 256 pieces in total
.sendPieceListLoop
    xor a
    ldh [hSerialInterruptTriggered], a
    ldi a, [hl]
    call DelayMillisecond
    ldh [rSB], a
    ld a, SERIAL_TRANSFER_INTERNAL_CLOCK
    ldh [rSC], a
    WAIT_FOR_SERIAL_INTERRUPT
    inc b
    jr nz, .sendPieceListLoop
.waitForSlave3
    call DelayMillisecond
    call DelayMillisecond
    xor a
    ldh [hSerialInterruptTriggered], a
    ld a, $30           ; TODO, magic constants? 30 = 29 + 1?
    ldh [rSB], a
    ld a, SERIAL_TRANSFER_INTERNAL_CLOCK
    ldh [rSC], a
    WAIT_FOR_SERIAL_INTERRUPT
    ldh a, [rSB]
    cp a, $56
    jr nz, .waitForSlave3

.nextState
    call .restoreFloor
    ld a, IEF_VBLANK | IEF_SERIAL
    ldh [rIE], a
    ld a, $1C
    ldh [hGameState], a
    ld a, $02
    ldh [hWipeCounter], a
    ld a, $03
    ldh [$CD], a
    ldh a, [hSerialRole]
    cp a, MASTER
    jr z, .skip
    ld hl, rSC
    set 7, [hl]
.skip
    ld hl, wPieceList
    ldi a, [hl]
    ld [$C203], a
    ldi a, [hl]
    ld [$C213], a
    ld a, h
    ldh [$AF], a        ; TODO Piece List Pointer high byte, but never used?
    ld a, l
    ldh [$B0], a
    ret

.slave                  ; Similar to the master code, only it receives the data
    ldh a, [hLuigiStartHeight]
    inc a
    ld b, a
    ld hl, $CA42
    ld de, -2*$20
.loop2
    dec b
    jr z, .receiveGarbage
    add hl, de
    jr .loop2

.receiveGarbage
.waitForMaster
    call DelayMillisecond
    xor a
    ldh [hSerialInterruptTriggered], a
    ld a, SLAVE
    ldh [rSB], a
    ld a, SERIAL_TRANSFER_EXTERNAL_CLOCK
    ldh [rSC], a
    WAIT_FOR_SERIAL_INTERRUPT
    ldh a, [rSB]
    cp a, MASTER
    jr nz, .waitForMaster

    ld de, $20 - 10
    ld c, 10
.receiveGarbageRowLoop
    ld b, 10
.receiveGarbageColumnLoop
    xor a
    ldh [hSerialInterruptTriggered], a
    ldh [rSB], a
    ld a, SERIAL_TRANSFER_EXTERNAL_CLOCK
    ldh [rSC], a
    WAIT_FOR_SERIAL_INTERRUPT
    ldh a, [rSB]
    ldi [hl], a
    dec b
    jr nz, .receiveGarbageColumnLoop
    add hl, de
    dec c
    jr nz, .receiveGarbageRowLoop

.waitForMaster2
    call DelayMillisecond
    xor a
    ldh [hSerialInterruptTriggered], a
    ld a, SLAVE
    ldh [rSB], a
    ld a, SERIAL_TRANSFER_EXTERNAL_CLOCK
    ldh [rSC], a
    WAIT_FOR_SERIAL_INTERRUPT
    ldh a, [rSB]
    cp a, MASTER
    jr nz, .waitForMaster2

    ld b, 0
    ld hl, wPieceList
.receivePieceListLoop
    xor a
    ldh [hSerialInterruptTriggered], a
    ldh [rSB], a
    ld a, SERIAL_TRANSFER_EXTERNAL_CLOCK
    ldh [rSC], a
    WAIT_FOR_SERIAL_INTERRUPT
    ldh a, [rSB]
    ldi [hl], a
    inc b
    jr nz, .receivePieceListLoop

.waitForMaster3
    call DelayMillisecond
    xor a
    ldh [hSerialInterruptTriggered], a
    ld a, $56
    ldh [rSB], a
    ld a, SERIAL_TRANSFER_EXTERNAL_CLOCK
    ldh [rSC], a
    WAIT_FOR_SERIAL_INTERRUPT
    ldh a, [rSB]
    cp a, $30
    jr nz, .waitForMaster3
    jp .nextState

.restoreFloor           ; Shifting the garbage might have erased the floor
    ld hl, $CA42        ; Invisible row below the bottom of the playing field
    ld a, $80           ; Generic solid block, technically part of I piece
    ld b, 10
.floorLoop
    ldi [hl], a
    dec b
    jr nz, .floorLoop
    ret

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

; Very similar to NextPiece, check the comments there
PickRandomPiece::
    push hl
    push bc
    ldh a, [hTempPreviewPiece]
    and a, ~%11
    ld c, a
    ld h, 3
.tryAgain
    ldh a, [rDIV]
    ld b, a
.wrap
    xor a
.loop
    dec b
    jr z, .checkRandomizer
    inc a
    inc a
    inc a
    inc a
    cp a, 7 * 4
    jr z, .wrap
    jr .loop

.checkRandomizer
    ld d, a
    ldh a, [hNextPreviewPiece]
    ld e, a
    dec h
    jr z, .acceptPiece
    or d
    or c
    and a, ~%11
    cp c
    jr z, .tryAgain
.acceptPiece
    ld a, d
    ldh [hNextPreviewPiece], a
    ld a, e
    ldh [hTempPreviewPiece], a
    pop bc
    pop hl
    ret

INCBIN "baserom.gb", $AD2, $1167 - $AD2

GameState_26::
    call Call_11B2
    ld hl, $9CE6
    ld de, LeftTowerLeftSideTilemap
    ld b, 7
    call LoadTilemap9C00Row
    ld hl, $9CE7
    ld de, LeftTowerRightSideTilemap
    ld b, 7
    call LoadTilemap9C00Row
    ld hl, $9D08
    ld [hl], $72        ; Launch tower umbilicals?
    inc l
    ld [hl], $C4
    ld hl, $9D28
    ld [hl], $B7
    inc l
    ld [hl], $B8        ; Crew tunnel?
    ld de, Data_2771
    ld hl, $C200
    ld c, 3
    call LoadSprites
    ld a, 3
    call Call_2673
    ld a, $DB           ; Re-enable LCD, and switch tilemap to 9C00
    ldh [rLCDC], a
    ld a, 187           ; A hint over 3 seconds. Sigh
    ldh [hTimer1], a
    ld a, $27
    ldh [hGameState], a
    ld a, $10
    ld [$DFE8], a
    ret

Call_11B2::
    call DisableLCD
    ld hl, MultiplayerAndBuranTiles
    ld bc, 256*16        ; Way too much
    call LoadTilesFromHL.loadBCBytes    ; todo wtf
    ld hl, $9FFF
    call $2798          ; Clears the tilemap
    ld hl, $9DC0
    ld de, BuranBackdropTilemap
    ld b, 4             ; todo
    call LoadTilemap.columnLoop
    ld hl, $9CEC
    ld de, RightTowerLeftSideTilemap
    ld b, 7
    call LoadTilemap9C00Row
    ld hl, $9CED
    ld de, RightTowerRightSideTilemap
    ld b, 7
    call LoadTilemap9C00Row
    ret

GameState_27::
    ldh a, [hTimer1]
    and a
    ret nz
    ld hl, $C210        ; Launch smoke
    ld [hl], $00
    ld l, $20
    ld [hl], $00
    ld a, 255           ; 4¼ seconds, maximum possible
    ldh [hTimer1], a
    ld a, $28
    ldh [hGameState], a
    ret

GameState_28::
    ldh a, [hTimer1]
    and a
    jr z, .nextState
    call Call_13FA
    ret

.nextState
    ld a, $29
    ldh [hGameState], a
    ld hl, $C213
    ld [hl], $35
    ld l, $23
    ld [hl], $35
    ld a, 255
    ldh [hTimer1], a
    ld a, " "
    call FillPlayingFieldAndWipe
    ret

GameState_29::
    ldh a, [hTimer1]
    and a
    jr z, .nextState
    call Call_13FA
    ret

.nextState
    ld a, $02
    ldh [hGameState], a
    ld hl, $9D08        ; Remove umbilicals
    ld b, " "
    call PrintCharacter.printB
    ld hl, $9D09
    call PrintCharacter.printB
    ld hl, $9D28
    call PrintCharacter.printB
    ld hl, $9D29
    call PrintCharacter.printB
    ret

GameState_02::
    ldh a, [hTimer1]
    and a
    jr nz, .label_1277
    ld a, 10            ; ⅙ second
    ldh [hTimer1], a
    ld hl, $C201
    dec [hl]
    ld a, [hl]
    cp a, $58
    jr nz, .label_1277
    ld hl, $C210
    ld [hl], $00
    inc l
    add a, $20
    ldi [hl], a
    ld [hl], $4C
    inc l
    ld [hl], $40
    ld l, $20
    ld [hl], $80
    ld a, $03
    call Call_2673
    ld a, $03
    ldh [hGameState], a
    ld a, $04
    ld [$DFF8], a
    ret

.label_1277
    call Call_13FA
    ret

GameState_03::
    ldh a, [hTimer1]
    and a
    jr nz, .label_129D
    ld a, 10
    ldh [hTimer1], a
    ld hl, $C211
    dec [hl]
    ld l, $01
    dec [hl]
    ld a, [hl]
    cp a, $D0
    jr nz, .label_129D
    ld a, $9C
    ldh [$C9], a
    ld a, $82
    ldh [$CA], a
    ld a, $2C
    ldh [hGameState], a
    ret

.label_129D
    ldh a, [hTimer2]
    and a
    jr nz, .label_12AD
    ld a, 6
    ldh [hTimer2], a
    ld hl, $C213
    ld a, [hl]
    xor a, $01
    ld [hl], a
.label_12AD
    ld a, $03
    call Call_2673
    ret

GameState_2C::
    ldh a, [hTimer1]
    and a
    ret nz
    ld a, 6
    ldh [hTimer1], a
    ldh a, [$FFCA]
    sub a, $82
    ld e, a
    ld d, $00
    ld hl, .data_12F5
    add hl, de
    push hl
    pop de
    ldh a, [$C9]
    ld h, a
    ldh a, [$CA]
    ld l, a
    ld a, [de]
    call PrintCharacter
    push hl
    ld de, $0020
    add hl, de
    ld b, $B6
    call PrintCharacter.printB
    pop hl
    inc hl
    ld a, $02
    ld [$DFE0], a
    ld a, h
    ldh [$C9], a
    ld a, l
    ldh [$CA], a
    cp a, $92
    ret nz
    ld a, $FF
    ldh [hTimer1], a
    ld a, $2D
    ldh [hGameState], a
    ret

.data_12F5
    db $B3, $BC, $3D, $BE, $BB, $B5, $1D, $B2, $BD, $B5, $1D, $2E, $BC, $3D, $0E, $3E

GameState_2D::
    ldh a, [hTimer1]
    and a
    ret nz
    call DisableLCD
    call LoadGameplayTiles
    call Call_2293
    ld a, $93           ; TODO
    ldh [rLCDC], a
    ld a, $05
    ldh [hGameState], a
    ret

GameState_34::
    ldh a, [hTimer1]
    and a
    ret nz
    ld a, $2E
    ldh [hGameState], a
    ret

GameState_2E::
    call Call_11B2      ; Loads the backdrop used for rocket and shuttle launches
    ld de, Data_2783
    ld hl, $C200
    ld c, 3
    call LoadSprites
    ldh a, [$F3]        ; Rocket sprite
    ld [$C203], a
    ld a, 3
    call Call_2673
    xor a
    ldh [$F3], a
    ld a, $DB
    ldh [rLCDC], a
    ld a, 187
    ldh [hTimer1], a
    ld a, $2F
    ldh [hGameState], a
    ld a, $10
    ld [$DFE8], a
    ret

GameState_2F::
    ldh a, [hTimer1]
    and a
    ret nz
    ld hl, $C210
    ld [hl], $00
    ld l, $20
    ld [hl], $00
    ld a, 160
    ldh [hTimer1], a
    ld a, $30
    ldh [hGameState], a
    ret

GameState_30::
    ldh a, [hTimer1]
    and a
    jr z, .label_1370
    call Call_13FA
    ret

.label_1370
    ld a, $31
    ldh [hGameState], a
    ld a, 128
    ldh [hTimer1], a
    ld a, " "
    call FillPlayingFieldAndWipe
    ret

GameState_31::
    ldh a, [hTimer1]
    and a
    jr nz, .label_13B1
    ld a, 10
    ldh [hTimer1], a
    ld hl, $C201
    dec [hl]
    ld a, [hl]
    cp a, $6A
    jr nz, .label_13B1
    ld hl, $C210
    ld [hl], $00
    inc l
    add a, $10
    ldi [hl], a
    ld [hl], $54
    inc l
    ld [hl], $5C
    ld l, $20
    ld [hl], $80
    ld a, 3
    call Call_2673
    ld a, $32
    ldh [hGameState], a
    ld a, $04
    ld [$DFF8], a
    ret

.label_13B1
    call Call_13FA
    ret

GameState_32::
    ldh a, [hTimer1]
    and a
    jr nz, .label_13CF
    ld a, 10
    ldh [hTimer1], a
    ld hl, $C211
    dec [hl]
    ld l, $01
    dec [hl]
    ld a, [hl]
    cp a, $E0
    jr nz, .label_13CF
    ld a, $33
    ldh [hGameState], a
    ret

.label_13CF
    ldh a, [hTimer2]
    and a
    jr nz, .label_13DF
    ld a, 6
    ldh [hTimer2], a
    ld hl, $C213
    ld a, [hl]
    xor a, $01
    ld [hl], a
.label_13DF
    ld a, 3
    call Call_2673
    ret

GameState_33::
    call DisableLCD
    call LoadGameplayTiles
    call $7FF3
    call Call_2293
    ld a, $93
    ldh [rLCDC], a
    ld a, $10
    ldh [hGameState], a
    ret

Call_13FA::
    ldh a, [hTimer2]
    and a
    ret nz
    ld a, 10
    ldh [hTimer2], a
    ld a, $03
    ld [$DFF8], a
    ld b, 2
    ld hl, $C210
.loop
    ld a, [hl]
    xor a, $80
    ld [hl], a
    ld l, $20
    dec b
    jr nz, .loop
    ld a, $03
    call Call_2673
    ret

LeftTowerLeftSideTilemap::
    db $C2, $CA, $CA, $CA, $CA, $CA, $CA

LeftTowerRightSideTilemap::
    db $C3, $CB, $58, $48, $48, $48, $48

RightTowerLeftSideTilemap::
    db $C8, $73, $73, $73, $73, $73, $73

RightTowerRightSideTilemap::
    db $C9, $74, $74, $74, $74, $74, $74


LoadTilemap9C00Row::
.loop
    ld a, [de]
    ld [hl], a
    inc de
    push de
    ld de, $0020
    add hl, de
    pop de
    dec b
    jr nz, .loop
    ret

GameState_08::
    ld a, IEF_VBLANK
    ldh [rIE], a
    xor a
    ldh [rSB], a
    ldh [rSC], a
    ldh [rIF], a
.loadTiles
    call DisableLCD
    call LoadGameplayTiles
    ld de, ConfigScreenTilemap
    call LoadTilemap.to9800
    call ClearObjects
    ld hl, $C200
    ld de, Data_26CF
    ld c, 2
    call LoadSprites
    ld de, $C201        ; Sprite Y-coordinate
    call PositionMusicTypeSprite
    ldh a, [hGameType]
    ld e, $12           ; LOW(C212), sprite X-coordinate
    ld [de], a
    inc de
    cp a, $37
    ld a, $1C           ; A-Type sprite
    jr z, .skip
    ld a, $1D           ; B-Type sprite
.skip
    ld [de], a
    call Call_2671
    call SwitchMusic
    ld a, $D3
    ldh [rLCDC], a
    ld a, $0E
    ldh [hGameState], a
GameState_09::          ; TODO
    ret

PositionMusicTypeSprite::
    ld a, $01           ; Menu selection SFX
    ld [$DFE0], a       ; TODO I don't think this plays for some reason though
.positionSprite
    ldh a, [hMusicType]
    push af
    sub a, $1C          ; The four music types have consecutive sprite indices
    add a
    ld c, a
    ld b, $00
    ld hl, MusicTypeSpriteCoordinates
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

MusicTypeSpriteCoordinates::
    db $70, $37
    db $70, $77
    db $80, $37
    db $80, $77

; Todo comment on the conditional jumps
GameState_0F::
    ld de, $C200        ; Music Type sprite
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
    jp z, GameState_0E.setupSprite      ; Bug! A jump to .out is exactly the
    cp a, $1E                           ; same and would have saved 2 bytes
    jr nc, .out
    add a, $02
.updateCursor
    ld [hl], a
    call PositionMusicTypeSprite
    call SwitchMusic
.out
    call Call_2671
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
    sub a, $17          ; Based on sprite number
    cp a, $08
    jr nz, .skip
    ld a, $FF           ; Disable music
.skip
    ld [$DFE8], a
    ret

GameState_0E::
    ld de, $C210        ; Game type sprite
    call ReadJoypadAndBlinkCursor
    ld hl, hGameType
    ld a, [hl]
    bit PADB_START, b
    jr nz, .pressedStart
    bit PADB_A, b
    jr nz, .pressedA
    inc e
    inc e               ; Sprite X coordinate
    bit PADB_RIGHT, b
    jr nz, .pressedRight
    bit PADB_LEFT, b
    jr z, .setupSprite
    cp a, $37           ; A-Type
    jr z, .setupSprite
    ld a, $37
    ld b, $1C           ; A-Type sprite
    jr .switchGameType

.pressedRight
    cp a, $77           ; B-Type
    jr z, .setupSprite
    ld a, $77
    ld b, $1D           ; B-Type sprite
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
.setupSprite        ; TODO name
    call Call_2671
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
    ld de, TypeADifficultyTilemap
    call LoadTilemap.to9800
    call Call_18FC
    call ClearObjects
    ld hl, $C200
    ld de, Data_26DB
    ld c, 1
    call LoadSprites
    ld de, $C201
    ldh a, [hTypeALevel]
    ld hl, Data_1615
    call UpdateDigitCursor
    call Call_2671
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
    jr z, .setupSprite
    cp a, 5             ; Levels 0-4 make up the top row
    jr nc, .setupSprite
    add a, 5
    jr .updateCursor

.pressedRight
    cp a, 9             ; 9 is the highest selectable level
    jr z, .setupSprite
    inc a
.updateCursor
    ld [hl], a
    ld de, $C201        ; Sprite 0's Y-coordinate
    ld hl, Data_1615
    call UpdateDigitCursor
    call Call_1795
.setupSprite        ; Name?
    call Call_2671
    ret

.pressedLeft
    and a
    jr z, .setupSprite
    dec a
    jr .updateCursor

.pressedUp
    cp a, 5
    jr c, .setupSprite
    sub a, 5
    jr .updateCursor

; Y and X coordinates of the cursor for various levels
Data_1615::
    db $40, $30, $40, $40, $40, $50, $40, $60, $40, $70
    db $50, $30, $50, $40, $50, $50, $50, $60, $50, $70

; Init Type B difficulty selection screen
GameState_12::
    call DisableLCD
    ld de, TypeBDifficultyTilemap
    call LoadTilemap.to9800
    call ClearObjects
    ld hl, $C200
    ld de, Data_26E1
    ld c, 2
    call LoadSprites
    ld de, $C201
    ldh a, [hTypeBLevel]
    ld hl, $16D2
    call UpdateDigitCursor
    ld de, $C211
    ldh a, [hTypeBStartHeight]
    ld hl, Data_1741
    call UpdateDigitCursor
    call Call_2671
    call Call_17AF
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
    ld a, $14           ; Select Type B start height
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
    jr z, .setupSprite
    cp a, $05
    jr nc, .setupSprite
    add a, $05
    jr .updateCursor

.pressedRight
    cp a, 9
    jr z, .setupSprite
    inc a
.updateCursor
    ld [hl], a
    ld de, $C201
    ld hl, Data_16D2
    call UpdateDigitCursor
    call Call_17AF
.setupSprite
    call Call_2671
    ret

.pressedLeft
    and a
    jr z, .setupSprite
    dec a
    jr .updateCursor

.pressedUp
    cp a, 5
    jr c, .setupSprite
    sub a, 5
    jr .updateCursor

Data_16D2::
    db $40, $18, $40, $28, $40, $38, $40, $48, $40, $58
    db $50, $18, $50, $28, $50, $38, $50, $48, $50, $58

; TODO XXX Seriously!?
Call_16E6::
    ldh [hGameState], a
    xor a
    ld [de], a
    ret

GameState_14::
    ld de, $C210
    call ReadJoypadAndBlinkCursor
    ld hl, hTypeBStartHeight
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
    jr z, .setupSprite
    cp a, 3
    jr nc, .setupSprite
    add a, 3
    jr .updateCursor

.pressedRight
    cp a, $05
    jr z, .setupSprite
    inc a
.updateCursor
    ld [hl], a
    ld de, $C211
    ld hl, Data_1741
    call UpdateDigitCursor
    call Call_17AF
.setupSprite
    call Call_2671
    ret

.pressedLeft
    and a
    jr z, .setupSprite
    dec a
    jr .updateCursor

.pressedUp
    cp a, 3
    jr c, .setupSprite
    sub a, 3
    jr .updateCursor

Data_1741::
    db $40, $70, $40, $80, $40, $90
    db $50, $70, $50, $80, $50, $90

    db $00              ; XXX TODO

; HL points to a list of coordinates, DE to the cursor sprite's Y-coordinate
; A is the required index in the list
UpdateDigitCursor::     ; TODO Name
    push af
    ld a, $01
    ld [$DFE0], a
    pop af
.afterSFX
    push af
    add a               ; Multiply A by 2, as there are two bytes for each
    ld c, a             ; coordinate pair
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

; Why on earth are these two things one function?
ReadJoypadAndBlinkCursor::
    ldh a, [hJoyPressed]
    ld b, a
    ldh a, [hTimer1]
    and a
    ret nz
    ld a, 16
    ldh [hTimer1], a
    ld a, [de]
    xor a, $80
    ld [de], a
    ret

; Init C sprites from adresses starting at DE to HL
LoadSprites::
.loop
    push hl
    ld b, 6             ; 6 datapoints per sprite, but what are they...
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

ClearObjects::
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
    ldh a, [hTypeBStartHeight]
    ld de, $001B
.loopHigh
    and a
    jr z, .label_17CD
    dec a
    add hl, de
    jr .loopHigh        ; TODO → start height

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
; TODO, this decrements DE
CopyThreeBytes::        ; Lol. Bug. TODO, name?
    ld b, 3
.copyBBytes
.loop
    ldd a, [hl]
    ld [de], a
    dec de
    dec b
    jr nz, .loop
    ret

Call_1800::
    ld a, d
    ldh [hTopScorePointerHi], a       ; Big-endian? Even though the Game Boy is
    ld a, e                         ; itself little-endian? Bug
    ldh [hTopScorePointerLo], a
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
    ldh a, [hTopScorePointerHi]
    ld d, a
    ldh a, [hTopScorePointerLo]
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
    Call CopyThreeBytes.copyBBytes    ; Similarly, shift the name one down
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
    ldh a, [hTopScorePointerHi]
    ld h, a
    ldh a, [hTopScorePointerLo]
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
.printB
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
    ldh [hTopScorePointerHi], a   ; Why not the lower byte too?
    ldh [$9F], a
    ld a, " "
    call FillPlayingFieldAndWipe
    call Call_1FF2
    call $2651
    xor a
    ldh [hWipeCounter], a
    call ClearObjects
    ldh a, [hGameType]
    ld de, TypeBGameplayTilemap
    ld hl, hTypeBLevel
    cp a, $77
    ld a, $50
    jr z, .skip
    ld a, $F1
    ld hl, hTypeALevel
    ld de, TypeAGameplayTilemap
.skip
    push de
    ldh [$E6], a
    ld a, [hl]
    ldh [hLevel], a
    call LoadTilemap.to9800
    pop de
    ld hl, $9C00        ; TODO
    call LoadTilemap.toHL
    ld de, PauseMessageTilemap
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
    ld de, ActivePieceSprite
    call CopyUntilFF
    ld hl, $C210
    ld de, PreviewPieceSprite
    call CopyUntilFF
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
    call LookupGravity
    ld a, [$C0DE]
    and a
    jr z, .label_1AA6
    ld a, $80
    ld [$C210], a
.label_1AA6
    call NextPiece
    call NextPiece
    call NextPiece
    call Call_2683
    xor a
    ldh [$A0], a
    ldh a, [hGameType]
    cp a, $77
    jr nz, .turnOnLCDAndReturn
    ld a, $34
    ldh [hDropTimer], a
    ldh a, [hTypeBStartHeight]
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
    call InitDemoGarbage
    jr .turnOnLCDAndReturn

.label_1AD6
    ld a, b
    ld de, -2 * $20     ; Two rows of garbage per height
    ld hl, $9A02        ; Top left of second row of playing field TODO
    call InitGarbage
.turnOnLCDAndReturn
    ld a, $D3           ; Urgh todo
    ldh [rLCDC], a
    xor a
    ldh [hGameState], a
    ret

LookupGravity::
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
InitDemoGarbage::
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
; Writes A rows of garbage 
InitGarbage::
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
    jr z, .writeTile    ; could have been done much more easily with a BIT
    cp a, $80           ; test or an AND, without wasting on average ~1300
    jr nz, .chooseBlock ; cycles every time a new block is picked. As no
    ld a, " "           ; interrupts can fire during this loop, it's completely
    jr .loop            ; deterministic as well.

.writeTile
    cp a, " "
    jr z, .writeHole
    ldh a, [rDIV]       ; Again, the DIV register is used as an RNG
    and a, $07
    or a, $80           ; Tetromino tile numbers range from $80 to $87
    jr .ensureAtLeastOneHole

.writeHole
    ldh [$A0], a
.ensureAtLeastOneHole
    push af
    ld a, l
    and a, $0F
    cp a, $0B           ; The rightmost cell of the playing field has $B as lowest nibble
    jr nz, .popAndWrite
    ldh a, [$A0]
    cp a, " "
    jr z, .popAndWrite
    pop af              ; There's a 1 in 512 chance we picked no empty blocks
    ld a, " "           ; at all for this line. In that case, make the rightmost
    jr .write           ; block empty

.popAndWrite
    pop af
.write
    ld [hl], a
    push hl
    push af
    ldh a, [hIsMultiplayer]
    and a
    jr nz, .skip        ; TODO Is the WRAM buffer not used in multiplayer?
    ld de, $3000        ; I think in multiplayer this routine writes to a
    add hl, de          ; which is later copied to the field?
.skip
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
    call Call_2683          ; TODO smth to do with sprites?
    call Call_2696
    xor a
    ldh [$98], a
    ldh [$9C], a
    call Call_2293
    ld a, $87           ; This tile isn't used in any tetromino
    call FillPlayingFieldAndWipe
    ld a, 70            ; 70 frames is 1⅙ seconds
    ldh [hTimer1], a
    ld a, $0D
    ldh [hGameState], a
    ret

GameState_04::
    ldh a, [hJoyPressed]
    bit PADB_A, a
    jr nz, .continue
    bit PADB_START, a
    ret z
.continue
    xor a
    ldh [hWipeCounter], a
    ldh a, [hIsMultiplayer]
    and a
    ld a, $16
    jr nz, .label_1D23
    ldh a, [hGameType]
    cp a, $37           ; Type A
    ld a, $10
    jr z, .label_1D23
    ld a, $12
.label_1D23
    ldh [hGameState], a
    ret

GameState_05::
    ldh a, [hTimer1]
    and a
    ret nz
    ld hl, $C802
    ld de, ScoreboardTilemap
    call LoadPlayingFieldTilemap
    ldh a, [hTypeBLevel]
    and a
    jr z, .label_1D66
    ld de, $0040
    ld hl, $C827
    call Call_1D84
    ld de, $0100
    ld hl, $C887
    call Call_1D84
    ld de, $0300
    ld hl, $C8E7
    call Call_1D84
    ld de, $1200
    ld hl, $C947
    call Call_1D84
    ld hl, wScore
    ld b, 3
    xor a
.loop
    ldi [hl], a
    dec b
    jr nz, .loop
.label_1D66
    ld a, 128           ; A little over 2 seconds
    ldh [hTimer1], a
    ld a, $80
    ld [$C200], a
    ld [$C210], a
    call Call_2683
    call Call_2696
    call $7FF3
    ld a, $25
    ldh [hLines], a
    ld a, $0B
    ldh [hGameState], a
    ret

Call_1D84::
    push hl
    ld hl, wScore
    ld b, 3
    xor a
.loop
    ldi [hl], a
    dec b
    jr nz, .loop
    ldh a, [hTypeBLevel]
    ld b, a
    inc b
.label_1D93
    ld hl, wScore
    call AddScore
    dec b
    jr nz, .label_1D93
    pop hl
    ld b, 3
    ld de, wScore + 2
.label_1DA2
    ld a, [de]
    and a, $F0
    jr nz, .label_1DB1
    ld a, [de]
    and a, $0F
    jr nz, .label_1DB7
    dec e
    dec b
    jr nz, .label_1DA2
    ret

.label_1DB1
    ld a, [de]
    and a, $F0
    swap a
    ldi [hl], a
.label_1DB7
    ld a, [de]
    and a, $0F
    ldi [hl], a
    dec e
    dec b
    jr nz, .label_1DB1
    ret

GameState_0B::
    ldh a, [hTimer1]
    and a
    ret nz
    ld a, 1
    ld [$C0C6], a
    ld a, 5
    ldh [hTimer1], a
    ret

GameState_22::
    ldh a, [hTimer1]
    and a
    ret nz
    ld hl, $C802
    ld de, DancersTilemap
    call LoadPlayingFieldTilemap
    call ClearObjects
    ld hl, $C200
    ld de, DancerSprites
    ld c, 10
    call LoadSprites
    ld a, $10           ; Pallete number 1
    ld hl, $C266        ; OAM attributes of 6th sprite?
    ld [hl], a
    ld l, $76           ; 7th sprite
    ld [hl], a
    ld hl, $C20E
    ld de, .data_1E31
    ld b, 10
.animationLengthsLoop
    ld a, [de]
    ldi [hl], a
    ldi [hl], a
    inc de
    push de
    ld de, $000E
    add hl, de
    pop de
    dec b
    jr nz, .animationLengthsLoop
    ldh a, [hTypeBStartHeight]
    cp a, 5
    jr nz, .makeDancersVisible
    ld a, 9
.makeDancersVisible
    inc a               ; One more dancer than the starting height, unless
    ld b, a             ; you start at height 5, in which case you get 10
    ld hl, $C200
    ld de, $0010
    xor a
.dancerVisibilityLoop
    ld [hl], a
    add hl, de
    dec b
    jr nz, .dancerVisibilityLoop
    ldh a, [hTypeBStartHeight]
    add a, $0A          ; Jingles 0A to 0F sound better and better
    ld [$DFE8], a
    ld a, $25           ; TODO why?
    ldh [hLines], a
    ld a, 27            ; Almost half a second. Super random number
    ldh [hTimer1], a
    ld a, $23
    ldh [hGameState], a
    ret

; Animation lengths
.data_1E31
    db $1C, $0F, $1E, $32, $20, $18, $26, $1D, $28, $2B

Label_1E3B::
    ld a, $0A
    call Call_2673
    ret

GameState_23::
    ldh a, [hTimer1]
    cp a, 20
    jr z, Label_1E3B    ; TODO
    and a
    ret nz
    ld hl, $C20E
    ld de, $0010
    ld b, 10
.animateDancersLoop
    push hl
    dec [hl]
    jr nz, .nextDancer
    inc l
    ldd a, [hl]         ; Restart animation timer
    ld [hl], a
    ld a, l
    and a, $F0          ; A XOR %1001 would have sufficed, but whatever
    or a, $03
    ld l, a
    ld a, [hl]
    xor a, $01          ; Switch between two sprites
    ld [hl], a
    cp a, $50
    jr z, .jumpDown
    cp a, $51
    jr z, .jumpUp
.nextDancer
    pop hl
    add hl, de
    dec b
    jr nz, .animateDancersLoop
    ld a, 10
    call Call_2673
    ld a, [$DFE9]       ; Keep animating until the music stops
    and a
    ret nz
    call ClearObjects
    ldh a, [hTypeBStartHeight]
    cp a, 5
    ld a, $26           ; Launch the Buran
    jr z, .nextState
    ld a, $05           ; Show the scoreboard
.nextState          ; Launch the Buran
    ldh [hGameState], a
    ret

.jumpDown
    dec l
    dec l
    ld [hl], $67
    jr .nextDancer

.jumpUp
    dec l
    dec l
    ld [hl], $5D
    jr .nextDancer

Label_1E95::
    xor a
    ld [$C0C6], a
    ld de, $C0C0
    ld a, [de]
    ld l, a
    inc de
    ld a, [de]
    ld h, a
    or l
    jp z, $263A         ; What? Bug
    dec hl
    ld a, h
    ld [de], a
    dec de
    ld a, l
    ld [de], a
    ld de, 1
    ld hl, $C0C2
    push de
    call AddScore
    ld de, $C0C4
    ld hl, $99A5
    call PrintScore
    xor a
    ldh [hTimer1], a
    pop de
    ld hl, wScore
    call AddScore
    ld de, $C0A2
    ld hl, $9A25
    call $2A3A
    ld a, $02
    ld [$DFE0], a
    ret

Call_1ED7::
    ld a, [$C0C6]
    and a
    ret z
    ld a, [$C0C5]
    cp a, 4
    jr z, Label_1E95
    ld de, $0040
    ld bc, $9823
    ld hl, $C0AC
    and a
    jr z, .label_1F12
    ld de, $0100
    ld bc, $9883
    ld hl, $C0B1
    cp a, 1
    jr z, .label_1F12
    ld de, $0300
    ld bc, $98E3
    ld hl, $C0B6
    cp a, 2
    jr z, .label_1F12
    ld de, $1200
    ld bc, $9943
    ld hl, $C0BB
.label_1F12
    call $25D9
    ret

GameState_0C::
    ldh a, [hJoyPressed]
    and a
    ret z
    ld a, $02
    ldh [hGameState], a
    ret

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
    call FillPlayingFieldAndWipe
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
    ld b, $58           ; Rocket sprite
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

FillPlayingFieldAndWipe::
    push af
    ld a, $02           ; Start a wipe after his
    ldh [hWipeCounter], a
    pop af
.fill
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

; This is a big one!
NextPiece::
    ld hl, $C200        ; Active piece
    ld [hl], $00
    inc l
    ld [hl], $18
    inc l
    ld [hl], $3F        ; Middle of the top of the playing field
    inc l
    ld a, [$C210 + 3]   ; Preview piece
    ld [hl], a          ; Make the preview piece the active piece
    and a, ~%11         ; The lower two bits are used for orientation
    ld c, a
    ldh a, [hDemoNumber]
    and a
    jr nz, .deterministicChoice
    ldh a, [hIsMultiplayer]
    and a
    jr z, .randomChoice
.deterministicChoice
    ld h, HIGH(wPieceList)
    ldh a, [hNumPiecesPlayed]
    ld l, a
    ld e, [hl]
    inc hl
    ld a, h             ; TODO $C4?
    cp a, $C4           ; After 256 pieces, restart from the beginning. This is
    jr nz, .label_2033  ; impossible to achieve, a 2P game stops after 30 lines
    ld hl, wPieceList   ; which can be done with 30*10/4 = 75 pieces. Even
.label_2033             ; filling up the rest of the playing field, it's not
    ld a, l             ; even close. Still, good on them to program defensively
    ldh [hNumPiecesPlayed], a
    ldh a, [$D3]
    and a
    jr z, .setupPiece
    or a, %10000000
    ldh [$D3], a
    jr .setupPiece

.randomChoice
    ld h, 3
.tryAgain
    ldh a, [rDIV]       ; "Random" number
    ld b, a
.wrap                   ; This loop in effect does a multiplication of the
    xor a               ; random number by 4 modulo 28. Why 28? Because there
.loop                   ; are 7 pieces with 4 orientations each
    dec b
    jr z, .checkRandomizer
    inc a               ; Why not ADD A, 4? Bug? Or a way to waste time to
    inc a               ; allow the DIV register to increment?
    inc a
    inc a
    cp a, 7 * 4
    jr z, .wrap
    jr .loop

; At this point, the active piece has been overwritten by the preview piece,
; and the preview piece will soon be set to the next preview piece (which is
; invisible to the player). The randomizer has to accept or reject a candidate
; for the new next preview piece. It does that by bitwise OR'ing the preview
; piece, the next preview piece and the candidate together, and rejecting up
; to 2 times if the result matches the current preview piece.
; TODO: This differs from what is described at this url
; https://tetris.fandom.com/wiki/Random_Generator
; who is right?
; TODO Any notes on the distribution?
.checkRandomizer
    ld d, a
    ldh a, [hNextPreviewPiece]
    ld e, a
    dec h
    jr z, .acceptPiece  ; A contains the next preview piece
    or d                ; D contains the candidate next preview piece
    or c                ; C contains the preview piece
    and a, ~%11
    cp c
    jr z, .tryAgain
.acceptPiece
    ld a, d
    ldh [hNextPreviewPiece], a
.setupPiece
    ld a, e
    ld [$C210 + 3], a
    call Call_2696
    ldh a, [hFramesPerDrop]
    ldh [hDropTimer], a
    ret

INCBIN "baserom.gb", $2071, $21E0 - $2071

Call_21E0::
    ldh a, [$98]
    cp a, 3
    ret nz
    ldh a, [hTimer1]
    and a
    ret nz
    ld de, $C0A3
    ldh a, [$FF9C]
    bit 0, a
    jr nz, .label_222E
    ld a, [de]
    and a
    jr z, .label_2248
.label_21F6
    sub a, $30
    ld h, a
    inc de
    ld a, [de]
    ld l, a
    ldh a, [$9C]
    cp a, 6
    ld a, $8C
    jr nz, .label_2206
    ld a, " "
.label_2206
    ld c, 10
.loop
    ldi [hl], a
    dec c
    jr nz, .loop
    inc de
    ld a, [de]
    and a
    jr nz, .label_21F6
.label_2211
    ldh a, [$9C]
    inc a
    ldh [$9C], a
    cp a, 7
    jr z, .label_221F
    ld a, 10
    ldh [hTimer1], a
    ret

.label_221F
    xor a
    ldh [$9C], a
    ld a, 13
    ldh [hTimer1], a
    ld a, 1
    ldh [hWipeCounter], a
.label_222A
    xor a
    ldh [$98], a
    ret

.label_222E
    ld a, [de]
    ld h, a
    sub a, $30
    ld c, a
    inc de
    ld a, [de]
    ld l, a
    ld b, 10
.label_2238
    ld a, [hl]
    push hl
    ld h, c
    ld [hl], a
    pop hl
    inc hl
    dec b
    jr nz, .label_2238
    inc de
    ld a, [de]
    and a
    jr nz, .label_222E
    jr .label_2211

.label_2248
    call NextPiece
    jr .label_222A

Call_224D::
    ldh a, [hTimer1]
    and a
    ret nz
    ldh a, [hWipeCounter]
    cp a, 1
    ret nz
    ld de, $C0A3
    ld a, [de]
.label_225A
    ld h, a
    inc de
    ld a, [de]
    ld l, a
    push de
    push hl
    ld bc, -$20
    add hl, bc
    pop de
.label_2265
    push hl
    ld b, 10
.loop
    ldi a, [hl]
    ld [de], a
    inc de
    dec b
    jr nz, .loop
    pop hl
    push hl
    pop de
    ld bc, -$20
    add hl, bc
    ld a, h
    cp a, $C7
    jr nz, .label_2265
    pop de
    inc de
    ld a, [de]
    and a
    jr nz, .label_225A
    ld hl, $C802
    ld a, " "
    ld b, 10
.loop2
    ldi [hl], a
    dec b
    jr nz, .loop2
    call Call_2293
    ld a, 2
    ldh [hWipeCounter], a
    ret

Call_2293::
    ld hl, $C0A3
    xor a
    ld b, 9
.loop
    ldi [hl], a
    dec b
    jr nz, .loop
    ret

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
    call NextPiece
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
    call Call_249D
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
    call LookupGravity
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

INCBIN "baserom.gb", $24BB, $2671 - $24BB

Call_2671::
    ld a, 2
Call_2673::             ; TODO
    ldh [hSpriteRendererCount], a
    xor a
    ldh [hSpriteRendererOAMLo], a
    ld a, HIGH(wOAMBuffer)
    ldh [hSpriteRendererOAMHi], a
    ld hl, $C200
    call RenderSprites
    ret

Call_2683::
    ld a, 1
    ldh [hSpriteRendererCount], a
    ld a, $10
    ldh [hSpriteRendererOAMLo], a
    ld a, HIGH(wOAMBuffer)
    ldh [hSpriteRendererOAMHi], a
    ld hl, $C200
    call RenderSprites
    ret

Call_2696::
    ld a, 1
    ldh [hSpriteRendererCount], a
    ld a, $20           ; TODO doesn't want priority?
    ldh [hSpriteRendererOAMLo], a
    ld a, HIGH(wOAMBuffer)
    ldh [hSpriteRendererOAMHi], a
    ld hl, $C210
    call RenderSprites
    ret

Call_26A9::
    ld b, $20
    ld a, $8E
    ld de, $0020
.loop
    ld [hl], a
    add hl, de
    dec b
    jr nz, .loop
    ret

; Name
CopyUntilFF::
.loop
    ld a, [de]
    cp a, $FF
    ret z
    ldi [hl], a
    inc de
    jr .loop

InterruptHandlerStub::
    reti

ActivePieceSprite::
    db $00, $18, $3F, $00, $80, $00, $00, $FF

PreviewPieceSprite::
    db $00, $80, $8F, $00, $80, $00, $00, $FF

; First config screen sprites
Data_26CF::
    db $00, $70, $37, $1C, $00, $00
    db $00, $38, $37, $1C, $00, $00

; Type A difficulty selection sprite
Data_26DB::
    db $00, $40, $34, $20, $00, $00

; Type B difficulty selection sprites
Data_26E1::
    db $00, $40, $1C, $20, $00, $00
    db $00, $40, $74, $20, $00, $00

Data_26ED::
    db $00, $40, $68, $21, $00, $00
    db $00, $78, $68, $21, $00, $00

INCBIN "baserom.gb", $26F9, $2735 - $26F9

DancerSprites::
    db $80, $3F, $40, $44, $00, $00
    db $80, $3F, $20, $4A, $00, $00
    db $80, $3F, $30, $46, $00, $00
    db $80, $77, $20, $48, $00, $00
    db $80, $87, $48, $4C, $00, $00
    db $80, $87, $58, $4E, $00, $00
    db $80, $67, $4D, $50, $00, $00
    db $80, $67, $5D, $52, $00, $00
    db $80, $8F, $88, $54, $00, $00
    db $80, $8F, $98, $55, $00, $00

Data_2771::
    db $00, $5F, $57, $2C, $00, $00 ; Buran
    db $80, $80, $50, $34, $00, $00 ; Left big smoke
    db $80, $80, $60, $34, $00, $20 ; Right big smoke

Data_2783::
    db $00, $6F, $57, $58, $00, $00 ; Rocket
    db $80, $80, $55, $34, $00, $00 ; Left smoke
    db $80, $80, $5B, $34, $00, $20 ; Right smoke

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

LoadGameplayTiles::
    call LoadFontTiles
    ld bc, 10 * 16      ; Some tiles used in config screens? One too many, bug?
    call CopyData
    ld hl, GameplayTiles    ; TODO config tiles too?
    ld de, $8300
    ld bc, 208 * 16     ; Even though there are only 197 tiles in the tileset
    call CopyData
    ret

LoadFontTiles::
    ld hl, FontTiles
    ld bc, 39 * 8
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

LoadCopyrightAndTitleScreenTiles::
    call LoadFontTiles
    ld bc, 218 * 16     ; Bug? This seems way too much, copying garbage data
    call CopyData
    ret

LoadTilesFromHL::
    ld bc, $1000
.loadBCBytes
    ld de, $8000
    call CopyData
    ret

; Todo name
LoadTilemap::
.to9800
    ld hl, $9800
.toHL
    ld b, SCRN_Y_B
.columnLoop
    push hl
    ld c, SCRN_X_B
.rowLoop
    ld a, [de]
    ldi [hl], a
    inc de
    dec c
    jr nz, .rowLoop
    pop hl
    push de
    ld de, SCRN_VX_B
    add hl, de
    pop de
    dec b
    jr nz, .columnLoop
    ret

; Load from DE to tilemap at HL until a $FF is read. Initiates a wipe
LoadPlayingFieldTilemap::
.columnLoop
    ld b, 10            ; Playing field width
    push hl
.rowLoop
    ld a, [de]
    cp a, $FF           ; Sentinel value
    jr z, .startWipe
    ldi [hl], a
    inc de
    dec b
    jr nz, .rowLoop
    pop hl
    push de
    ld de, $0020        ; TODO
    add hl, de
    pop de
    jr .columnLoop

.startWipe
    pop hl
    ld a, $02
    ldh [hWipeCounter], a
    ret

; Disabling the LCD *must* be performed during VBlank only,
; to prevent damaging the hardware
DisableLCD::
    ldh a, [rIE]
    ldh [hSavedIE], a
    res 0, a            ; Bit 0 is VBlank
    ldh [rIE], a
.wait
    ldh a, [rLY]
    cp a, $91           ; TODO
    jr nz, .wait
    ldh a, [rLCDC]
    and a, $FF ^ LCDCF_ON
    ldh [rLCDC], a
    ldh a, [hSavedIE]
    ldh [rIE], a
    ret

PauseMessageTilemap::
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

ScoreboardTilemap::
    db "single    "
    db " 0 × 40   "
    db "        0 "
    db "double    "
    db " 0 × 100  "
    db "        0 "
    db "triple    "
    db " 0 × 300  "
    db "        0 "
    db "tetris    "
    db " 0 × 1200 "
    db "        0 "
    db "drops     "
    db "        0 "
    db "          "
    db "⋯⋯⋯⋯⋯⋯⋯⋯⋯⋯"
    db "this stage"
    db "        0 "
    db $FF

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
    ldh a, [$B2]
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
    ldh [$B5], a
    ld a, l
    ldh [$B4], a
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
.end

; Renders hSpriteRendererCount sprites starting from HL
RenderSprites::
.renderLoop
    ld a, h
    ldh [hSpriteRendererSpriteHi], a
    ld a, l
    ldh [hSpriteRendererSpriteLo], a
    ld a, [hl]
    and a
    jr z, .label_2AB0
    cp a, $80
    jr z, .label_2AAE
.nextSprite
    ldh a, [hSpriteRendererSpriteHi]
    ld h, a
    ldh a, [hSpriteRendererSpriteLo]
    ld l, a
    ld de, $0010
    add hl, de
    ldh a, [hSpriteRendererCount]
    dec a
    ldh [hSpriteRendererCount], a
    ret z
    jr .renderLoop

.label_2AA9
    xor a
    ldh [hSpriteRendererVisible], a
    jr .nextSprite

.label_2AAE
    ldh [hSpriteRendererVisible], a
.label_2AB0
    ld b, 7
    ld de, $FF86        ; TODO
.copyLoop
    ldi a, [hl]
    ld [de], a
    inc de
    dec b
    jr nz, .copyLoop
    ldh a, [$86 + 3]    ; Sprite index
    ld hl, $2B64
    rlca                ; This is a really stupid way to multiply by 2. Bug?
    ld e, a
    ld d, $00
    add hl, de          ; HL points somewhere inside the sprite master list
    ld e, [hl]          ; An address is loaded from that list
    inc hl
    ld d, [hl]
    ld a, [de]          ; That address points to another address
    ld l, a
    inc de
    ld a, [de]
    ld h, a
    inc de
    ld a, [de]          ; Followed by two offset bytes
    ldh [hSpriteRendererOffsetY], a
    inc de
    ld a, [de]
    ldh [hSpriteRendererOffsetX], a
    ld e, [hl]          ; Now load the second-degree address
    inc hl
    ld d, [hl]
.nextObject
    inc hl
    ldh a, [$86 + 6]    ; OAM flags?
    ldh [$94], a
    ld a, [hl]
    cp a, $FF
    jr z, .label_2AA9
    cp a, $FD
    jr nz, .label_2AF4
    ldh a, [$86 + 6]
    xor a, $20
    ldh [$94], a
    inc hl
    ld a, [hl]
    jr .calculateYCoordinate

.skipObject
    inc de
    inc de
    jr .nextObject

.label_2AF4
    cp a, $FE
    jr z, .skipObject
.calculateYCoordinate
    ldh [$86 + 3], a
    ldh a, [$86 + 1]    ; Sprite Y coordinate
    ld b, a
    ld a, [de]          ; Load this tile's Y offset
    ld c, a
    ldh a, [$86 + 5]
    bit 6, a
    jr nz, .flipY
    ldh a, [hSpriteRendererOffsetY]
    add b
    adc c
    jr .storeYCoordinate

.flipY
    ld a, b
    push af
    ldh a, [hSpriteRendererOffsetY]
    ld b, a
    pop af
    sub b
    sbc c
    sbc a, $08
.storeYCoordinate
    ldh [hSpriteRendererObjY], a
    ldh a, [$86 + 2]
    ld b, a
    inc de
    ld a, [de]
    inc de
    ld c, a
    ldh a, [$86 + 5]
    bit 5, a
    jr nz, .flipX
    ldh a, [hSpriteRendererOffsetX]
    add b
    adc c
    jr .storeXCoordinate

.flipX
    ld a, b
    push af
    ldh a, [hSpriteRendererOffsetX]
    ld b, a
    pop af
    sub b
    sbc c
    sbc a, $08
.storeXCoordinate
    ldh [hSpriteRendererObjX], a
    push hl
    ldh a, [hSpriteRendererOAMHi]
    ld h, a
    ldh a, [hSpriteRendererOAMLo]
    ld l, a
    ldh a, [hSpriteRendererVisible]
    and a
    jr z, .copyToOAM
    ld a, $FF           ; Setting Y > 160 hides the sprite
    jr .copyRestToOAM

.copyToOAM
    ldh a, [hSpriteRendererObjY]
.copyRestToOAM
    ldi [hl], a
    ldh a, [hSpriteRendererObjX]
    ldi [hl], a
    ldh a, [$86 + 3]
    ldi [hl], a
    ldh a, [$94]
    ld b, a
    ldh a, [$86 + 5]
    or b
    ld b, a
    ldh a, [$86 + 4]
    or b
    ldi [hl], a
    ld a, h
    ldh [hSpriteRendererOAMHi], a
    ld a, l
    ldh [hSpriteRendererOAMLo], a
    pop hl
    jp .nextObject

    dw $2C20
INCBIN "baserom.gb", $2B66, $2C20 - $2B66

    dw $2D58
    db -$11, -$10

INCBIN "baserom.gb", $2C24, $2D58 - $2C24

    dw $31A9
    db $FE, $FE, $FE, $FE
    db $FE, $FE, $FE, $FE
    db $84, $84, $84, $FE
    db $84, $FF

INCBIN "baserom.gb", $2D68, $31A9 - $2D68

    db $00, $00, $00, $08, $00, $10, $00, $18
    db $08, $00, $08, $08, $08, $10, $08, $18
    db $10, $00, $10, $08, $10, $10, $10, $18
    db $18, $00, $18, $08, $18, $10, $18, $18

INCBIN "baserom.gb", $31C9, $323F - $31C9

GameplayTiles::
INCBIN "gfx/configandgameplay.2bpp"

TypeAGameplayTilemap::
    db $2A, $7B, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7B, $30, $31, $31, $31, $31, $31, $32
    db $2A, $7C, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7C, $44, $1C, $0C, $18, $1B, $0E, $45
    db $2A, $7D, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7D, $67, $46, $46, $46, $46, $46, $68
    db $2A, $7B, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7B, $2F, $2F, $2F, $2F, $2F, $00, $2F
    db $2A, $7C, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7C, $43, $34, $34, $34, $34, $34, $34
    db $2A, $7D, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7D, $30, $31, $31, $31, $31, $31, $32
    db $2A, $7B, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7B, $36, $15, $0E, $1F, $0E, $15, $37
    db $2A, $7C, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7C, $36, $2F, $2F, $2F, $2F, $2F, $37
    db $2A, $7D, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7D, $40, $42, $42, $42, $42, $42, $41
    db $2A, $7B, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7B, $36, $15, $12, $17, $0E, $1C, $37
    db $2A, $7C, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7C, $36, $2F, $2F, $2F, $2F, $2F, $37
    db $2A, $7D, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7D, $33, $34, $34, $34, $34, $34, $35
    db $2A, $7B, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7B, $2B, $38, $39, $39, $39, $39, $3A
    db $2A, $7C, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7C, $2B, $3B, $2F, $2F, $2F, $2F, $3C
    db $2A, $7D, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7D, $2B, $3B, $2F, $2F, $2F, $2F, $3C
    db $2A, $7B, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7B, $2B, $3B, $2F, $2F, $2F, $2F, $3C
    db $2A, $7C, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7C, $2B, $3B, $2F, $2F, $2F, $2F, $3C
    db $2A, $7D, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7D, $2B, $3D, $3E, $3E, $3E, $3E, $3F

TypeBGameplayTilemap::
    db $2A, $7B, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7B, $30, $31, $31, $31, $31, $31, $32
    db $2A, $7C, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7C, $36, $15, $0E, $1F, $0E, $15, $37
    db $2A, $7D, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7D, $36, $2F, $2F, $2F, $2F, $2F, $37
    db $2A, $7B, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7B, $40, $42, $42, $42, $42, $42, $41
    db $2A, $7C, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7C, $36, $11, $12, $10, $11, $2F, $37
    db $2A, $7D, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7D, $36, $2F, $2F, $2F, $2F, $2F, $37
    db $2A, $7B, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7B, $33, $34, $34, $34, $34, $34, $35
    db $2A, $7C, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7C, $2B, $8E, $8E, $8E, $8E, $8E, $8E
    db $2A, $7D, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7D, $30, $31, $31, $31, $31, $31, $32
    db $2A, $7B, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7B, $36, $15, $12, $17, $0E, $1C, $37
    db $2A, $7C, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7C, $36, $2F, $2F, $02, $05, $2F, $37
    db $2A, $7D, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7D, $33, $34, $34, $34, $34, $34, $35
    db $2A, $7B, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7B, $2B, $38, $39, $39, $39, $39, $3A
    db $2A, $7C, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7C, $2B, $3B, $2F, $2F, $2F, $2F, $3C
    db $2A, $7D, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7D, $2B, $3B, $2F, $2F, $2F, $2F, $3C
    db $2A, $7B, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7B, $2B, $3B, $2F, $2F, $2F, $2F, $3C
    db $2A, $7C, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7C, $2B, $3B, $2F, $2F, $2F, $2F, $3C
    db $2A, $7D, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7D, $2B, $3D, $3E, $3E, $3E, $3E, $3F

FontTiles::
INCBIN "gfx/font.1bpp"

CopyrightAndTitleScreenTiles::
INCBIN "gfx/copyrightandtitlescreen.2bpp"

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

TitleScreenTilemap::
    db $8E, $8E, $8E, $8E, $8E, $8E, $8E, $8E, $8E, $8E, $8E, $8E, $8E, $8E, $8E, $8E, $8E, $8E, $8E, $8E
    db $5A, $5B, $5B, $5B, $5B, $5B, $5B, $5B, $5B, $5B, $5B, $5B, $5B, $5B, $5B, $5B, $5B, $5B, $5B, $5C
    db $5D, $80, $81, $82, $83, $90, $91, $92, $81, $82, $83, $90, $6C, $6D, $6E, $6F, $70, $71, $72, $5E
    db $5D, $84, $85, $86, $87, $93, $94, $95, $85, $86, $87, $93, $73, $74, $75, $76, $77, $78, $2F, $5E
    db $5D, $2F, $88, $89, $2F, $96, $97, $98, $88, $89, $2F, $96, $79, $7A, $7B, $7C, $7D, $7E, $2F, $5E
    db $5D, $2F, $8A, $8B, $2F, $8E, $8F, $6B, $8A, $8B, $2F, $8E, $7F, $66, $67, $68, $69, $6A, $2F, $5E
    db $5F, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $61
    db $8E, $3C, $3C, $3C, $3C, $3C, $3C, $3C, $3C, $3C, $3C, $3C, $3C, $3C, $3D, $3E, $3C, $3C, $3C, $8E
    db $8E, $8C, $8C, $62, $63, $8C, $8C, $3A, $8C, $8C, $8C, $8C, $8C, $3A, $42, $43, $3B, $8C, $8C, $8E
    db $8E, $3A, $8C, $64, $65, $8C, $8C, $8C, $8C, $3B, $8C, $8C, $8C, $8C, $44, $45, $8C, $8C, $8C, $8E
    db $8E, $8C, $8C, $8C, $8C, $8C, $8C, $8C, $8C, $8C, $8C, $8C, $8C, $46, $47, $48, $49, $3F, $40, $8E
    db $8E, $8C, $8C, $8C, $8C, $3A, $8C, $8C, $8C, $8C, $53, $54, $8C, $4A, $4B, $4C, $4D, $42, $43, $8E
    db $8E, $8C, $8C, $8C, $8C, $8C, $8C, $8C, $8C, $54, $55, $56, $57, $4E, $4F, $50, $51, $52, $45, $8E
    db $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41
    db $2F, $2F, $59, $19, $15, $0A, $22, $0E, $1B, $2F, $2F, $2F, $99, $19, $15, $0A, $22, $0E, $1B, $2F
    db $2F, $2F, $9A, $9A, $9A, $9A, $9A, $9A, $9A, $2F, $2F, $2F, $9A, $9A, $9A, $9A, $9A, $9A, $9A, $2F
    db $2F, $2F, $2F, $2F, $33, $30, $31, $32, $31, $2F, $34, $35, $36, $37, $38, $39, $2F, $2F, $2F, $2F
    db $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F

ConfigScreenTilemap::
    db $47, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $49
    db $4A, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $50, $51, $51, $51, $51, $51, $51, $51, $51, $51, $52, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $53, $10, $0A, $16, $0E, $2F, $1D, $22, $19, $0E, $54, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $55, $56, $6D, $58, $58, $58, $58, $58, $A9, $58, $58, $58, $6E, $56, $56, $5A, $2C, $4B
    db $4A, $2C, $5B, $78, $77, $7E, $7F, $9A, $9B, $2F, $AA, $79, $77, $7E, $7F, $9A, $9B, $5C, $2C, $4B
    db $4A, $2C, $2D, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $AC, $4F, $4F, $4F, $4F, $4F, $4F, $2E, $2C, $4B
    db $4A, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $50, $51, $51, $51, $51, $51, $51, $51, $51, $51, $51, $52, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $53, $16, $1E, $1C, $12, $0C, $2F, $1D, $22, $19, $0E, $54, $2C, $2C, $2C, $4B
    db $4A, $2C, $55, $56, $6D, $58, $58, $58, $58, $58, $A9, $58, $58, $58, $58, $6E, $56, $5A, $2C, $4B
    db $4A, $2C, $5B, $78, $77, $7E, $7F, $9A, $9B, $2F, $AA, $79, $77, $7E, $7F, $9A, $9B, $5C, $2C, $4B
    db $4A, $2C, $71, $72, $72, $72, $72, $72, $72, $72, $AB, $72, $72, $72, $72, $72, $72, $74, $2C, $4B
    db $4A, $2C, $5B, $7A, $77, $7E, $7F, $9A, $9B, $2F, $AA, $2F, $9D, $9C, $9C, $2F, $2F, $5C, $2C, $4B
    db $4A, $2C, $2D, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $AC, $4F, $4F, $4F, $4F, $4F, $4F, $2E, $2C, $4B
    db $4A, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $4B
    db $4C, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4E

TypeADifficultyTilemap::
    db $47, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $49
    db $4A, $2F, $0A, $25, $1D, $22, $19, $0E, $2F, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $2C, $2C, $50, $51, $51, $51, $51, $51, $52, $2C, $2C, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $2C, $2C, $53, $15, $0E, $1F, $0E, $15, $54, $2C, $2C, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $55, $56, $57, $58, $6C, $58, $6C, $58, $59, $56, $5A, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $5B, $90, $6F, $91, $6F, $92, $6F, $93, $6F, $94, $5C, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $71, $72, $73, $72, $73, $72, $73, $72, $73, $72, $74, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $5B, $95, $6F, $96, $6F, $97, $6F, $98, $6F, $99, $5C, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $2D, $4F, $6B, $4F, $6B, $4F, $6B, $4F, $6B, $4F, $2E, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $50, $51, $51, $51, $51, $51, $51, $51, $51, $51, $52, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $53, $1D, $18, $19, $25, $1C, $0C, $18, $1B, $0E, $54, $2C, $2C, $2C, $2C, $4B
    db $4A, $55, $56, $70, $6D, $58, $58, $58, $58, $58, $58, $58, $58, $58, $6E, $56, $56, $56, $5A, $4B
    db $4A, $5B, $01, $6F, $60, $60, $60, $60, $60, $60, $2F, $2F, $60, $60, $60, $60, $60, $60, $5C, $4B
    db $4A, $5B, $02, $6F, $60, $60, $60, $60, $60, $60, $2F, $2F, $60, $60, $60, $60, $60, $60, $5C, $4B
    db $4A, $5B, $03, $6F, $60, $60, $60, $60, $60, $60, $2F, $2F, $60, $60, $60, $60, $60, $60, $5C, $4B
    db $4A, $2D, $4F, $6B, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $2E, $4B
    db $4C, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4E

TypeBDifficultyTilemap::
    db $47, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $49
    db $4A, $2F, $0B, $25, $1D, $22, $19, $0E, $2F, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $50, $51, $51, $51, $51, $51, $52, $2C, $2C, $50, $51, $51, $51, $51, $52, $2C, $4B
    db $4A, $2C, $2C, $53, $15, $0E, $1F, $0E, $15, $54, $2C, $2C, $53, $11, $12, $10, $11, $54, $2C, $4B
    db $4A, $55, $56, $57, $58, $6C, $58, $6C, $58, $59, $56, $5A, $75, $58, $6C, $58, $6C, $6E, $5A, $4B
    db $4A, $5B, $90, $6F, $91, $6F, $92, $6F, $93, $6F, $94, $5C, $5B, $90, $6F, $91, $6F, $92, $5C, $4B
    db $4A, $71, $72, $73, $72, $73, $72, $73, $72, $73, $72, $74, $71, $72, $73, $72, $73, $72, $74, $4B
    db $4A, $5B, $95, $6F, $96, $6F, $97, $6F, $98, $6F, $99, $5C, $5B, $93, $6F, $94, $6F, $95, $5C, $4B
    db $4A, $2D, $4F, $6B, $4F, $6B, $4F, $6B, $4F, $6B, $4F, $2E, $2D, $4F, $6B, $4F, $6B, $4F, $2E, $4B
    db $4A, $2C, $2C, $2C, $50, $51, $51, $51, $51, $51, $51, $51, $51, $51, $52, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $53, $1D, $18, $19, $25, $1C, $0C, $18, $1B, $0E, $54, $2C, $2C, $2C, $2C, $4B
    db $4A, $55, $56, $70, $6D, $58, $58, $58, $58, $58, $58, $58, $58, $58, $6E, $56, $56, $56, $5A, $4B
    db $4A, $5B, $01, $6F, $60, $60, $60, $60, $60, $60, $2F, $2F, $60, $60, $60, $60, $60, $60, $5C, $4B
    db $4A, $5B, $02, $6F, $60, $60, $60, $60, $60, $60, $2F, $2F, $60, $60, $60, $60, $60, $60, $5C, $4B
    db $4A, $5B, $03, $6F, $60, $60, $60, $60, $60, $60, $2F, $2F, $60, $60, $60, $60, $60, $60, $5C, $4B
    db $4A, $2D, $4F, $6B, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F, $2E, $4B
    db $4C, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4E

DancersTilemap::
    db $CD, $CD, $CD, $CD, $CD, $CD, $CD, $CD, $CD, $CD
    db $8C, $C9, $CA, $8C, $8C, $8C, $8C, $8C, $8C, $8C
    db $8C, $CB, $CC, $8C, $8C, $8C, $8C, $8C, $8C, $CE
    db $D7, $D7, $D7, $D7, $D7, $D7, $D7, $D7, $D7, $CF
    db $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $D0
    db $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $D1, $D2
    db $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $D3, $D4
    db $7C, $7C, $7C, $7C, $7C, $7C, $2F, $2F, $D5, $D6
    db $7D, $7D, $7D, $7D, $2F, $2F, $2F, $2F, $D8, $2F
    db $7B, $7B, $7B, $7B, $2F, $2F, $2F, $2F, $D8, $2F
    db $7C, $7C, $7C, $7C, $2F, $2F, $2F, $2F, $D8, $2F
    db $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $D8, $2F
    db $2F, $2F, $2F, $2F, $2F, $2F, $7C, $7C, $7C, $7C
    db $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7C
    db $7D, $7D, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $7D
    db $2F, $2F, $2F, $D9, $2F, $2F, $2F, $2F, $2F, $7B
    db $B7, $B8, $D9, $B7, $2F, $7C, $7C, $7C, $7C, $7C
    db $7D, $7D, $7D, $7D, $7D, $7D, $7D, $7D, $7D, $7D
    db $FF

BuranBackdropTilemap::
    db $4A, $4A, $4A, $4A, $4A, $4A, $59, $69, $69, $69, $69, $69, $69, $49, $4A, $4A, $4A, $4A, $4A, $4A
    db $5A, $5A, $5A, $5A, $5A, $5A, $85, $85, $85, $85, $85, $85, $85, $85, $5A, $5A, $38, $39, $38, $5A
    db $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A
    db $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07

MultiplayerDifficultyTilemap::
    db $47, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $48, $49
    db $4A, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $16, $0A, $1B, $12, $18, $2F, $1F, $1C, $24, $15, $1E, $12, $10, $12, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $50, $51, $51, $51, $51, $52, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $53, $11, $12, $10, $11, $54, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $55, $56, $56, $5A, $2C, $2C, $2C, $75, $58, $6C, $58, $6C, $6E, $5A, $2C, $2C, $4B
    db $4A, $2C, $2C, $5B, $2F, $2F, $5C, $2C, $2C, $2C, $5B, $90, $6F, $91, $6F, $92, $5C, $2C, $2C, $4B
    db $4A, $2C, $2C, $5B, $2F, $2F, $5C, $2C, $2C, $2C, $71, $72, $73, $72, $73, $72, $74, $2C, $2C, $4B
    db $4A, $2C, $2C, $2D, $4F, $4F, $2E, $2C, $2C, $2C, $5B, $93, $6F, $94, $6F, $95, $5C, $2C, $2C, $4B
    db $4A, $2C, $2C, $16, $0A, $1B, $12, $18, $2C, $2C, $2D, $4F, $6B, $4F, $6B, $4F, $2E, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $50, $51, $51, $51, $51, $52, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $2C, $53, $11, $12, $10, $11, $54, $2C, $2C, $2C, $4B
    db $4A, $2C, $2C, $55, $56, $56, $5A, $2C, $2C, $2C, $75, $58, $6C, $58, $6C, $6E, $5A, $2C, $2C, $4B
    db $4A, $2C, $2C, $5B, $2F, $2F, $5C, $2C, $2C, $2C, $5B, $90, $6F, $91, $6F, $92, $5C, $2C, $2C, $4B
    db $4A, $2C, $2C, $5B, $2F, $2F, $5C, $2C, $2C, $2C, $71, $72, $73, $72, $73, $72, $74, $2C, $2C, $4B
    db $4A, $2C, $2C, $2D, $4F, $4F, $2E, $2C, $2C, $2C, $5B, $93, $6F, $94, $6F, $95, $5C, $2C, $2C, $4B
    db $4A, $2C, $2C, $15, $1E, $12, $10, $12, $2C, $2C, $2D, $4F, $6B, $4F, $6B, $4F, $2E, $2C, $2C, $4B
    db $4C, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4D, $4E

MultiplayerGameplayTilemap::
    db $8E, $B2, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B3, $30, $31, $31, $31, $31, $31, $32
    db $8E, $B0, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B5, $36, $2F, $2F, $2F, $2F, $2F, $37
    db $8E, $B0, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B5, $36, $2F, $2F, $2F, $2F, $2F, $37
    db $8E, $B0, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B5, $40, $42, $42, $42, $42, $42, $41
    db $8E, $B0, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B5, $36, $11, $12, $10, $11, $2F, $37
    db $8E, $B0, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B5, $36, $2F, $2F, $2F, $2F, $2F, $37
    db $8E, $B0, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B5, $33, $34, $34, $34, $34, $34, $35
    db $8E, $B0, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B5, $2B, $8E, $8E, $8E, $8E, $8E, $8E
    db $8E, $B0, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B5, $30, $31, $31, $31, $31, $31, $32
    db $8E, $B0, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B5, $36, $15, $12, $17, $0E, $1C, $37
    db $8E, $B0, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B5, $36, $2F, $2F, $2F, $2F, $2F, $37
    db $8E, $B0, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B5, $33, $34, $34, $34, $34, $34, $35
    db $8E, $B0, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B5, $2B, $38, $39, $39, $39, $39, $3A
    db $8E, $B0, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B5, $2B, $3B, $2F, $2F, $2F, $2F, $3C
    db $8E, $B0, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B5, $2B, $3B, $2F, $2F, $2F, $2F, $3C
    db $8E, $B0, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B5, $2B, $3B, $2F, $2F, $2F, $2F, $3C
    db $8E, $B0, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B5, $2B, $3B, $2F, $2F, $2F, $2F, $3C
    db $8E, $B1, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $2F, $B4, $2B, $3D, $3E, $3E, $3E, $3E, $3F

MultiplayerVictoryTop::
    db $07, $07, $07, $07, $07, $07, $84, $87, $87, $8C, $87, $87, $8C, $87, $87, $8C, $87, $87, $86, $07
    db $07, $1E, $1E, $1E, $1E, $1E, $79, $2F, $2F, $8D, $2F, $2F, $8D, $2F, $2F, $8D, $2F, $2F, $88, $07
    db $07, $B4, $B5, $BB, $2E, $BC, $79, $2F, $2F, $8D, $2F, $2F, $8D, $2F, $2F, $8D, $2F, $2F, $88, $07
    db $07, $BF, $BF, $BF, $BF, $BF, $89, $8A, $8A, $8E, $8A, $8A, $8E, $8A, $8A, $8E, $8A, $8A, $8B, $07

MultiplayerVictoryBottom::
    db $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06
    db $16, $16, $16, $16, $16, $16, $16, $16, $16, $16, $16, $16, $16, $16, $16, $16, $16, $16, $16, $16
    db $07, $07, $07, $07, $07, $07, $84, $87, $87, $8C, $87, $87, $8C, $87, $87, $8C, $87, $87, $86, $07
    db $07, $1E, $1E, $1E, $1E, $1E, $79, $2F, $2F, $8D, $2F, $2F, $8D, $2F, $2F, $8D, $2F, $2F, $88, $07
    db $07, $BD, $B2, $2E, $BE, $2E, $79, $2F, $2F, $8D, $2F, $2F, $8D, $2F, $2F, $8D, $2F, $2F, $88, $07
    db $07, $BF, $BF, $BF, $BF, $BF, $89, $8A, $8A, $8E, $8A, $8A, $8E, $8A, $8A, $8E, $8A, $8A, $8B, $07

MultiplayerAndBuranTiles::
INCBIN "gfx/multiplayerandburan.2bpp"

ds 20, $00              ; TODO find out if this is ever used? Probably not

; TODO Can this be prettier?
TypeADemoData::
INCBIN "typeademodata.bin"
TypeBDemoData::
INCBIN "typebdemodata.bin"
DemoPieceList::
INCBIN "demopiecelist.bin"

INCBIN "baserom.gb", $6480, $8000 - $6480
; vim: set expandtab tabstop=4 shiftwidth=4 
