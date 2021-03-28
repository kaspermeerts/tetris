INCLUDE "hardware.inc"

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
    ldh [$CC], a
    pop bc
    pop de
    pop hl
    pop af
    reti
SerialTableJump::
    ldh a, [$CD]
    rst $28             ; TableJump
    dw Label_78
    dw Label_9F
    dw Label_A4
    dw Label_BA
    dw $27EA            ; XXX Is this used? The target is a simple ret

Label_78:
    ldh a, [$E1]
    cp a, 7
    jr z, Label_86
    cp a, 6
    ret z
    ld a, 6
    ldh [$E1], a
    ret

Label_86:
    ldh a, [rSB]
    cp a, $55
    jr nz, Label_94
    ld a, $29
    ldh [$CB], a
    ld a, 1
    jr Label_9C
Label_94:
    cp a, $29
    ret nz
    ld a, $55
    ldh [$CB], a
    xor a
Label_9C:
    ldh [rSC], a
    ret

Label_9F:
    ldh a, [rSB]
    ldh [$D0], a
    ret

Label_A4:
    ldh a, [rSB]
    ldh [$D0], a
    ldh a, [$CB]
    cp a, $29
    ret z
Label_AD:
    ldh a, [$CF]
    ldh [rSB], a
    ld a, $FF
    ldh [$CF], a
    ld a, $80
    ldh [rSC], a
    ret

Label_BA:
    ldh a, [rSB]
    ldh [$D0], a
    ldh a, [$CB]
    cp a, $29
    ret z
    ldh a, [$CF]
    ldh [rSB], a
    ei
    call $A98
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
    call $29E3
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
    call $FFB6
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
    ldh [$85], a
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

    call ClearBgMap
    call $7FF3          ; Init music?

    ld a, IEF_SERIAL | IEF_VBLANK
    ldh [rIE], a
    ld a, $37
    ldh [$C0], a
    ld a, $1C
    ldh [$C1], a
    ld a, $24
    ldh [$E1], a
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
    and a, $0F
    cp a, $0F           ; Reset if all buttons are pressed
    jp z, Init.softReset
    ld hl, $FFA6
    ld b, 2
.label_2DB
    ld a, [hl]
    and a
    jr z, $2E0
    dec [hl]
    inc l
    dec b
    jr nz, .label_2DB
    ldh a, [$C5]
    and a
    jr z, .label_2ED
    ld a, IEF_SERIAL | IEF_VBLANK
    ldh [rIE], a
.label_2ED
    ldh a, [$85]
    and a
    jr z, .label_2ED
    xor a
    ldh [$85], a
    jp MainLoop
.dispatch
    ldh a, [$E1]
    rst $28

dw $1BCE
dw $1CE2
dw $1244
dw $127B
dw $1D06
dw $1D26
dw $03AE
dw $0479
dw $1444
dw $148C
dw $1A07
dw $1DC0
dw $1F16
dw $1F1F
dw $1525
dw $14B0
dw $157B
dw $15BF
dw $1629
dw $167A
dw $16EB
dw $1913
dw $0677
dw $072C
dw $0825
dw $08E4
dw $0B31
dw $0CEB
dw $0AD2
dw $0D32
dw $0E23
dw $1112
dw $0D99
dw $0E8A
dw $1DCE
dw $1E41
dw $0369
dw $0393
dw $1167
dw $11E6
dw $11FC
dw $121C
dw $05C7
dw $05F7
dw $12B3
dw $1305
dw $1324
dw $1351
dw $1367
dw $137E
dw $13B5
dw $13E5
dw $131B
dw $03A0
dw $27EA

Call_369::

INCBIN "baserom.gb", $369, $2795 - $369

ClearBgMap::
    ld hl, _SCRN0 + $400 - 1    ; TODO constants
    ld bc, $400
.loop
    ld a, $2F           ; TODO charmap
    ldd [hl], a
    dec bc
    ld a, b
    or c
    jr nz, .loop
    ret

INCBIN "baserom.gb", $27A4, $29A6 - $27A4

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

INCBIN "baserom.gb", $29E3, $2A7F - $29E3

; TODO Use rgbds's LOAD block
DMARoutine:
    ld a, HIGH(wOAMBuffer)
    ldh [rDMA], a
    ld a, $28
.wait
    dec a
    jr nz, .wait
    ret
DMARoutineEnd:

INCBIN "baserom.gb", $2A89, $4000 - $2A89
INCBIN "baserom.gb", $4000, $8000 - $4000

; vim: set expandtab tabstop=4 shiftwidth=4 
