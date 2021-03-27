INCLUDE "hardware.inc"

; RST vectors (unused?)
SECTION "RST 0", ROM0[$0000]
    jp $020C
SECTION "RST 8", ROM0[$0008]
    jp $020C
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
    jp $017E
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
INCBIN "baserom.gb", $150, $8000 - $150
