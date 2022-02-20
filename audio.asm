INCLUDE "hardware.inc"

SECTION "Audio", ROM0[$6480]

SquareSFXStartPointers::
    dw StartTinkSFX         ;  1 - Menu cursor movement, TODO name
    dw StartChangeScreenSFX ;  2 - Menu change screen
    dw StartRotatePieceSFX  ;  3 - Rotate piece
    dw StartShiftPieceSFX   ;  4 - Shift piece
    dw StartGarbageAttackSFX;  5 - Garbage attack
    dw StartLineClearSFX    ;  6 - Line clear
    dw StartTetrisSFX       ;  7 - Tetris
    dw StartLevelUpSFX      ;  8 - Level up

SquareSFXContinuePointers::
    dw ContinueTinkSFX
    dw ContinueGenericSquareSFX
    dw ContinueRotatePieceSFX
    dw ContinueGenericSquareSFX
    dw ContinueGenericSquareSFX
    dw ContinueLineClearSFX
    dw ContinueTetrisSFX
    dw ContinueLevelUpSFX

NoiseSFXStartPointers::
    dw StartStackFallSFX    ;  1 - Stack settles after a line clear
    dw StartLockPieceSFX    ;  2 - 
    dw StartIgnitionSFX     ;  3 - Rocket ignition
    dw StartLiftoffSFX      ;  4 - Rocket flying

NoiseSFXContinuePointers::
    dw ContinueGenericNoiseSFX
    dw ContinueGenericNoiseSFX
    dw ContinueGenericNoiseSFX
    dw ContinueLiftoffSFX

MusicPointers:: ; TODO
    dw $6F3F    ;  1 - Top Score
    dw $6F4A    ;  2 - Stage clear
    dw $6F55    ;  3 - Title screen
    dw $6F60    ;  4 - Game over
    dw $6F6B    ;  5 - Type A - Korobeiniki
    dw $6F76    ;  6 - Type B - ?
    dw $6F81    ;  7 - Type C - Bach, French Suite №3 in Bm, Menuet
    dw $6F8C    ;  8 - Danger, March of the Toreadors from Carmen
    dw $6F97    ;  9 - Multiplayer Round over
    dw $6FA2    ;  A - Type B Jingle #1
    dw $6FAD    ;  B - Type B Jingle #2
    dw $6FB8    ;  C - Type B Jingle #3
    dw $6FC3    ;  D - Type B Jingle #4
    dw $6FCE    ;  E - Type B Jingle #5
    dw $6FD9    ;  F - Type B Jingle #6
    dw $6FE4    ; 10 - Rocket launch
    dw $6FEF    ; 11 - Multiplayer victory

Call_64D2::
    ret

_UpdateAudio::
    push af
    push bc
    push de
    push hl
    ld a, [$DF7F]
    cp a, 1
    jr z, .pauseAudio
    cp a, 2
    jr z, .unpauseAudio
    ld a, [$DF7E]
    and a
    jr nz, .playPauseTune
.label_64E8
    ldh a, [hDemoNumber]
    and a
    jr z, .playSounds
    xor a
    ld [$DFE0], a
    ld [$DFE8], a
    ld [$DFF0], a
    ld [$DFF8], a
.playSounds
    call Call_64D2
    call PlaySquareSFX
    call PlayNoiseSFX
    call PlayWaveSFX
    call Call_6A21
    call PlayMusic
    call Call_6A65
.out
    xor a
    ld [$DFE0], a
    ld [$DFE8], a
    ld [$DFF0], a
    ld [$DFF8], a
    ld [$DF7F], a
    pop hl
    pop de
    pop bc
    pop af
    ret

.pauseAudio
    call _InitAudio.muteChannels
    xor a
    ld [$DFE1], a
    ld [$DFF1], a
    ld [$DFF9], a
    ld hl, $DFBF
    res 7, [hl]
    ld hl, $DF9F
    res 7, [hl]
    ld hl, $DFAF
    res 7, [hl]
    ld hl, $DFCF
    res 7, [hl]
    ld hl, $6EE9
    call LoadWavePattern
    ld a, $30
    ld [$DF7E], a
.playFirstNote
    ld hl, .pauseTuneFirstNoteData
    call SetupChannel.square2
    jr .out

.playSecondNote
    ld hl, .pauseTuneSecondNoteData
    jr $6553

.unpauseAudio
    xor a
    ld [$DF7E], a
    jr .label_64E8

.playPauseTune
    ld hl, $DF7E
    dec [hl]
    ld a, [hl]
    cp a, 40
    jr z, .playSecondNote
    cp a, 32
    jr z, .playFirstNote
    cp a, 24
    jr z, .playSecondNote
    cp a, 16
    jr nz, .out
    inc [hl]
    jr .out

; Exactly the same as Super Mario Land
; 50% duty cycle, 14/256 seconds, volume 15/16, decreasing envelope, 3/64 seconds per step (pointless?)
.pauseTuneFirstNoteData
    db $B2, $E3, $83, $C7   ; 1048.6 Hz ~ C6
.pauseTuneSecondNoteData
    db $B2, $E3, $C1, $C7   ; 2080.5 Hz ~ C7

Call_6583::
    ld a, [$DFF1]
    cp a, $01       ; End of Tetris sweep
    ret

Call_6589::
    ld a, [$DFE1]
    cp a, $05       ; Garbage attack sweep
    ret

Call_658F::
    ld a, [$DFE1]
    cp a, $07       ; Tetris
    ret

Call_6595::
    ld a, [$DFE1]
    cp a, $08       ; Level Up
    ret

Data_659B::
    db $00, $B5, $D0, $40, $C7
Data_65A0::
    db $00, $B5, $20, $40, $C7
Data_65A5::
    db $00, $B6, $A1, $80, $C7

StartTinkSFX::
    ld a, 5
    ld hl, Data_659B
    jp Label_6936

ContinueTinkSFX::
    call $698B
    and a
    ret nz
    ld hl ,$DFE4
    inc [hl]
    ld a, [hl]
    cp a, $02
    jr z, ContinueGenericSquareSFX.stop
    ld hl, Data_65A0
    jp SetupChannel.square1

StartChangeScreenSFX::
    ld a, 3
    ld hl, Data_65A5
    jp Label_6936

ContinueGenericSquareSFX::
    call $698B
    and a
    ret nz
.stop
    xor a
    ld [$DFE1], a
    ldh [rNR10], a
    ld a, $08
    ldh [rNR12], a
    ld a, $80
    ldh [rNR14], a
    ld hl, $DF9F
    res 7, [hl]
    ret

Data_65E7::
    db $00, $80, $E1, $C1, $87
Data_65EC::
    db $00, $80, $E1, $AC, $87

StartTetrisSFX::
    ld hl, Data_65E7
    jp Label_6936

ContinueTetrisSFX::
    ld hl, $DFE4
    inc [hl]
    ld a, [hl]
    cp a, 4
    jr z, .label_6617
    cp a, 11
    jr z, .label_661D
    cp a, 15
    jr z, .label_6617
    cp a, 24
    jp z, .label_660E   ; Should be a JR, bug
    ret

.label_660E
    ld a, $01
    ld hl, $DFF0
    ld [hl], a
    jp ContinueGenericSquareSFX.stop

.label_6617
    ld hl, Data_65EC
    jp SetupChannel.square1

.label_661D
    ld hl, Data_65E7
    jp SetupChannel.square1

Data_6623::
    db $48, $BC, $42, $66, $87

StartShiftPieceSFX::
    call Call_6583
    ret z
    call Call_6595
    ret z
    call Call_658F
    ret z
    call Call_6589
    ret z
    ld a, 2
    ld hl, Data_6623
    jp Label_6936

Data_6640::
    db $00, $B0, $F1, $B6, $C7
Data_6645::
    db $00, $B0, $F1, $C4, $C7
Data_664A::
    db $00, $B0, $F1, $CE, $C7
Data_664F::
    db $00, $B0, $F1, $DB, $C7

StartLevelUpSFX::
    call Call_658F
    ret z
    ld a, 7
    ld hl, Data_6640
    jp Label_6936

ContinueLevelUpSFX::
    call Call_698B
    and a
    ret nz
    ld hl, $DFE4
    inc [hl]
    ld a, [hl]
    cp a, 1
    jr z, .label_6680
    cp a, 2
    jr z, .label_6685
    cp a, 3
    jr z, .label_668A
    cp a, 4
    jr z, .label_668F
    cp a, 5
    jp z, ContinueGenericSquareSFX.stop
    ret

.label_6680
    ld hl, Data_6645
    jr .label_6692

.label_6685
    ld hl, Data_664A
    jr .label_6692

.label_668A
    ld hl, Data_664F
    jr .label_6692

.label_668F
    ld hl, Data_6640
.label_6692
    jp SetupChannel.square1

Data_6695::
    db $3E, $80, $E3, $00, $C4
Data_669A::
    db $93, $83, $83, $73, $63, $53, $43, $33, $23, $13, $00
Data_66A5::
    db $00, $23, $43, $63, $83, $A3, $C3, $D3, $E3, $FF

StartLineClearSFX::
    call Call_6583
    ret z
    call Call_6595
    ret z
    call Call_658F
    ret z
    ld a, 6
    ld hl, Data_6695
    jp Label_6936

ContinueLineClearSFX::
    call Call_698B
    and a
    ret nz
    ld hl, $DFE4
    ld c, [hl]
    inc [hl]
    ld b, 0
    ld hl, Data_669A
    add hl, bc
    ld a, [hl]
    and a
    jp z, ContinueGenericSquareSFX.stop
    ld e, a
    ld hl, Data_66A5
    add hl, bc
    ld a, [hl]
    ld d, a
    ld b, $86
.label_66E1
    ld c, LOW(rNR12)
    ld a, e
    ldh [c], a          ; NR12
    inc c
    ld a, d
    ldh [c], a          ; NR13
    inc c
    ld a, b
    ldh [c], a          ; NR14
    ret

Data_66EC::
    db $3B, $80, $B2, $87, $87
Data_66F1::
    db $A2, $93, $62, $43, $23, $00
Data_66F7::
    db $80, $40, $80, $40, $80

StartRotatePieceSFX::
    call Call_6583
    ret z
    call Call_6595
    ret z
    call Call_658F
    ret z
    call Call_6589
    ret z
    ld a, 3
    ld hl, Data_66EC
    jp Label_6936

ContinueRotatePieceSFX::
    call Call_698B
    and a
    ret nz
    ld hl, $DFE4
    ld c, [hl]
    inc [hl]
    ld b, $00
    ld hl, Data_66F1
    add hl, bc
    ld a, [hl]
    and a
    jp z, ContinueGenericSquareSFX.stop
    ld e, a
    ld hl, Data_66F7
    add hl, bc
    ld a, [hl]
    ld d, a
    ld b, $87
    jr ContinueLineClearSFX.label_66E1

StartGarbageAttackSFX::
    call Call_658F
    ret z
    ld a, 40
    ld hl, Data_6740
    jp Label_6936

; Sets high bit of NR10, which does nothing? Bug?
; 3/128 Hz sweep up with shift 7, starting from frequency 127.9 Hz ~ C3
Data_6740::
    db $B7, $80, $90, $FF, $83
Data_6745::
    db $00, $D1, $45, $80
Data_6749::
    db $00, $F1, $54, $80
Data_674D::
    db $00, $D5, $65, $80
Data_6751::
    db $00, $70, $66, $80

Data_6755::
    db $65, $65, $65, $64, $57, $56, $55, $54, $54, $54, $54, $54
    db $47, $46, $46, $45, $45, $45, $44, $44, $44, $34, $34, $34
    db $34, $34, $34, $34, $34, $34, $34, $34, $34, $34, $34, $34
Data_6779::
    db $70, $60, $70, $70, $70, $80, $90, $A0, $D0, $F0, $E0, $D0
    db $C0, $B0, $A0, $90, $80, $70, $60, $50, $40, $30, $30, $20
    db $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $10, $10

StartIgnitionSFX::
    ld a, 48
    ld hl, Data_674D
    jp Label_6936

StartLiftoffSFX::
    ld a, 48
    ld hl, Data_6751
    jp Label_6936

ContinueLiftoffSFX::
    call Call_698B
    and a
    ret nz
    ld hl, $DFFC
    ld a, [hl]
    ld c, a
    cp a, 36
    jp z, ContinueGenericNoiseSFX.stop
    inc [hl]
    ld b, $00
    push bc
    ld hl, Data_6755
    add hl, bc
    ld a, [hl]
    ldh [rNR43], a
    pop bc
    ld hl, Data_6779
    add hl, bc
    ld a, [hl]
    ldh [rNR42], a
    ld a, $80
    ldh [rNR44], a
    ret

StartStackFallSFX::
    ld a, 32
    ld hl, Data_6749
    jp Label_6936

StartLockPieceSFX::
    ld a, 18
    ld hl, Data_6745
    jp Label_6936

ContinueGenericNoiseSFX::
    call Call_698B
    and a
    ret nz
.stop
    xor a
    ld [$DFF9], a
    ld a, $08
    ldh [rNR42], a
    ld a, $80
    ldh [rNR44], a
    ld hl, $DFCF
    res 7, [hl]
    ret

Data_67FB::
    db $80, $3A, $20, $60, $C6

Call_6800::
    ld hl, GameOverWavePattern
    call Call_690D
    ldh a, [rDIV]
    and a, $1F
    ld b, a
    ld a, $D0
    add b
    ld [$DFF5], a
    ld hl, Data_67FB
    jp SetupChannel.wave

Call_6817::
    ldh a, [rDIV]
    and a, $0F
    ld b, a
    ld hl, $DFF4
    inc [hl]
    ld a, [hl]
    ld hl, $DFF5
    cp a, 14
    jr nc, .label_6832
    inc [hl]
    inc [hl]
.label_682A
    ld a, [hl]
    and a, $F0
    or b
    ld c, LOW(rNR33)
    ldh [c], a
    ret

.label_6832
    cp a, 30
    jp z, Label_68E2
    dec [hl]
    dec [hl]
    dec [hl]
    jr .label_682A

PlayWaveSFX::
    ld a, [$DFF0]
    cp a, $01
    jp z, Label_686B   ; bug
    cp a, $02
    jp z, Call_6800
    ld a, [$DFF1]
    cp a, $01
    jp z, Label_68B6
    cp a, 2
    jp z, Call_6817
    ret

Data_6857::
    db $80, $80, $20
Data_685A::
    db $9D, $87
Data_685C::
    db $80, $F8, $20
Data_685F::
    db $98, $87
Data_6861::
    db $80, $FB, $20
Data_6864::
    db $96, $87
Data_6866::
    db $80, $F6, $20
Data_6869::
    db $95, $87

Label_686B::
    ld hl, WavePattern_6EA9
    call Call_690D
    ld hl, Data_685A
    ld a, [hl]
    ld [$DFF6], a
    ld a, $01
    ld [$DFF5], a
    ld hl, Data_6857
.label_6880
    jp SetupChannel.wave

Label_6883::
    ld a, $00
    ld [$DFF5], a
    ld hl, Data_685F
    ld a, [hl]
    ld [$DFF6], a
    ld hl, Data_685C
    jr Label_686B.label_6880

Label_6894::
    ld a, $01
    ld [$DFF5], a
    ld hl, Data_6864
    ld a, [hl]
    ld [$DFF6], a
    ld hl, Data_6861
    jr Label_686B.label_6880

Label_68A5::
    ld a, $02
    ld [$DFF5], a
    ld hl, Data_6869
    ld a, [hl]
    ld [$DFF6], a
    ld hl, Data_6866
    jr Label_686B.label_6880

Label_68B6::
    ld hl, $DFF4
    inc [hl]
    ldi a, [hl]
    cp a, 9
    jr z, Label_6883
    cp a, 19
    jr z, Label_6894
    cp a, 23
    jr z, Label_68A5
    cp a, 32
    jr z, Label_68E2
    ldi a, [hl]
    cp a, 0
    ret z
    cp a, 1
    jr z, .label_68D8
    cp a, 2
    jr z, .label_68DC
    ret

.label_68D8
    inc [hl]
    inc [hl]
    jr .label_68DE

.label_68DC
    dec [hl]
    dec [hl]
.label_68DE
    ld a, [hl]
    ldh [rNR33], a
    ret

Label_68E2::
    xor a
    ld [$DFF1], a
    ldh [rNR30], a
    ld hl, $DFBF
    res 7, [hl]
    ld hl, $DF9F
    res 7, [hl]
    ld hl, $DFAF
    res 7, [hl]
    ld hl, $DFCF
    res 7, [hl]
    ld a, [$DFE9]
    cp a, 5
    jr z, .label_6908
    ld hl, DefaultWavePattern
    jr $6932

.label_6908::
    ld hl, WavePattern_6EC9
    jr $6932

Call_690D::
    push hl
    ld [$DFF1], a
    ld hl, $DFBF
    set 7, [hl]
    xor a
    ld [$DFF4], a
    ld [$DFF5], a
    ld [$DFF6], a
    ldh [rNR30], a
    ld hl, $DF9F
    set 7, [hl]
    ld hl, $DFAF
    set 7, [hl]
    ld hl, $DFCF
    set 7, [hl]
    pop hl
    call LoadWavePattern
    ret

Label_6936::
    push af
    dec e
    ld a, [$DF71]
    ld [de], a
    inc e
    pop af
    inc e
    ld [de], a
    dec e
    xor a
    ld [de], a
    inc e
    inc e
    ld [de], a
    inc e
    ld [de], a
    ld a, e
    cp a, $E5
    jr z, SetupChannel.square1
    cp a, $F5
    jr z, SetupChannel.wave
    cp a, $FD
    jr z, SetupChannel.noise
    ret

SetupChannel::
.square1
    push bc
    ld c, LOW(rNR10)
    ld b, 5
    jr .copy

.square2
    push bc
    ld c, LOW(rNR21)
    ld b, 4
    jr .copy

.wave
    push bc
    ld c, LOW(rNR30)
    ld b, 5
    jr .copy

.noise
    push bc
    ld c, LOW(rNR41)
    ld b, 4

.copy
    ldi a, [hl]
    ldh [c], a
    inc c
    dec b
    jr nz, .copy
    pop bc
    ret

LookupSFXPointer::
    inc e
    ld [$DF71], a
.lookup
    inc e
    dec a               ; SFX start counting from 1, but to calculate the
    sla a               ; into the table we need to go back to 0-based
    ld c, a
    ld b, $00
    add hl, bc
    ld c, [hl]
    inc hl
    ld b, [hl]
    ld l, c
    ld h, b
    ld a, h             ; Why? A is usually immediately overwritten? Bug?
    ret

Call_698B::
    push de
    ld l, e
    ld h, d
    inc [hl]
    ldi a, [hl]
    cp [hl]
    jr nz, .label_6996
    dec l
    xor a
    ld [hl], a
.label_6996
    pop de
    ret

LoadWavePattern::
    push bc
    ld c, LOW(_AUD3WAVERAM)
.loop
    ldi a, [hl]
    ldh [c], a
    inc c
    ld a, c
    cp a, LOW(_AUD3WAVERAM) + $10  ; 32 4-bit samples
    jr nz, .loop
    pop bc
    ret

_InitAudio::
    xor a
    ld [$DFE1], a
    ld [$DFE9], a
    ld [$DFF1], a
    ld [$DFF9], a
    ld [$DF9F], a
    ld [$DFAF], a
    ld [$DFBF], a
    ld [$DFCF], a
    ld a, $FF
    ldh [rNR51], a      ; Output all channels to both terminals
    ld a, $03
    ld [$DF78], a
.muteChannels
    ld a, $08           ; On channels with a volume envelope, set volume to 
    ldh [rNR12], a      ; zero and envelope to increase (fade-in?)
    ldh [rNR22], a
    ldh [rNR42], a
    ld a, $80
    ldh [rNR14], a      ; Restart sound, continuous mode
    ldh [rNR24], a
    ldh [rNR44], a
    xor a
    ldh [rNR10], a      ; Disable tone-sweep
    ldh [rNR30], a      ; Disable wave
    ret

PlaySquareSFX::
    ld de, $DFE0
    ld a, [de]
    and a
    jr z, .continue
    ld hl, $DF9F
    set 7, [hl]
    ld hl, SquareSFXStartPointers
    call LookupSFXPointer
    jp hl

.continue
    inc e
    ld a, [de]
    and a
    jr z, .out
    ld hl, SquareSFXContinuePointers
    call LookupSFXPointer.lookup
    jp hl

.out                    ; Just like in Super Mario Land, they seem to have
    ret                 ; forgotten about the RET Z instruction

PlayNoiseSFX::
    ld de, $DFF8
    ld a, [de]
    and a
    jr z, .continue
    ld hl, $DFCF
    set 7, [hl]
    ld hl, NoiseSFXStartPointers
    call LookupSFXPointer
    jp hl

.continue
    inc e
    ld a, [de]
    and a
    jr z, .out
    ld hl, NoiseSFXContinuePointers
    call LookupSFXPointer.lookup
    jp hl

.out
    ret

; ?
_Unused::
    call _InitAudio
    ret

Call_6A21::
    ld hl, $DFE8
    ldi a, [hl]
    and a
    ret z
    cp a, $FF
    jr z, _Unused
    ld [hl], a
    ld b, a
    ld hl, MusicPointers
    and a, $1F          ; Curious, there are only $11 songs?
    call LookupSFXPointer.lookup
    call Call_6B13
    call Call_6A3C
    ret

Call_6A3C:
    ld a, [$DFE9]
    and a
    ret z
    ld hl, Data_6ABE
.loop
    dec a
    jr z, .jp_6A4D
    inc hl
    inc hl
    inc hl
    inc hl
    jr .loop

.jp_6A4D
    ldi a, [hl]
    ld [$DF78], a
    ldi a, [hl]
    ld [$DF76], a
    ldi a, [hl]
    ld [$DF79], a
    ldi a, [hl]
    ld [$DF7A], a
    xor a
    ld [$DF75], a
    ld [$DF77], a
    ret

Call_6A65::
    ld a, [$DFE9]
    and a
    jr z, .label_6AA8
    ld hl, $DF75
    ld a, [$DF78]
    cp a, $01
    jr z, .label_6AAC
    cp a, $03
    jr z, .label_6AA8
    inc [hl]
    ldi a, [hl]
    cp [hl]
    jr nz, .label_6AB1
    dec l
    ld [hl], $00
    inc l
    inc l
    inc [hl]
    ld a, [$DF79]
    bit 0, [hl]
    jp z, .label_6A8F
    ld a, [$DF7A]
.label_6A8F
    ld b, a
    ld a, [$DFF1]
    and a
    jr z, .label_6A9A
    set 2, b
    set 6, b
.label_6A9A
    ld a, [$DFF9]
    and a
    jr z, .label_6AA4
    set 3, b
    set 7, b
.label_6AA4
    ld a, b
.label_6AA5
    ldh [rNR51], a
    ret

.label_6AA8
    ld a, $FF
    jr .label_6AA5

.label_6AAC
    ld a, [$DF79]
    jr .label_6A8F

.label_6AB1
    ld a, [$DFF9]
    and a
    jr nz, .label_6AA8
    ld a, [$DFF1]
    and a
    jr nz, .label_6AA8
    ret

Data_6ABE::
    db $01, $24, $EF, $56
    db $01, $00, $E5, $00
    db $01, $20, $FD, $00
    db $01, $20, $DE, $F7
    db $03, $18, $7F, $F7
    db $03, $18, $F7, $7F
    db $03, $48, $DF, $5B
    db $01, $18, $DB, $E7
    db $01, $00, $FD, $F7
    db $03, $20, $7F, $F7
    db $01, $20, $ED, $F7
    db $01, $20, $ED, $F7
    db $01, $20, $ED, $F7
    db $01, $20, $ED, $F7
    db $01, $20, $ED, $F7
    db $01, $20, $EF, $F7
    db $01, $20, $EF, $F7

CopyPointerIndirect::
    ldi a, [hl]
    ld c, a
    ld a, [hl]
    ld b, a
    ld a, [bc]
    ld [de], a
    inc e               ; XXX Should this be INC DE?                           
    inc bc
    ld a, [bc]
    ld [de], a
    ret

CopyPointer::
    ldi a, [hl]
    ld [de], a
    inc e               ; XXX Idem
    ldi a, [hl]
    ld [de], a
    ret

Call_6B13::
    call _InitAudio.muteChannels
    xor a
    ld [$DF75], a
    ld [$DF77], a
    ld de, $DF80
    ld b, $00
    ldi a, [hl]
    ld [de], a
    inc e
    call CopyPointer
    ld de, $DF90
    call CopyPointer
    ld de, $DFA0
    call CopyPointer
    ld de, $DFB0
    call CopyPointer
    ld de, $DFC0
    call CopyPointer
    ld hl, $DF90
    ld de, $DF94
    call CopyPointerIndirect
    ld hl, $DFA0
    ld de, $DFA4
    call CopyPointerIndirect
    ld hl, $DFB0
    ld de, $DFB4
    call CopyPointerIndirect
    ld hl, $DFC0
    ld de, $DFC4
    call CopyPointerIndirect
    ld bc, $0410
    ld hl, $DF92
.loop
    ld [hl], $01
    ld a, c
    add l
    ld l, a
    dec b
    jr nz, .loop
    xor a
    ld [$DF9E], a
    ld [$DFAE], a
    ld [$DFBE], a
    ret

Call_6B7D::
.label_6B7D
    push hl
    xor a
    ldh  [rNR30], a
    ld l, e
    ld h, d
    call LoadWavePattern
    pop hl
    jr .label_6BB3

.label_6B89
    call IncrementPointer
    call LoadFromHLindirect
    ld e, a
    call IncrementPointer
    call LoadFromHLindirect
    ld d, a
    call IncrementPointer
    call LoadFromHLindirect
    ld c, a
    inc l
    inc l
    ld [hl], e
    inc l
    ld [hl], d
    inc l
    ld [hl], c
    dec l
    dec l
    dec l
    dec l
    push hl
    ld hl, $DF70
    ld a, [hl]
    pop hl
    cp a, $03
    jr z, .label_6B7D
.label_6BB3
    call IncrementPointer
    jp $6C5E

IncrementPointer::
    push de
    ldi a, [hl]
    ld e, a
    ldd a, [hl]
    ld d, a
    inc de
.storeDE
    ld a, e
    ldi [hl], a
    ld a, d
    ldd [hl], a
    pop de
    ret

IncrementPointerTwice::
    push de
    ldi a, [hl]
    ld e, a
    ldd a, [hl]
    ld d, a
    inc de              ; Instead of incrementing DE twice, could've just
    inc de              ; incremented once, and jumped one byte earlier. Bug?
    jr IncrementPointer.storeDE

; A ← [[HL]]
LoadFromHLindirect::
    ldi a, [hl]
    ld c, a
    ldd a, [hl]
    ld b, a
    ld a, [bc]
    ld b, a             ; Why? Bug?
    ret

Jmp_6BD5::
    pop hl
    jr .label_6C04

.label_6BD8
    ld a, [$DF70]
.label_6BDB
    cp a, $03
    jr nz, .label_6BEF
    ld a, [$DFB8]
    bit 7, a
    jr z, .label_6BEF
    ld a, [hl]
    cp a, $06
    jr nz, .label_6BEF
    ld a, $40
    ldh [rNR32], a
.label_6BEF
    push hl
    ld a, l
    add a, $09
    ld l, a
    ld a, [hl]
    and a
    jr nz, Jmp_6BD5
    ld a, l
    add a, $04
    ld l, a
    bit 7, [hl]
    jr nz, Jmp_6BD5
    pop hl
    call $6D67
.label_6C04
    dec l
    dec l
    jp $6D39

.label_6C09
    dec l
    dec l
    dec l
    dec l
    call IncrementPointerTwice
.label_6C10
    ld a, l
    add a, $04
    ld e, a
    ld d, h
    call CopyPointerIndirect
    cp a, $00
    jr z, .label_6C3B
    cp a, $FF
    jr z, .label_6C24
    inc l
    jp $6C5C

.label_6C24
    dec l
    push hl
    call IncrementPointerTwice
    call LoadFromHLindirect
    ld e, a
    call IncrementPointer
    call LoadFromHLindirect
    ld d, a
    pop hl
    ld a, e
    ldi [hl], a
    ld a, d
    ldd [hl], a
    jr .label_6C10

.label_6C3B
    ld hl, $DFE9
    ld [hl], $00
    call _InitAudio
    ret

PlayMusic::
    ld hl, $DFE9
    ld a, [hl]
    and a
    ret z
    ld a, $01
    ld [$DF70], a
    ld hl, $DF90
.label_6C52
    inc l
    ldi a, [hl]
    and a
    jp z, Jmp_6BD5.label_6C04
    dec [hl]
    jp nz, Jmp_6BD5.label_6BD8
    inc l
    inc l
    call LoadFromHLindirect
    cp a, $00
    jp z, Jmp_6BD5.label_6C09
    cp a, $9D
    jp z, Call_6B7D.label_6B89
    and a, $F0
    cp a, $A0
    jr nz, .label_6C8B
    ld a, b
    and a, $0F
    ld c, a
    ld b, $00
    push hl
    ld de, $DF81
    ld a, [de]
    ld l, a
    inc de
    ld a, [de]
    ld h, a
    add hl, bc
    ld a, [hl]
    pop hl
    dec l
    ldi [hl], a
    call IncrementPointer
    call LoadFromHLindirect
.label_6C8B
    ld a, b
    ld c, a
    ld b, $00
    call IncrementPointer
    ld a, [$DF70]
    cp a, $04
    jp z, .label_6CBC
    push hl
.label_6C9B
    ld a, l
    add a, $05
    ld l, a
    ld e, l
    ld d, h
    inc l
    inc l
    ld a, c
    cp a, $01
    jr z, .label_6CB7
    ld [hl], $00
    ld hl, NotePitches
    add hl, bc
    ldi a, [hl]
    ld [de], a
    inc e
    ld a, [hl]
    ld [de], a
    pop hl
    jp .label_6CD3

.label_6CB7
    ld [hl], $01
    pop hl
    jr .label_6CD3

.label_6CBC
    push hl
    ld de, $DFC6
    ld hl, Data_6E94
    add hl, bc
.label_6CC4
    ldi a, [hl]
    ld [de], a
    inc e
    ld a, e
    cp a, $CB
    jr nz, .label_6CC4
    ld c, $20
    ld hl, $DFC4
    jr .label_6D01

.label_6CD3
    push hl
    ld a, [$DF70]
    cp a, $01
    jr z, .label_6CFC
    cp a, $02
    jr z, .label_6CF8
    ld c, $1A
    ld a, [$DFBF]
    bit 7, a
    jr nz, .label_6CED
    xor a
    ldh [c], a
    ld a, $80
    ldh [c], a
.label_6CED
    inc c
    inc l
    inc l
    inc l
    inc l
    ldi a, [hl]
    ld e, a
    ld d, $00
    jr .label_6D0D

.label_6CF8
    ld c, $16
    jr .label_6D01

.label_6CFC
    ld c, $10
    ld a, $00
    inc c
.label_6D01
    inc l
    inc l
    inc l
    ldd a, [hl]
    and a
    jr nz, .label_6D57
    ldi a, [hl]
    ld e, a
.label_6D0A
    inc l
    ldi a, [hl]
    ld d, a
.label_6D0D
    push hl
    inc l
    inc l
    ldi a, [hl]
    and a
    jr z, .label_6D16
    ld e, $01
.label_6D16
    inc l
    inc l
    ld [hl], $00
    inc l
    ld a, [hl]
    pop hl
    bit 7, a
    jr nz, .label_6D34
    ld a, d
    ldh [c], a
    inc c
    ld a, e
    ldh [c], a
    inc c
    ldi a, [hl]
    ldh [c], a
    inc c
    ld a, [hl]
    or a, $80
    ldh [c], a
    ld a, l
    or a, $05
    ld l, a
    res 0, [hl]
.label_6D34
    pop hl
    dec l
    ldd a, [hl]
    ldd [hl], a
    dec l
    ld de, $DF70
    ld a, [de]
    cp a, $04
    jr z, .label_6D4A
    inc a
    ld [de], a
    ld de, $0010
    add hl, de
    jp .label_6C52

.label_6D4A
    ld hl, $DF9E
    inc [hl]
    ld hl, $DFAE
    inc [hl]
    ld hl, $DFBE
    inc [hl]
    ret

.label_6D57
    ld b, $00
    push hl
    pop hl              ; Hm?
    inc l
    jr .label_6D0A

.label_6D5E
    ld a, b
    srl a
    ld l, a
    ld h, $00
    add hl, de
    ld e, [hl]
    ret

Call_6D67::
    push hl
    ld a, l
    add a, $06
    ld l, a
    ld a, [hl]
    and a, $0F
    jr z, .label_6D89
    ld [$DF71], a
    ld a, [$DF70]
    ld c, $13
    cp a, $01
    jr z, .label_6D8B
    ld c, $18
    cp a, $02
    jr z, .label_6D8B
    ld c, $1D
    cp a, $03
    jr z, .label_6D8B
.label_6D89
    pop hl
    ret

.label_6D8B
    inc l
    ldi a, [hl]
    ld e, a
    ld a, [hl]
    ld d, a
    push de
    ld a, l
    add a, $04
    ld l, a
    ld b, [hl]
    ld a, [$DF71]
    cp a, $01
    jr .label_6DA6      ; Huh? Bug?

.label_6D9D
    cp a, $03
    jr .label_6DA1

.label_6DA1
    ld hl, $FFFF
    jr .label_6DC2

.label_6DA6
    ld de, Data_6DCB
    call $6D5E
    bit 0, b
    jr nz, .label_6DB2
    swap e
.label_6DB2
    ld a, e
    and a, $0F
    bit 3, a
    jr z, .label_6DBF
    ld h, $FF
    or a, $F0
    jr .label_6DC1

.label_6DBF
    ld h, $00
.label_6DC1
    ld l, a
.label_6DC2
    pop de
    add hl, de
    ld a, l
    ldh [c], a
    inc c
    ld a, h
    ldh [c], a
    jr .label_6D89

Data_6DCB::
    db $00, $00, $00, $00, $00
    db $00, $10, $00, $0F, $00
    db $00, $11, $00, $0F, $F0
    db $01, $12, $10, $FF, $EF
    db $01, $12, $10, $FF, $EF
    db $01, $12, $10, $FF, $EF
    db $01, $12, $10, $FF, $EF
    db $01, $12, $10, $FF, $EF
    db $01, $12, $10, $FF, $EF
    db $01, $12, $10, $FF, $EF
    db $01, $12, $10, $FF, $EF

NotePitches::
    dw $F00 ; 
    dw $02C ;   65.4 Hz C 2 (-0)
    dw $09C ;   69.3 Hz C#2 (-0) Bug! 09D is closer
    dw $106 ;   73.4 Hz D 2 (-1) Bug! 107 is closer
    dw $16B ;   77.8 Hz D#2 (+0)
    dw $1C9 ;   82.4 Hz E 2 (-0)
    dw $223 ;   87.3 Hz F 2 (+0)
    dw $277 ;   92.5 Hz F#2 (+0)
    dw $2C6 ;   98.0 Hz G 2 (-1) Bug! 2C7 is closer
    dw $312 ;  103.9 Hz G#2 (+1)
    dw $356 ;  109.8 Hz A 2 (-4) Bug! 357 is closer
    dw $39B ;  116.5 Hz A#2 (-0)
    dw $3DA ;  123.4 Hz B 2 (-1)
    dw $416 ;  130.8 Hz C 3 (-0)
    dw $44E ;  138.6 Hz C#3 (-0)
    dw $483 ;  146.8 Hz D 3 (-1)
    dw $4B5 ;  155.5 Hz D#3 (-1)
    dw $4E5 ;  164.9 Hz E 3 (+1)
    dw $511 ;  174.5 Hz F 3 (-1)
    dw $53B ;  184.9 Hz F#3 (-1)
    dw $563 ;  195.9 Hz G 3 (-1)
    dw $589 ;  207.7 Hz G#3 (+1)
    dw $5AC ;  219.9 Hz A 3 (-1)
    dw $5CE ;  233.2 Hz A#3 (+1)
    dw $5ED ;  246.8 Hz B 3 (-1)
    dw $60A ;  261.1 Hz C 4 (-3) Bug! 60B is closer
    dw $627 ;  277.1 Hz C#4 (-0)
    dw $642 ;  293.9 Hz D 4 (+1)
    dw $65B ;  311.3 Hz D#4 (+1)
    dw $672 ;  329.3 Hz E 4 (-2)
    dw $689 ;  349.5 Hz F 4 (+1)
    dw $69E ;  370.3 Hz F#4 (+1)
    dw $6B2 ;  392.4 Hz G 4 (+2)
    dw $6C4 ;  414.8 Hz G#4 (-2)
    dw $6D6 ;  439.8 Hz A 4 (-1)
    dw $6E7 ;  466.4 Hz A#4 (+1)
    dw $6F7 ;  494.6 Hz B 4 (+3)
    dw $706 ;  524.3 Hz C 5 (+3)
    dw $714 ;  555.4 Hz C#5 (+3)
    dw $721 ;  587.8 Hz D 5 (+1)
    dw $72D ;  621.2 Hz D#5 (-3)
    dw $739 ;  658.7 Hz E 5 (-2)
    dw $744 ;  697.2 Hz F 5 (-3)
    dw $74F ;  740.5 Hz F#5 (+1)
    dw $759 ;  784.9 Hz G 5 (+2)
    dw $762 ;  829.6 Hz G#5 (-2)
    dw $76B ;  879.7 Hz A 5 (-1)
    dw $773 ;  929.6 Hz A#5 (-5)
    dw $77B ;  985.5 Hz B 5 (-4)
    dw $783 ; 1048.6 Hz C 6 (+3)
    dw $78A ; 1110.8 Hz C#6 (+3)
    dw $790 ; 1170.3 Hz D 6 (-6)
    dw $797 ; 1248.3 Hz D#6 (+5)
    dw $79D ; 1324.0 Hz E 6 (+7)
    dw $7A2 ; 1394.4 Hz F 6 (-3)
    dw $7A7 ; 1472.7 Hz F#6 (-9)
    dw $7AC ; 1560.4 Hz G 6 (-8)
    dw $7B1 ; 1659.1 Hz G#6 (-2)
    dw $7B6 ; 1771.2 Hz A 6 (+11)
    dw $7BA ; 1872.5 Hz A#6 (+7)
    dw $7BE ; 1985.9 Hz B 6 (+9)
    dw $7C1 ; 2080.5 Hz C 7 (-10)
    dw $7C4 ; 2184.5 Hz C#7 (-26) Bug! 7C5 is closer
    dw $7C8 ; 2340.6 Hz D 7 (-6)
    dw $7CB ; 2473.1 Hz D#7 (-11)
    dw $7CE ; 2621.4 Hz E 7 (-10)
    dw $7D1 ; 2788.8 Hz F 7 (-3)
    dw $7D4 ; 2978.9 Hz F#7 (+11)
    dw $7D6 ; 3120.8 Hz G 7 (-8)
    dw $7D9 ; 3360.8 Hz G#7 (+20)
    dw $7DB ; 3542.5 Hz A 7 (+11)
    dw $7DD ; 3744.9 Hz A#7 (+7)
    dw $7DF ; 3971.9 Hz B 7 (+9)

Data_6E94::
    db $00
    db $00, $00, $00, $00, $C0
    db $A1, $00, $3A, $00, $C0
    db $B1, $00, $29, $01, $C0
    db $61, $00, $3A, $00, $C0

;             ▄▀▄       ▄        
;           ▄▀                   
;         ▄▀        ▄▀ ▀ ▄       
;        ▀       ▀▄       ▄      
;      ▄▀          ▀  ▀    ▀▄    
;   ▄▄▀                      ▀▄  
; ▄▀                           ▀ 
;▀                              ▀
WavePattern_6EA9::
    db $12, $34, $45, $67, $9A, $BC, $DE, $FE
    db $98, $7A, $B7, $BE, $A8, $76, $54, $31
;                                
;                                
;             ▄▀▀▄               
;          ▄▄▀    ▀▄▄            
;        ▄▀          ▀▄          
;    ▄▄▀▀              ▀▀▄▄      
;  ▄▀                      ▀▀▄▄  
;▄▀                            ▀▀
WavePattern_6EB9::
    db $01, $23, $44, $55, $67, $88, $9A, $BB
    db $A9, $88, $76, $55, $44, $33, $22, $11
;              ▄▀▀▄              
;            ▄▀    ▀▄            
;          ▄▀        ▀▄          
;        ▄▀            ▀▄        
;      ▄▀                ▀▄      
;    ▄▀                    ▀▄    
;  ▄▀                        ▀▄  
;▄▀                            ▀▄

WavePattern_6EC9::
    db $01, $23, $45, $67, $89, $AB, $CD, $EF
    db $FE, $DC, $BA, $98, $76, $54, $32, $10
;                                
;                       ▄▄▀      
;▄                  ▄▄▀▀         
;  ▄            ▄▄▀▀             
;           ▄▄▀▀           ▄     
;       ▄▄▀▀                ▄    
;   ▄▄▀▀                     ▀▄  
; ▀                            ▀▄
GameOverWavePattern::
    db $A1, $82, $23, $34, $45, $56, $67, $78
    db $89, $9A, $AB, $BC, $CD, $64, $32, $10
; Very similar to, but not exactly the same as the one used in Super Mario Land
;                   ▀▀▄          
;                  ▀   ▄         
;                 ▄              
;       ▄▀▀▀▄    ▀      ▀▄       
;     ▄▀     ▀▄▄▀                
;    ▀                    ▀▄     
;  ▄▀                       ▄  ▀ 
;▀▀                          ▀▀ ▀
DefaultWavePattern::
    db $11, $23, $56, $78, $99, $98, $76, $67
    db $9A, $DF, $FE, $C9, $85, $42, $11, $31

INCBIN "baserom.gb", $6EF9, $7FF0 - $6EF9
UpdateAudio::
    jp _UpdateAudio
InitAudio::
    jp _InitAudio


; vim: set expandtab tabstop=4 shiftwidth=4 
