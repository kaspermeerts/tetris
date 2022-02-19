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
    call $6A21
    call $6C44
    call $6A65
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
    call $69C7
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

INCBIN "baserom.gb", $6A21, $6EA9 - $6A21
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
