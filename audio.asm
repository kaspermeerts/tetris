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

DoNothing::
    ret

_UpdateAudio::
    push af
    push bc
    push de
    push hl
    ld a, [wPauseUnpauseSound]
    cp a, 1
    jr z, .pauseAudio
    cp a, 2
    jr z, .unpauseAudio
    ld a, [wPauseTuneTimer]
    and a
    jr nz, .playPauseTune
.updateAudio
    ldh a, [hDemoNumber]
    and a
    jr z, .playSounds
    xor a
    ld [wNewSquareSFXID], a
    ld [wNewMusicID], a
    ld [wNewWaveSFXID], a
    ld [wNewNoiseSFXID], a
.playSounds
    call DoNothing      ; Simply returns. No corresponding routine in SML
    call PlaySquareSFX
    call PlayNoiseSFX
    call PlayWaveSFX
    call StartMusic
    call PlayMusic
    call PanStereo
.out
    xor a
    ld [wNewSquareSFXID], a
    ld [wNewMusicID], a
    ld [wNewWaveSFXID], a
    ld [wNewNoiseSFXID], a
    ld [wPauseUnpauseSound], a
    pop hl
    pop de
    pop bc
    pop af
    ret

.pauseAudio
    call _InitAudio.muteChannels
    xor a
    ld [wCurrentSquareSFXID], a
    ld [wCurrentWaveSFXID], a
    ld [wCurrentNoiseSFXID], a
    ld hl, $DFBF
    res 7, [hl]
    ld hl, $DF9F
    res 7, [hl]
    ld hl, $DFAF
    res 7, [hl]
    ld hl, $DFCF
    res 7, [hl]
    ld hl, DefaultWavePattern
    call LoadWavePattern
    ld a, 48            ; 48 frames, ~0.8 seconds, but of course, it stops at 16
    ld [wPauseTuneTimer], a
.playFirstNote
    ld hl, .pauseTuneFirstNoteData
.playNote
    call SetupChannel.square2
    jr .out

.playSecondNote
    ld hl, .pauseTuneSecondNoteData
    jr .playNote

.unpauseAudio
    xor a
    ld [wPauseTuneTimer], a
    jr .updateAudio

.playPauseTune
    ld hl, wPauseTuneTimer
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
    inc [hl]            ; Keep the music paused by keeping wPauseTuneTimer
    jr .out             ; non-zero. Seems like a hack...

; Exactly the same as Super Mario Land
; 50% duty cycle, 14/256 seconds, volume 15/16, decreasing envelope, 3/64 seconds per step (pointless?)
.pauseTuneFirstNoteData
    db $B2, $E3, $83, $C7   ; 1048.6 Hz ~ C6
.pauseTuneSecondNoteData
    db $B2, $E3, $C1, $C7   ; 2080.5 Hz ~ C7

CheckPlayingTetrisSweep::
    ld a, [wCurrentWaveSFXID]
    cp a, $01       ; End of Tetris sweep
    ret

CheckPlayingGarbageAttack::
    ld a, [wCurrentSquareSFXID]
    cp a, $05       ; Garbage attack sweep
    ret

CheckPlayingTetris::
    ld a, [wCurrentSquareSFXID]
    cp a, $07       ; Tetris
    ret

CheckPlayingLevelUp::
    ld a, [wCurrentSquareSFXID]
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
    jp StartSFXCommon

ContinueTinkSFX::
    call UpdateSFXProgress
    and a
    ret nz
    ld hl, $DFE4
    inc [hl]
    ld a, [hl]
    cp a, $02
    jr z, ContinueGenericSquareSFX.stop
    ld hl, Data_65A0
    jp SetupChannel.square1

StartChangeScreenSFX::
    ld a, 3
    ld hl, Data_65A5
    jp StartSFXCommon

ContinueGenericSquareSFX::
    call UpdateSFXProgress
    and a
    ret nz
.stop
    xor a
    ld [wCurrentSquareSFXID], a
    ldh [rNR10], a
    ld a, $08           ; Keep envelope direction at increase for some reason?
    ldh [rNR12], a
    ld a, $80           ; Trigger channel??
    ldh [rNR14], a
    ld hl, $DF9F        ; Channel lock
    res 7, [hl]
    ret

Data_65E7::
    db $00, $80, $E1, $C1, $87
Data_65EC::
    db $00, $80, $E1, $AC, $87

StartTetrisSFX::
    ld hl, Data_65E7
    jp StartSFXCommon

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
    ld a, 1
    ld hl, wNewWaveSFXID
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
    call CheckPlayingTetrisSweep
    ret z
    call CheckPlayingLevelUp
    ret z
    call CheckPlayingTetris
    ret z
    call CheckPlayingGarbageAttack
    ret z
    ld a, 2
    ld hl, Data_6623
    jp StartSFXCommon

LevelUpNote1::
    db $00, $B0, $F1, $B6, $C7 ; A 6
LevelUpNote2::
    db $00, $B0, $F1, $C4, $C7 ; C#7
LevelUpNote3::
    db $00, $B0, $F1, $CE, $C7 ; E 7
LevelUpNote4::
    db $00, $B0, $F1, $DB, $C7 ; A 7

StartLevelUpSFX::
    call CheckPlayingTetris
    ret z
    ld a, 7
    ld hl, LevelUpNote1
    jp StartSFXCommon

ContinueLevelUpSFX::
    call UpdateSFXProgress
    and a
    ret nz
    ld hl, wSquareSFXNoteCounter
    inc [hl]
    ld a, [hl]
    cp a, 1
    jr z, .note2
    cp a, 2
    jr z, .note3
    cp a, 3
    jr z, .note4
    cp a, 4
    jr z, .note5
    cp a, 5
    jp z, ContinueGenericSquareSFX.stop
    ret

.note2
    ld hl, LevelUpNote2
    jr .playNote

.note3
    ld hl, LevelUpNote3
    jr .playNote

.note4
    ld hl, LevelUpNote4
    jr .playNote

.note5
    ld hl, LevelUpNote1
.playNote
    jp SetupChannel.square1

Data_6695::
    db $3E, $80, $E3, $00, $C4
Data_669A::
    db $93, $83, $83, $73, $63, $53, $43, $33, $23, $13, $00
Data_66A5::
    db $00, $23, $43, $63, $83, $A3, $C3, $D3, $E3, $FF

StartLineClearSFX::
    call CheckPlayingTetrisSweep
    ret z
    call CheckPlayingLevelUp
    ret z
    call CheckPlayingTetris
    ret z
    ld a, 6
    ld hl, Data_6695
    jp StartSFXCommon

ContinueLineClearSFX::
    call UpdateSFXProgress
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
    call CheckPlayingTetrisSweep
    ret z
    call CheckPlayingLevelUp
    ret z
    call CheckPlayingTetris
    ret z
    call CheckPlayingGarbageAttack
    ret z
    ld a, 3
    ld hl, Data_66EC
    jp StartSFXCommon

ContinueRotatePieceSFX::
    call UpdateSFXProgress
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
    call CheckPlayingTetris
    ret z
    ld a, 40
    ld hl, Data_6740
    jp StartSFXCommon

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

LiftOffNoiseData::
    db $65, $65, $65, $64, $57, $56, $55, $54, $54, $54, $54, $54
    db $47, $46, $46, $45, $45, $45, $44, $44, $44, $34, $34, $34
    db $34, $34, $34, $34, $34, $34, $34, $34, $34, $34, $34, $34
LiftOffVolumeData::
    db $70, $60, $70, $70, $70, $80, $90, $A0, $D0, $F0, $E0, $D0
    db $C0, $B0, $A0, $90, $80, $70, $60, $50, $40, $30, $30, $20
    db $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $10, $10

StartIgnitionSFX::
    ld a, 48
    ld hl, Data_674D
    jp StartSFXCommon

StartLiftoffSFX::
    ld a, 48
    ld hl, Data_6751
    jp StartSFXCommon

ContinueLiftoffSFX::
    call UpdateSFXProgress
    and a
    ret nz
    ld hl, $DFFC
    ld a, [hl]          ; Note counter
    ld c, a
    cp a, 36
    jp z, ContinueGenericNoiseSFX.stop
    inc [hl]
    ld b, $00
    push bc
    ld hl, LiftOffNoiseData
    add hl, bc
    ld a, [hl]
    ldh [rNR43], a      ; Polynomial counter
    pop bc
    ld hl, LiftOffVolumeData
    add hl, bc
    ld a, [hl]
    ldh [rNR42], a      ; Volume envelope
    ld a, $80           ; Trigger channel
    ldh [rNR44], a
    ret

StartStackFallSFX::
    ld a, 32
    ld hl, Data_6749
    jp StartSFXCommon

StartLockPieceSFX::
    ld a, 18
    ld hl, Data_6745
    jp StartSFXCommon

ContinueGenericNoiseSFX::
    call UpdateSFXProgress
    and a
    ret nz
.stop
    xor a
    ld [wCurrentNoiseSFXID], a
    ld a, $08
    ldh [rNR42], a      ; Stop envelope operation
    ld a, $80
    ldh [rNR44], a      ; Trigger channel?
    ld hl, $DFCF        ; Channel lock
    res 7, [hl]
    ret

; Sound on, 198/256s, 100% volume, 157.5 Hz (~D#3), 
Data_67FB::
    db $80, $3A, $20, $60, $C6

StartGameOverSFX::
    ld hl, GameOverWavePattern
    call LockChannelsAndPrepareWaveChannel
    ldh a, [rDIV]
    and a, $1F
    ld b, a
    ld a, $D0
    add b               ; Generate a "random" number between 0xD0 and 0xEF
    ld [$DFF5], a
    ld hl, Data_67FB
    jp SetupChannel.wave

ContinueGameOverSFX::
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
    ld c, LOW(rNR33)    ; Frequency lower 8 bits
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
    ld a, [wNewWaveSFXID]
    cp a, 1
    jp z, StartTetrisSweepSFX   ; Bug. Could've been a JR
    cp a, 2
    jp z, StartGameOverSFX
    ld a, [wCurrentWaveSFXID]
    cp a, 1
    jp z, ContinueTetrisSweepSFX
    cp a, 2
    jp z, ContinueGameOverSFX
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

StartTetrisSweepSFX::
    ld hl, WavePattern_6EA9
    call LockChannelsAndPrepareWaveChannel
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
    jr StartTetrisSweepSFX.label_6880

Label_6894::
    ld a, $01
    ld [$DFF5], a
    ld hl, Data_6864
    ld a, [hl]
    ld [$DFF6], a
    ld hl, Data_6861
    jr StartTetrisSweepSFX.label_6880

Label_68A5::
    ld a, $02
    ld [$DFF5], a
    ld hl, Data_6869
    ld a, [hl]
    ld [$DFF6], a
    ld hl, Data_6866
    jr StartTetrisSweepSFX.label_6880

ContinueTetrisSweepSFX::
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
    ld [wCurrentWaveSFXID], a
    ldh [rNR30], a
    ld hl, $DFBF        ; Disable all locks
    res 7, [hl]
    ld hl, $DF9F
    res 7, [hl]
    ld hl, $DFAF
    res 7, [hl]
    ld hl, $DFCF
    res 7, [hl]
    ld a, [wCurrentMusicID]
    cp a, 5             ; TODO Korobeiniki
    jr z, .korobeinikiException
    ld hl, DefaultWavePattern
    jr LockChannelsAndPrepareWaveChannel.loadWavePattern

.korobeinikiException
    ld hl, WavePattern_6EC9
    jr LockChannelsAndPrepareWaveChannel.loadWavePattern

LockChannelsAndPrepareWaveChannel::
    push hl
    ld [wCurrentWaveSFXID], a
    ld hl, $DFBF
    set 7, [hl]         ; Lock wave channel
    xor a
    ld [$DFF4], a
    ld [$DFF5], a
    ld [$DFF6], a
    ldh [rNR30], a      ; Channel 3 off
    ld hl, $DF9F        ; Ah heck, lock all the channels!
    set 7, [hl]
    ld hl, $DFAF
    set 7, [hl]
    ld hl, $DFCF
    set 7, [hl]
    pop hl
.loadWavePattern
    call LoadWavePattern
    ret

; HL contains channel specific data
; A is note length
StartSFXCommon::
    push af
    dec e               ; Back to DFE(0,10,18) + 1, i.e. currently playing SFX
    ld a, [$DF71]       ; Temp?
    ld [de], a          ; Commit to new SFX
    inc e
    pop af
    inc e
    ld [de], a          ; DFx3, note length
    dec e
    xor a
    ld [de], a          ; DFx2 frame counter?
    inc e
    inc e
    ld [de], a          ; DFx4 note counter (i.e. counts notes)
    inc e
    ld [de], a          ; DFx5?
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

LookupSoundPointer::
    inc e               ; DFE1, DFF1 or DFF9
    ld [$DF71], a
.lookup
    inc e               ; DFE2, DFF2 or DFFA (why tho)
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

UpdateSFXProgress::
    push de             ; DE contains DFE(0,8,10,18) + 2
    ld l, e
    ld h, d
    inc [hl]            ; Increment
    ldi a, [hl]
    cp [hl]             ; and compare with sound length
    jr nz, .out
    dec l
    xor a
    ld [hl], a
.out
    pop de
    ret

; From HL
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
    ld [wCurrentSquareSFXID], a
    ld [wCurrentMusicID], a
    ld [wCurrentWaveSFXID], a
    ld [wCurrentNoiseSFXID], a
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
    ld de, wNewSquareSFXID
    ld a, [de]
    and a
    jr z, .continue
    ld hl, $DF9F        ; Enable channel lock
    set 7, [hl]
    ld hl, SquareSFXStartPointers
    call LookupSoundPointer
    jp hl

.continue
    inc e
    ld a, [de]
    and a
    jr z, .out
    ld hl, SquareSFXContinuePointers
    call LookupSoundPointer.lookup
    jp hl

.out                    ; Just like in Super Mario Land, they seem to have
    ret                 ; forgotten about the RET Z instruction

PlayNoiseSFX::
    ld de, wNewNoiseSFXID
    ld a, [de]
    and a
    jr z, .continue
    ld hl, $DFCF        ; Lock channel
    set 7, [hl]
    ld hl, NoiseSFXStartPointers
    call LookupSoundPointer
    jp hl

.continue
    inc e
    ld a, [de]
    and a
    jr z, .out
    ld hl, NoiseSFXContinuePointers
    call LookupSoundPointer.lookup
    jp hl

.out
    ret

; Seems like pointless indirection, but whatever
_StopAudio::
    call _InitAudio
    ret

StartMusic::
    ld hl, wNewMusicID
    ldi a, [hl]
    and a
    ret z
    cp a, $FF
    jr z, _StopAudio    ; Unreachable in SML due to boundary check
    ld [hl], a
    ld b, a
    ld hl, MusicPointers
    and a, $1F          ; Curious, there are only $11 songs?
    call LookupSoundPointer.lookup
    call InitMusicChannels
    call InitStereo
    ret

InitStereo:
    ld a, [wCurrentMusicID]
    and a
    ret z
    ld hl, StereoData
.loop
    dec a
    jr z, .writeStereoData
    inc hl
    inc hl
    inc hl
    inc hl
    jr .loop

.writeStereoData
    ldi a, [hl]
    ld [wMonoOrStereo], a
    ldi a, [hl]
    ld [wPanInterval], a
    ldi a, [hl]
    ld [wChannelEnableMask1], a
    ldi a, [hl]
    ld [wChannelEnableMask2], a
    xor a
    ld [wPanFrameCounter], a
    ld [wPanCounter], a
    ret

PanStereo::
    ld a, [wCurrentMusicID]
    and a
    jr z, .enableAllChannels
    ld hl, wPanFrameCounter
    ld a, [wMonoOrStereo]
    cp a, 1
    jr z, .applyMask1
    cp a, 3
    jr z, .enableAllChannels
    inc [hl]
    ldi a, [hl]
    cp [hl]             ; Compare framecounter with interval
    jr nz, .checkNoiseAndWave
    dec l
    ld [hl], 0          ; Reset framecounter
    inc l
    inc l
    inc [hl]            ; And add one to the pan-counter
    ld a, [wChannelEnableMask1]
    bit 0, [hl]         ; Alternate between mask 1 and mask 2
    jp z, .checkWaveSFX
    ld a, [wChannelEnableMask2]
.checkWaveSFX
    ld b, a
    ld a, [wCurrentWaveSFXID]
    and a
    jr z, .checkNoiseSFX
    set 2, b            ; If a sound effect is playing which uses the wave
    set 6, b            ; channel, override the mask to play on both terminals
.checkNoiseSFX
    ld a, [wCurrentNoiseSFXID]
    and a
    jr z, .out
    set 3, b            ; Et pour le bruit la même chose
    set 7, b
.out
    ld a, b
.applyChannelEnableMask
    ldh [rNR51], a
    ret

.enableAllChannels
    ld a, $FF
    jr .applyChannelEnableMask

.applyMask1
    ld a, [wChannelEnableMask1]
    jr .checkWaveSFX

.checkNoiseAndWave
    ld a, [wCurrentNoiseSFXID]
    and a
    jr nz, .enableAllChannels
    ld a, [wCurrentWaveSFXID]
    and a
    jr nz, .enableAllChannels
    ret

; TODO is the second mask just unused for mono?
; TODO Is this all just pointless?
StereoData::
    db 1, 36, $EF, $56
    db 1,  0, $E5, $00
    db 1, 32, $FD, $00
    db 1, 32, $DE, $F7
    db 3, 24, $7F, $F7
    db 3, 24, $F7, $7F
    db 3, 72, $DF, $5B
    db 1, 24, $DB, $E7
    db 1,  0, $FD, $F7
    db 3, 32, $7F, $F7
    db 1, 32, $ED, $F7
    db 1, 32, $ED, $F7
    db 1, 32, $ED, $F7
    db 1, 32, $ED, $F7
    db 1, 32, $ED, $F7
    db 1, 32, $EF, $F7
    db 1, 32, $EF, $F7

; [DE] ← [[HL]]
UpdateWordAtDEfromHLindirect::
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

; [DE] ← [HL]
UpdateDEfromHL::
    ldi a, [hl]
    ld [de], a
    inc e               ; XXX Idem
    ldi a, [hl]
    ld [de], a
    ret

; HL contains a pointer from MusicPointers
InitMusicChannels::
    call _InitAudio.muteChannels
    xor a
    ld [wPanFrameCounter], a
    ld [wPanCounter], a
    ld de, $DF80
    ld b, $00
    ldi a, [hl]
    ld [de], a          ; Load DF80 with the first byte. Always zero? Unused?
    inc e               ; DE ← DF81
    call UpdateDEfromHL
    ld de, $DF90
    call UpdateDEfromHL
    ld de, $DFA0
    call UpdateDEfromHL
    ld de, $DFB0
    call UpdateDEfromHL
    ld de, $DFC0
    call UpdateDEfromHL
    ld hl, $DF90
    ld de, $DF94
    call UpdateWordAtDEfromHLindirect
    ld hl, $DFA0
    ld de, $DFA4
    call UpdateWordAtDEfromHLindirect
    ld hl, $DFB0
    ld de, $DFB4
    call UpdateWordAtDEfromHLindirect
    ld hl, $DFC0
    ld de, $DFC4
    call UpdateWordAtDEfromHLindirect
    ld bc, $0410        ; 4 loops, $10 between DFx2's
    ld hl, $DF92
.loop
    ld [hl], $01        ; Set DF(9ABC)2 to 1
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

Command_9D::
.loadNewWavepattern
    push hl
    xor a
    ldh [rNR30], a      ; Turn wave channel off
    ld l, e
    ld h, d
    call LoadWavePattern
    pop hl
    jr .nextCommand

.entry
    call IncrementWordAtHL      ; HL is DFx4
    call LoadFromHLindirect     ; Read the three bytes following $9D
    ld e, a
    call IncrementWordAtHL
    call LoadFromHLindirect
    ld d, a
    call IncrementWordAtHL
    call LoadFromHLindirect
    ld c, a
    inc l
    inc l
    ld [hl], e                  ; And write them to DFx6 - DFx7 - DFx8
    inc l
    ld [hl], d
    inc l
    ld [hl], c
    dec l
    dec l
    dec l
    dec l               ; HL is back to DFx4
    push hl
    ld hl, wMusicCurrentChannel
    ld a, [hl]
    pop hl
    cp a, 3
    jr z, .loadNewWavepattern
.nextCommand
    call IncrementWordAtHL
    jp PlayMusic.readCommand

IncrementWordAtHL::
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

IncrementWordAtHLtwice::
    push de
    ldi a, [hl]
    ld e, a
    ldd a, [hl]
    ld d, a
    inc de              ; Instead of incrementing DE twice, could've just
    inc de              ; incremented once, and jumped one byte earlier. Bug?
    jr IncrementWordAtHL.storeDE

; A,B ← [[HL]]
LoadFromHLindirect::
    ldi a, [hl]
    ld c, a
    ldd a, [hl]
    ld b, a
    ld a, [bc]
    ld b, a             ; Keep a copy around
    ret

; Vibrato and dampen wave channel
ApplyMusicEffects::
.popAndNextChannel
    pop hl
    jr ._nextChannel

.entry
    ld a, [wMusicCurrentChannel]
    cp a, 3
    jr nz, .checkMuteAndLock
    ld a, [$DFB8]
    bit 7, a            ; Bit 7 never matters for the channel registers
    jr z, .checkMuteAndLock
    ld a, [hl]          ; DFB2 - Note timer
    cp a, 6             ; If bit 7 of DFB8 is set, halve the volume for the last
    jr nz, .checkMuteAndLock
    ld a, AUD3LEVEL_50  ; 6 frames of a note?
    ldh [rNR32], a
.checkMuteAndLock
    push hl
    ld a, l
    add a, $09
    ld l, a
    ld a, [hl]          ; DFxB
    and a
    jr nz, ApplyMusicEffects.popAndNextChannel
    ld a, l
    add a, $04
    ld l, a
    bit 7, [hl]         ; Check channel lock
    jr nz, ApplyMusicEffects.popAndNextChannel
    pop hl
    call ApplyVibrato
._nextChannel
    dec l
    dec l
    jp PlayMusic.nextChannel

; That is to say, go to the next section
Command_00::
    dec l
    dec l
    dec l
    dec l               ; HL points to DFx0, section pointer?
    call IncrementWordAtHLtwice
.label_6C10
    ld a, l
    add a, $04
    ld e, a
    ld d, h             ; DE is DFx4, pointer inside the section
    call UpdateWordAtDEfromHLindirect
    cp a, $00           ; Top nibble being 00 means the address was 0000
    jr z, .stopMusic
    cp a, $FF           ; Or FFFF
    jr z, .label_6C24
    inc l
    jp PlayMusic.label_6C5C

.label_6C24
    dec l
    push hl
    call IncrementWordAtHLtwice ; DFx0
    call LoadFromHLindirect
    ld e, a
    call IncrementWordAtHL
    call LoadFromHLindirect
    ld d, a
    pop hl
    ld a, e
    ldi [hl], a
    ld a, d
    ldd [hl], a
    jr .label_6C10

.stopMusic
    ld hl, wCurrentMusicID
    ld [hl], $00
    call _InitAudio
    ret

PlayMusic::
    ld hl, wCurrentMusicID
    ld a, [hl]
    and a
    ret z
    ld a, 1
    ld [wMusicCurrentChannel], a
    ld hl, $DF90
.channelLoop
    inc l
    ldi a, [hl]         ; Upper nibble of the pointer at DFx0 - DFx1
    and a               ; Seems like a hacky way to see if the channel is active
    jp z, ApplyMusicEffects._nextChannel
    dec [hl]            ; Decrement the timer at DFx2
    jp nz, ApplyMusicEffects.entry
.label_6C5C             ; This note is done?
    inc l
    inc l
.readCommand
    call LoadFromHLindirect ; Load from pointer at DFx4 - DFx5
    cp a, $00
    jp z, Command_00
    cp a, $9D
    jp z, Command_9D.entry
    and a, $F0
    cp a, $A0
    jr nz, .commandPlayNoteOrRest
    ld a, b             ; Command AX
    and a, $0F          ; Add lower nibble to pointer at DF81
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
    ld a, [hl]          ; Lookup what is pointed at
    pop hl
    dec l
    ldi [hl], a         ; And load that value into DFx3, which is a type of length?
    call IncrementWordAtHL  ; Increment pointer at DFx4 - DFx5
    call LoadFromHLindirect ; and load its contents
.commandPlayNoteOrRest
    ld a, b
    ld c, a
    ld b, $00
    call IncrementWordAtHL  ; Increment that same pointer
    ld a, [wMusicCurrentChannel]
    cp a, 4
    jp z, .noiseNote
    push hl
    ld a, l
    add a, $05
    ld l, a
    ld e, l
    ld d, h             ; DE is DFx9 - frequency?
    inc l
    inc l
    ld a, c
    cp a, 1
    jr z, .rest
    ld [hl], 0          ; DFxB
    ld hl, NotePitches
    add hl, bc
    ldi a, [hl]
    ld [de], a          ; DFx9
    inc e
    ld a, [hl]
    ld [de], a          ; DFxA
    pop hl
    jp .updateChannelRegisters

.rest
    ld [hl], 1         ; DFxB
    pop hl
    jr .updateChannelRegisters

.noiseNote
    push hl
    ld de, $DFC6
    ld hl, Data_6E94
    add hl, bc
.loop                   ; Load 5 bytes from the table at Data_6E94 to
    ldi a, [hl]         ; DFC6 - DFCA
    ld [de], a
    inc e
    ld a, e
    cp a, $CB
    jr nz, .loop
    ld c, $20
    ld hl, $DFC4
    jr .loadChannelDataToRegisters

.updateChannelRegisters
    push hl
    ld a, [wMusicCurrentChannel]
    cp a, 1
    jr z, .square1
    cp a, 2
    jr z, .square2
    ld c, LOW(rNR30)    ; NR30 Wave on/off
    ld a, [$DFBF]
    bit 7, a
    jr nz, .label_6CED
    xor a               ; Have you tried turning it off and on again?
    ldh [c], a
    ld a, $80
    ldh [c], a
.label_6CED
    inc c               ; NR31 Length
    inc l
    inc l
    inc l
    inc l
    ldi a, [hl]         ; DFB8
    ld e, a
    ld d, $00
    jr .setChannelRegisters

.square2
    ld c, LOW(rNR21)    ; NR21 Length and duty
    jr .loadChannelDataToRegisters

.square1
    ld c, LOW(rNR10)    ; NR10 is sweep, but this is immediately overwritten
    ld a, $00           ; Bug? XOR A? And A is overwritten anyway
    inc c               ; Bug? NR11 Length and duty
.loadChannelDataToRegisters
    inc l
    inc l
    inc l
    ldd a, [hl]         ; DFx7
    and a
    jr nz, .label_6D57
    ldi a, [hl]         ; DFx6
    ld e, a
.loadLengthAndOrDuty
    inc l
    ldi a, [hl]         ; DFx8
    ld d, a
.setChannelRegisters
    push hl
    inc l
    inc l
    ldi a, [hl]         ; DFxB
    and a
    jr z, .set
    ld e, $01           ; Why 01 and not just 00????
.set
    inc l
    inc l
    ld [hl], $00        ; DFxE
    inc l
    ld a, [hl]          ; DFxF
    pop hl
    bit 7, a            ; Check channel lock, if locked, don't trigger channel
    jr nz, .startNoteTimer
    ld a, d
    ldh [c], a          ; C is here LOW(rNRx1) - TODO Check if the length is
    inc c               ; used?
    ld a, e
    ldh [c], a          ; NRx2
    inc c
    ldi a, [hl]
    ldh [c], a          ; NRx3
    inc c
    ld a, [hl]
    or a, $80           ; Set the highest bit, ensures channel (re)starts
    ldh [c], a          ; NRx4
    ld a, l
    or a, $05
    ld l, a             ; DFxF
    res 0, [hl]         ; Bug? Bit 0 is never used, locks are indicated by bit 7
.startNoteTimer
    pop hl
    dec l
    ldd a, [hl]
    ldd [hl], a         ; DFx2 ← DFx3
    dec l               ; HL points to DFx0 after this
.nextChannel
    ld de, wMusicCurrentChannel
    ld a, [de]
    cp a, 4
    jr z, .out
    inc a
    ld [de], a
    ld de, $0010
    add hl, de
    jp .channelLoop

.out
    ld hl, $DF9E
    inc [hl]
    ld hl, $DFAE
    inc [hl]
    ld hl, $DFBE
    inc [hl]
    ret

; I'm pretty sure this is some kind of bug or unworked feature
.label_6D57
    ld b, $00
    push hl             ; DFx6
    pop hl              ; Hm?
    inc l
    jr .loadLengthAndOrDuty

; E ← [DE + B/2]
LookupVibratoOffset::
    ld a, b
    srl a
    ld l, a
    ld h, $00
    add hl, de
    ld e, [hl]
    ret

ApplyVibrato::
    push hl             ; DFx2?
    ld a, l
    add a, $06
    ld l, a
    ld a, [hl]          ; DFx8 - Length, duty?
    and a, $0F          ; Actual length is inverse of this?
    jr z, .out
    ld [$DF71], a
    ld a, [wMusicCurrentChannel]
    ld c, LOW(rNR13)    ; Low frequency byte
    cp a, 1
    jr z, .vibrato
    ld c, LOW(rNR23)
    cp a, 2
    jr z, .vibrato
    ld c, LOW(rNR33)
    cp a, 3
    jr z, .vibrato
.out
    pop hl
    ret

.vibrato
    inc l
    ldi a, [hl]         ; DFx9
    ld e, a
    ld a, [hl]          ; DFxA
    ld d, a             ; DE ← frequency
    push de
    ld a, l
    add a, $04
    ld l, a
    ld b, [hl]          ; DFxE - Vibrato counter
    ld a, [$DF71]
    cp a, $01
    jr .skip            ; Huh?

._unreachable1
    cp a, $03
    jr ._unreachable2

._unreachable2
    ld hl, -1           ; Constant -1 vibrato? Shouldn't even be audible
    jr ._unreachableApplyVibrato

.skip
    ld de, VibratoOffsets
    call LookupVibratoOffset
    bit 0, b            ; Depending on the parity, use the lower or upper nibble
    jr nz, .maskNibble
    swap e
.maskNibble
    ld a, e
    and a, $0F
    bit 3, a
    jr z, .applyVibratoPositive
    ld h, $FF           ; Make HL negative due to two's complement
    or a, $F0
    jr .applyVibrato
                ; DFE2, DFF2 or DFFA (why tho)
.applyVibratoPositive
    ld h, $00
.applyVibrato
    ld l, a     ; HL is FFFx or 000x
._unreachableApplyVibrato
    pop de
    add hl, de
    ld a, l
    ldh [c], a          ; NRx3 - lower frequency bits
    inc c
    ld a, h
    ldh [c], a          ; NRx4 - higher frequency bits
    jr .out

VibratoOffsets::
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

Data_6EF9::
    db 2, 4, 8, 16, 32, 64
    db 12, 24, 48
    db 5, 0, 1

Data_6F05::
    db 3, 5, 10, 20, 40, 80
    db 15, 30, 60

Data_6F0E::
    db 3, 6, 12, 24, 48, 96
    db 18, 36, 72
    db 8, 16
    db 0

    db 7, 14, 28, 56, 112
    db 21, 42, 84

    db 4, 8, 16, 32, 64, 128
    db 24, 48, 96

Data_6F2B::
    db 4, 9, 18, 36, 72, 144
    db 27, 54, 108
    db 12, 24

    db 4, 10, 20, 40, 80, 160
    db 30

    dw $783C

INCBIN "baserom.gb", $6F3F, $7FF0 - $6F3F

SECTION "Footer", ROM0[$7FF0]
UpdateAudio::
    jp _UpdateAudio
InitAudio::
    jp _InitAudio

; vim: set expandtab tabstop=4 shiftwidth=4 
