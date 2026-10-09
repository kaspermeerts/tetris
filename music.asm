INCLUDE "music_macros.asm"

SECTION "Music", ROM0[$6F3F]

Song_TopScore::
	db $00
	dw NoteLengths3
	dw Channel1_TopScore
	dw Channel2_TopScore
	dw Channel3_TopScore
	dw Channel4_TopScore

Song_StageClear::
	db $00
	dw NoteLengths2
	dw Channel1_StageClear
	dw Channel2_StageClear
	dw Channel3_StageClear
	dw Channel4_StageClear

Song_TitleScreen::
	db $00
	dw NoteLengths3
	dw Channel1_TitleScreen
	dw Channel2_TitleScreen
	dw Channel3_TitleScreen
	dw Channel4_TitleScreen

Song_GameOver::
	db $00
	dw NoteLengths1
	dw Channel1_GameOver
	dw Channel2_GameOver
	dw Channel3_GameOver
	dw $0000

Song_TypeA::
	db $00
	dw NoteLengths3
	dw Channel1_TypeA
	dw Channel2_TypeA
	dw Channel3_TypeA
	dw Channel4_TypeA

Song_TypeB::
	db $00
	dw NoteLengths3
	dw Channel1_TypeB
	dw Channel2_TypeB
	dw Channel3_TypeB
	dw Channel4_TypeB

Song_TypeC::
	db $00
	dw NoteLengths3
	dw Channel1_TypeC
	dw Channel2_TypeC
	dw $0000
	dw $0000

Song_Danger::
	db $00
	dw NoteLengths2
	dw Channel1_Danger
	dw Channel2_Danger
	dw Channel3_Danger
	dw Channel4_Danger

Song_MultiplayerRoundOver::
	db $00
	dw NoteLengths3
	dw Channel1_MultiplayerRoundOver
	dw Channel2_MultiplayerRoundOver
	dw Channel3_MultiplayerRoundOver
	dw Channel4_MultiplayerRoundOver

Song_TypeBJingle1::
	db $00
	dw NoteLengths3
	dw $0000
	dw Channel2_TypeBJingle1
	dw $0000
	dw $0000

Song_TypeBJingle2::
	db $00
	dw NoteLengths3
	dw $0000
	dw Channel2_TypeBJingle2
	dw Channel3_TypeBJingle2
	dw $0000

Song_TypeBJingle3::
	db $00
	dw NoteLengths3
	dw Channel1_TypeBJingle3
	dw Channel2_TypeBJingle3
	dw Channel3_TypeBJingle3
	dw $0000

Song_TypeBJingle4::
	db $00
	dw NoteLengths3
	dw Channel1_TypeBJingle4
	dw Channel2_TypeBJingle4
	dw Channel3_TypeBJingle4
	dw Channel4_TypeBJingle4

Song_TypeBJingle5::
	db $00
	dw NoteLengths3
	dw Channel1_TypeBJingle5
	dw Channel2_TypeBJingle5
	dw Channel3_TypeBJingle5
	dw Channel4_TypeBJingle5

Song_TypeBJingle6::
	db $00
	dw NoteLengths3
	dw Channel1_TypeBJingle6
	dw Channel2_TypeBJingle6
	dw Channel3_TypeBJingle6
	dw Channel4_TypeBJingle6

Song_RocketLaunch::
	db $00
	dw NoteLengths4
	dw Channel1_RocketLaunch
	dw Channel2_RocketLaunch
	dw Channel3_RocketLaunch
	dw $0000

Song_MultiplayerVictory::
	db $00
	dw NoteLengths3
	dw Channel1_MultiplayerVictory
	dw Channel2_MultiplayerVictory
	dw Channel3_MultiplayerVictory
	dw $0000

Channel2_TypeC::
	dw Section_TypeC1, Section_TypeC2, Section_TypeC1, Section_TypeC3, Section_TypeC7, $FFFF, Channel2_TypeC

Channel1_TypeC::
	dw Section_TypeC4, Section_TypeC5, Section_TypeC4, Section_TypeC6, Section_TypeC8, $FFFF, Channel1_TypeC

Section_TypeC1::
	db $9D, $74, $00, $41
	db $A2
	Notes G_, 7, D#, 8, C#, 9, D#, 8, F_, 7, D#, 8, G_, 7, D#, 8, C#, 7, D#, 8, B_, 6, D#, 8, G_, 7, D#, 8, C#, 9, D#, 8, F_, 7, D#, 8, G_, 7, D#, 8, C#, 7, D#, 8, B_, 6, D#, 8
	EndSection

Section_TypeC2::
	Notes G_, 7, D#, 8, G_, 7, C#, 7, F_, 8, B_, 7, F_, 7, B_, 7, F_, 7, A_, 6, D#, 8, G_, 7, C#, 7, D#, 8, B_, 7, G_, 7, F_, 7, C#, 7, B_, 6, D#, 6, B_, 6, F_, 7, D#, 8, B_, 7
	EndSection

Section_TypeC3::
	Notes G_, 7, D#, 8, G_, 7, C#, 7, F_, 8, B_, 7, F_, 7, B_, 7, F_, 7, A_, 6, A_, 8, B_, 7, D#, 8, A_, 8, D#, 8, G_, 7, A_, 6, F_, 7
	db $A8
	Notes G_, 7
	EndSection

Section_TypeC4::
	db $9D, $64, $00, $41
	db $A3
	Notes C#, 5, C#, 7, B_, 6, C#, 5, G_, 5, D#, 6, C#, 7, F_, 6, D#, 6, C#, 7, G_, 5, D#, 6
	EndSection

Section_TypeC5::
	Notes C#, 5, C#, 7, B_, 5, A_, 4, A_, 6, G_, 5, F_, 4, F_, 6, B_, 5
	db $A2
	Notes D#, 6, F_, 6, D#, 6, B_, 5, G_, 5, F_, 5
	EndSection

Section_TypeC6::
	db $A3
	Notes C#, 5, C#, 7, B_, 5, A_, 4, A_, 6, F_, 5, G_, 5, D#, 6, D#, 6, G_, 5, A_, 4, G_, 3
	EndSection

Section_TypeC7::
	db $A2
	Notes A_, 8, F_, 8, D#, 8, B_, 7, G_, 7, F_, 7, G_, 7, B_, 7, D#, 8, G_, 7, B_, 7, F_, 8, D#, 8, F_, 8
	db $A3
	Notes A_, 8, F_, 7
	db $A2
	Notes G_, 7, B_, 7
	db $A3
	Notes D#, 8, B_, 7, D#, 8, C#, 9, G_, 8
	db $A2
	Notes C#, 9, F_, 9
	db $A3
	Notes G_, 9, F_, 9
	db $A2
	Notes C#, 9, A_, 8, G_, 8, D#, 8, G_, 8, C#, 8
	db $A8
	Notes D#, 8
	db $A7
	Notes A_, 8
	db $A1
	Notes C#, 9, D#, 9
	db $A3
	Notes C#, 9
	db $A2
	Notes A_, 8, F_, 8, A_, 8, D#, 8, F_, 8, B_, 7
	db $A7
	Notes C#, 9
	db $A1
	Notes F_, 9, G_, 9
	db $A3
	Notes F_, 9
	db $A2
	Notes C#, 9, B_, 8, C#, 9, G_, 8, B_, 8, D#, 8, F_, 9, B_, 8, D#, 8, B_, 8, F_, 9, B_, 9, F_, 10, B_, 8, D#, 10, B_, 8, B_, 9, B_, 8
	db $A3
	Notes G_, 9
	db $A2
	Notes B_, 9, G_, 9, F_, 9, G_, 9
	db $A1
	Notes C#, 9, F_, 9
	db $A4
	Notes C#, 9
	db $A2
	Rest
	EndSection

Section_TypeC8::
	db $A2
	Notes D#, 6, A_, 6, G_, 7, A_, 6, B_, 5, A_, 6, D#, 6, A_, 6, G_, 5, A_, 6, F_, 5, A_, 6, G_, 5, A_, 6, G_, 7, A_, 6, B_, 5, A_, 6, D#, 6, A_, 6, G_, 5, A_, 6, F_, 5, A_, 6, G_, 5, D#, 6, G_, 5, C#, 5, C#, 7, G_, 6, C#, 6, G_, 6, F_, 5, G_, 6, C#, 6, G_, 6
	db $A3
	Notes D#, 6, F_, 7, F_, 5
	db $A2
	Notes D#, 6, A_, 6, F_, 7, A_, 6, B_, 5, A_, 6, A_, 5, D#, 6, C#, 5, D#, 6, A_, 5, D#, 6
	db $A8
	Notes B_, 5
	db $A2
	Notes C#, 6, G_, 6, F_, 5, G_, 6, C#, 6, G_, 6
	db $A8
	Notes D#, 6
	db $A3
	Notes D#, 6, F_, 5, B_, 4, D#, 4, G_, 4, B_, 4, G_, 5, B_, 5, D#, 6
	db $A8
	Notes C#, 5
	EndSection

Channel2_TypeA::
	dw Section_TypeA1, Section_TypeA1, Section_TypeA2, $FFFF, Channel2_TypeA

Channel1_TypeA::
	dw Section_TypeA3, Section_TypeA3, Section_TypeA4, $FFFF, Channel1_TypeA

Channel3_TypeA::
	dw Section_TypeA5, Section_TypeA5, Section_TypeA6, Section_TypeA6, $FFFF, Channel3_TypeA

Channel4_TypeA::
	dw Section_TypeA7, $FFFF, Channel4_TypeA

Section_TypeA1::
	db $9D, $84, $00, $81
	db $A3
	Notes A_, 8
	db $A2
	Notes B_, 7, C#, 8
	db $A3
	Notes F_, 8
	db $A2
	Notes C#, 8, B_, 7
	db $A3
	Notes G_, 7
	db $A2
	Notes G_, 7, C#, 8
	db $A3
	Notes A_, 8
	db $A2
	Notes F_, 8, C#, 8
	db $A7
	Notes B_, 7
	db $A2
	Notes C#, 8
	db $A3
	Notes F_, 8, A_, 8
	db $A3
	Notes C#, 8, G_, 7, G_, 7
	Rest
	db $A2
	Rest
	db $A3
	Notes F_, 8
	db $A2
	Notes B_, 8
	db $A3
	Notes G_, 9
	db $A2
	Notes D#, 9, B_, 8
	db $A7
	Notes A_, 8
	db $A2
	Notes C#, 8
	db $A3
	Notes A_, 8
	db $A2
	Notes F_, 8, C#, 8
	db $A3
	Notes B_, 7
	db $A2
	Notes B_, 7, C#, 8
	db $A3
	Notes F_, 8, A_, 8
	db $A3
	Notes C#, 8, G_, 7, G_, 7
	Rest
	EndSection

Section_TypeA2::
	db $9D, $50, $00, $81
	db $A4
	Notes A_, 6, C#, 6, F_, 6, B_, 5
	db $A4
	Notes C#, 6, G_, 5
	db $A8
	Notes F_, 5
	db $A3
	Rest
	db $A4
	Notes A_, 6, C#, 6, F_, 6, B_, 5
	db $A3
	Notes C#, 6, A_, 6
	db $A4
	Notes G_, 7, F_, 7
	Rest
	EndSection

Section_TypeA3::
	db $9D, $43, $00, $81
	db $A3
	Notes B_, 7
	db $A2
	Notes F_, 7, G_, 7, B_, 7
	db $A1
	Notes A_, 8, F_, 8
	db $A2
	Notes G_, 7, F_, 7
	db $A7
	Notes A_, 6
	db $A2
	Notes G_, 7, C#, 8
	Rest
	db $A2
	Notes B_, 7, G_, 7
	db $A1
	Notes F_, 7, F_, 7
	db $A2
	Notes A_, 6, F_, 7, G_, 7
	db $A3
	Notes B_, 7, C#, 8
	db $A3
	Notes G_, 7, A_, 6, A_, 6
	Rest
	db $A2
	Notes F_, 4
	db $A3
	Notes B_, 6
	db $A2
	Notes G_, 7, C#, 8
	db $A1
	Notes C#, 8, C#, 8
	db $A2
	Notes B_, 7, G_, 7
	db $A7
	Notes D#, 7
	db $A2
	Notes A_, 6, D#, 7
	db $A1
	Notes G_, 7, D#, 7
	db $A2
	Notes B_, 6, A_, 6, F_, 7, A_, 6, F_, 7, G_, 7, B_, 7, F_, 7, C#, 8, F_, 7
	db $A1
	Notes G_, 7, C#, 8, A_, 6
	Rest
	db $A3
	Notes A_, 6, A_, 6
	Rest
	EndSection

Section_TypeA4::
	db $9D, $30, $00, $81
	db $A4
	Notes C#, 6, G_, 5, B_, 5, F_, 5, G_, 5, A_, 4
	db $A4
	Notes A_, 4
	db $A3
	Notes B_, 5
	Rest
	db $A4
	Notes C#, 6, G_, 5, B_, 5, F_, 5
	db $A3
	Notes G_, 5, C#, 6
	db $A4
	Notes A_, 6, F_, 6
	Rest
	EndSection

Section_TypeA5::
	db $9D, LOW(KorobeinikiWavePattern), HIGH(KorobeinikiWavePattern), $20
	db $A2
	Notes A_, 4, A_, 6, A_, 4, A_, 6, A_, 4, A_, 6, A_, 4, A_, 6, G_, 5, G_, 7, G_, 5, G_, 7, G_, 5, G_, 7, G_, 5, G_, 7, F_, 5, F_, 7, F_, 5, F_, 7, A_, 4, A_, 6, A_, 4, A_, 6, G_, 5, G_, 7, G_, 5, G_, 7, G_, 5, G_, 7, B_, 5, C#, 6, F_, 6, F_, 4
	Rest
	Notes F_, 4
	Rest
	Notes F_, 4, G_, 5, B_, 4, C#, 4, C#, 6
	Rest
	Notes C#, 6, C#, 4, D#, 5, D#, 5
	Rest
	Notes B_, 5, B_, 7
	Rest
	Notes B_, 7
	Rest
	Notes A_, 6
	Rest
	Notes F_, 7, G_, 5, A_, 6, G_, 5, A_, 6
	db $A3
	Notes G_, 5
	Rest
	EndSection

Section_TypeA6::
	db $9D, LOW(KorobeinikiWavePattern), HIGH(KorobeinikiWavePattern), $20
	db $A2
	Notes G_, 7, A_, 8, G_, 7, A_, 8, G_, 7, A_, 8, G_, 7, A_, 8, F_, 7, A_, 8, F_, 7, A_, 8, F_, 7, A_, 8, F_, 7, A_, 8, G_, 7, A_, 8, G_, 7, A_, 8, G_, 7, A_, 8, G_, 7, A_, 8, F_, 7, A_, 8, F_, 7, A_, 8
	db $A4
	Rest
	EndSection

Section_TypeA7::
	db $A2
	Rest
	Noise 1
	Rest
	Noise 1
	Rest
	db $A1
	Noise 1, 1
	db $A2
	Rest
	Noise 1
	Rest
	Noise 1
	Rest
	Noise 1
	Rest
	Noise 1, 1, 1
	EndSection

Channel2_TypeB::
	dw Section_TypeB2, Section_TypeB6, Section_TypeB8, Section_TypeB8, Section_TypeB11, $FFFF, Channel2_TypeB

Channel1_TypeB::
	dw Section_TypeB1, Section_TypeB5, Section_TypeB9, Section_TypeB9, Section_TypeB12, $FFFF, Channel1_TypeB

Channel3_TypeB::
	dw Section_TypeB3, Section_TypeB7, Section_TypeB10, Section_TypeB10, Section_TypeB10, Section_TypeB10, Section_TypeB10, Section_TypeB10, Section_TypeB13, Section_TypeB14, Section_TypeB14, Section_TypeB14, Section_TypeB15, Section_TypeB16, Section_TypeB16, Section_TypeB17, Section_TypeB17, Section_TypeB18, Section_TypeB18, Section_TypeB17, Section_TypeB19, $FFFF, Channel3_TypeB

Channel4_TypeB::
	dw Section_TypeB4, $FFFF, Channel4_TypeB

Section_TypeB1::
	db $A5
	Rest
	EndSection

Section_TypeB2::
	db $9D, $62, $00, $80
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes B_, 5, B_, 5, A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes B_, 5, B_, 5
	EndSection

Section_TypeB3::
	db $9D, LOW(DefaultWavePattern), HIGH(DefaultWavePattern), $A0
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes B_, 5, B_, 5, A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes B_, 5, B_, 5
	EndSection

Section_TypeB4::
	db $A2
	Noise 1
	db $A1
	Noise 1, 1
	db $A2
	Noise 1, 1
	EndSection

Section_TypeB5::
	db $A5
	Rest
	EndSection

Section_TypeB6::
	db $9D, $32, $00, $80
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes B_, 5, B_, 5, A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes B_, 5, B_, 5
	EndSection

Section_TypeB7::
	db $9D, LOW(DefaultWavePattern), HIGH(DefaultWavePattern), $A0
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes B_, 5, B_, 5, A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes B_, 5, B_, 5
	EndSection

Section_TypeB8::
	db $9D, $82, $00, $80
	db $A2
	Notes A_, 6, B_, 7, A_, 8, G_, 8, A_, 8
	db $A1
	Notes B_, 7, B_, 7
	db $A2
	Notes C#, 8, G_, 7, B_, 7
	db $A1
	Notes D#, 7, D#, 7
	db $A2
	Notes G_, 7, C#, 7, D#, 7
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes C#, 7, G_, 6, A_, 6, B_, 5, C#, 6, G_, 6, A_, 6, B_, 5, C#, 6, C#, 7
	EndSection

Section_TypeB9::
	db $9D, $53, $00, $40
	db $A2
	Notes B_, 5, D#, 7, D#, 7, G_, 7, D#, 7
	db $A1
	Notes C#, 7, D#, 7
	db $A2
	Notes G_, 7, C#, 7, D#, 7
	db $A1
	Notes G_, 6, A_, 6
	db $A2
	Notes C#, 7, G_, 6, A_, 6
	db $A1
	Notes A_, 5, B_, 5
	db $A2
	Notes G_, 6, B_, 5, B_, 5, D#, 5, G_, 5, G_, 5, B_, 5, D#, 5, G_, 5, G_, 6
	EndSection

Section_TypeB10::
	db $9D, LOW(DefaultWavePattern), HIGH(DefaultWavePattern), $A0
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes B_, 5, B_, 5, A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes B_, 5, B_, 5
	EndSection

Section_TypeB11::
	db $A8
	Notes A_, 6
	db $A2
	Notes C#, 7, G_, 6
	db $A8
	Notes A_, 6
	db $A3
	Notes C#, 7
	db $A2
	Notes D#, 7
	db $A1
	Notes D#, 7, D#, 7
	db $A2
	Notes G_, 7, C#, 7, D#, 7
	db $A1
	Notes D#, 7, D#, 7
	db $A2
	Notes G_, 7, C#, 7
	db $A8
	Notes D#, 7
	db $A3
	Notes G_, 7
	db $A2
	Notes B_, 7
	db $A1
	Notes B_, 7, B_, 7
	db $A2
	Notes C#, 8, G_, 7, B_, 7
	db $A1
	Notes B_, 7, B_, 7
	db $A2
	Notes C#, 8, G_, 7
	db $A8
	Notes B_, 7
	db $A3
	Notes D#, 8
	db $A2
	Notes F_, 8
	db $A1
	Notes F_, 8, F_, 8
	db $A2
	Notes F_, 8, F_, 8, A_, 8, F_, 8, F_, 8, D#, 8, F_, 8
	db $A1
	Notes F_, 8, F_, 8
	db $A2
	Notes F_, 8, F_, 8, A_, 8, F_, 8, F_, 8, D#, 8, F_, 8
	db $A1
	Notes F_, 8, F_, 8
	db $A2
	Notes F_, 8, F_, 8, D#, 8
	db $A1
	Notes D#, 8, D#, 8
	db $A2
	Notes D#, 8, D#, 8, C#, 8
	db $A1
	Notes C#, 8, C#, 8
	db $A2
	Notes C#, 8, G_, 7, C#, 7, D#, 7, G_, 7, F_, 6, G_, 7
	db $A1
	Notes D#, 7, D#, 7
	db $A2
	Notes F_, 6
	db $A3
	Notes D#, 7
	db $A1
	Notes F_, 6, A_, 6
	db $A2
	Notes F_, 6, B_, 5, G_, 7
	db $A1
	Notes D#, 7, D#, 7
	db $A2
	Notes F_, 6
	db $A3
	Notes D#, 7
	db $A1
	Notes F_, 6, A_, 6
	db $A2
	Notes F_, 6, A_, 5
	db $A5
	Notes F_, 6
	db $A8
	Rest
	db $A3
	Notes G_, 6
	EndSection

Section_TypeB12::
	db $A8
	Notes B_, 5
	db $A2
	Notes B_, 5, B_, 5
	db $A8
	Notes B_, 5
	db $A3
	Notes F_, 6
	db $A5
	Rest
	db $A8
	Rest
	db $A3
	Notes C#, 7
	db $A2
	Notes D#, 7
	db $A1
	Notes D#, 7, D#, 7
	db $A2
	Notes G_, 7, C#, 7, D#, 7
	db $A1
	Notes D#, 7, D#, 7
	db $A2
	Notes G_, 7, C#, 7
	db $A8
	Notes F_, 6
	db $A3
	Notes A_, 6
	db $A2
	Notes C#, 7
	db $A1
	Notes D#, 7, G_, 7
	db $A2
	Notes C#, 7, G_, 7, B_, 7, B_, 7, B_, 7, A_, 6, C#, 7
	db $A1
	Notes D#, 7, G_, 7
	db $A2
	Notes C#, 7, G_, 7, A_, 7, A_, 7, A_, 7, A_, 6, C#, 7
	db $A1
	Notes D#, 7, G_, 7
	db $A2
	Notes C#, 7, G_, 7, A_, 6
	db $A1
	Notes C#, 7, D#, 7
	db $A2
	Notes A_, 6, D#, 7, A_, 6
	db $A1
	Notes C#, 7, D#, 7
	db $A2
	Notes C#, 7, C#, 7, G_, 5, A_, 6, C#, 7, C#, 5, B_, 5
	db $A1
	Notes B_, 5, B_, 5
	db $A2
	Notes B_, 5
	db $A3
	Notes B_, 5
	db $A1
	Notes B_, 5, D#, 6
	db $A2
	Notes B_, 5, D#, 5, A_, 5
	db $A1
	Notes A_, 5, A_, 5
	db $A2
	Notes A_, 5
	db $A3
	Notes A_, 5
	db $A1
	Notes A_, 5, C#, 6
	db $A2
	Notes A_, 5, D#, 5
	db $A5
	Notes C#, 5
	db $A8
	Rest
	db $A3
	Notes G_, 5
	EndSection

Section_TypeB13::
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes C#, 6, G_, 5, A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes G_, 6, B_, 5, A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes C#, 6, G_, 5, A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes G_, 5, F_, 4
	EndSection

Section_TypeB14::
	db $A2
	Notes D#, 5
	db $A1
	Notes D#, 7, D#, 5
	db $A2
	Notes F_, 4, F_, 6, D#, 5
	db $A1
	Notes D#, 7, D#, 5
	db $A2
	Notes F_, 4, F_, 6
	EndSection

Section_TypeB15::
	db $A2
	Notes D#, 5
	db $A1
	Notes D#, 7, D#, 5
	db $A2
	Notes F_, 4, F_, 6, D#, 5
	db $A1
	Notes D#, 7, D#, 5
	db $A2
	Notes G_, 5, G_, 7
	EndSection

Section_TypeB16::
	db $A2
	Notes F_, 4
	db $A1
	Notes F_, 6, F_, 4
	db $A2
	Notes F_, 4, F_, 6, D#, 5
	db $A1
	Notes D#, 7, D#, 5
	db $A2
	Notes D#, 5, D#, 7
	EndSection

Section_TypeB17::
	db $A2
	Notes F_, 4
	db $A1
	Notes F_, 6, F_, 4
	db $A2
	Notes F_, 4, F_, 6, F_, 4
	db $A1
	Notes F_, 6, F_, 4
	db $A2
	Notes F_, 4, F_, 6
	EndSection

Section_TypeB18::
	db $A2
	Notes A_, 4
	db $A1
	Notes A_, 6, A_, 4
	db $A2
	Notes A_, 4, A_, 6, A_, 4
	db $A1
	Notes A_, 6, A_, 4
	db $A2
	Notes A_, 4, A_, 6
	EndSection

Section_TypeB19::
	db $A2
	Notes F_, 4
	db $A1
	Notes F_, 6, F_, 4
	db $A2
	Notes F_, 4, F_, 6, F_, 4
	db $A1
	Notes F_, 6, F_, 4
	db $A2
	db $A4
	Notes C#, 7
	EndSection
	Notes F_, 6, C#, 7, G_, 7
	db $A4
	Notes G_, 7
Channel1_RocketLaunch::
	dw Section_RocketLaunch1
.loop
	dw Section_RocketLaunch3, $FFFF, Channel1_RocketLaunch.loop

Channel2_RocketLaunch::
	dw Section_RocketLaunch2, $FFFF, Channel2_RocketLaunch

Channel3_RocketLaunch::
	dw Section_RocketLaunch4, $FFFF, Channel3_RocketLaunch

Section_RocketLaunch1::
	db $9D, $20, $00, $81
	db $AA
	Rest
	EndSection

Section_RocketLaunch2::
	db $9D, $70, $00, $81

Section_RocketLaunch3::
	db $A2
	Notes F_, 7, C#, 6, G_, 6, F_, 7, A_, 7, D#, 6, B_, 6, A_, 7, C#, 8, G_, 6, F_, 7, C#, 8, D#, 8, B_, 6, F_, 7, D#, 8, A_, 7, D#, 6, B_, 6, A_, 7, D#, 7, A_, 5, D#, 6, D#, 7
	EndSection

Section_RocketLaunch4::
	db $9D, LOW(DefaultWavePattern), HIGH(DefaultWavePattern), $21
	db $A8
	Notes F_, 7
	db $A3
	Notes F_, 5
	db $A8
	Notes F_, 7
	db $A3
	Notes F_, 5
	db $A8
	Notes F_, 7
	db $A3
	Notes F_, 5
	EndSection

Channel1_MultiplayerVictory::
	dw Section_MultiplayerVictory1
.loop
	dw Section_MultiplayerVictory3, $FFFF, Channel1_MultiplayerVictory.loop

Channel2_MultiplayerVictory::
	dw Section_MultiplayerVictory2, $FFFF, Channel2_MultiplayerVictory

Channel3_MultiplayerVictory::
	dw Section_MultiplayerVictory4, $FFFF, Channel3_MultiplayerVictory

Section_MultiplayerVictory1::
	db $9D, $20, $00, $81
	db $AA
	Rest
	EndSection

Section_MultiplayerVictory2::
	db $9D, $70, $00, $81

Section_MultiplayerVictory3::
	db $A2
	Notes D#, 8, F_, 7, G_, 8, F_, 7, B_, 8, F_, 7, G_, 8, F_, 7, C#, 9, F_, 7, B_, 8, F_, 7, G_, 8, F_, 7, B_, 8, F_, 7, D#, 8, F_, 7, G_, 8, F_, 7, B_, 8, F_, 7, G_, 8, F_, 7, C#, 9, F_, 7, B_, 8, F_, 7, G_, 8, F_, 7, B_, 8, F_, 7, F_, 9, A_, 7, C#, 9, A_, 7, B_, 8, A_, 7, G_, 8, A_, 7, F_, 8, A_, 7, G_, 8, A_, 7, B_, 8, A_, 7, G_, 8, A_, 7, G_, 8, C#, 7, D#, 8, C#, 7, D#, 8, C#, 7, C#, 8, C#, 7, C#, 8, C#, 7, A_, 7, C#, 7, C#, 8, C#, 7, G_, 8, C#, 7
	EndSection

Section_MultiplayerVictory4::
	db $9D, LOW(DefaultWavePattern), HIGH(DefaultWavePattern), $21
	db $A5
	Notes D#, 8, C#, 8, A_, 7, F_, 7, G_, 6, C#, 7, F_, 7, F_, 7
	EndSection

Channel2_GameOver::
	dw Section_GameOver1, $0000

Channel1_GameOver::
	dw Section_GameOver2

Channel3_GameOver::
	dw Section_GameOver3

Section_GameOver1::
	db $9D, $B2, $00, $80
	db $A2
	Notes B_, 9, G_, 9, B_, 9, G_, 9, B_, 9, C#, 10, B_, 9, G_, 9
	db $A4
	Notes B_, 9
	EndSection

Section_GameOver2::
	db $9D, $92, $00, $80
	db $A2
	Notes A_, 8, F_, 8, A_, 8, F_, 8, A_, 8, B_, 8, A_, 8, F_, 8
	db $A4
	Notes A_, 8
Section_GameOver3::
	db $9D, LOW(DefaultWavePattern), HIGH(DefaultWavePattern), $20
	db $A2
	Notes C#, 10, B_, 9, C#, 10, B_, 9, C#, 10, F_, 10, C#, 10, B_, 9
	db $A3
	Notes C#, 10
	Rest

Channel2_TitleScreen::
	dw Section_TitleScreen1, Section_TitleScreen5, Section_TitleScreen5, $0000

Channel1_TitleScreen::
	dw Section_TitleScreen2, Section_TitleScreen6, Section_TitleScreen13

Channel3_TitleScreen::
	dw Section_TitleScreen3, Section_TitleScreen7, Section_TitleScreen7, Section_TitleScreen8, Section_TitleScreen7, Section_TitleScreen7, Section_TitleScreen9, Section_TitleScreen8, Section_TitleScreen7, Section_TitleScreen7, Section_TitleScreen9, Section_TitleScreen8, Section_TitleScreen10, Section_TitleScreen11, Section_TitleScreen9, Section_TitleScreen8, Section_TitleScreen7

Channel4_TitleScreen::
	dw Section_TitleScreen4, Section_TitleScreen4, Section_TitleScreen12, Section_TitleScreen12, Section_TitleScreen12, Section_TitleScreen12

Section_TitleScreen1::
	db $9D, $C3, $00, $80
	db $A2
	Notes B_, 6, C#, 7, B_, 6, C#, 7, G_, 6, G_, 8
	db $A3
	Rest
	db $A2
	Notes B_, 6, C#, 7, B_, 6, C#, 7, G_, 6, G_, 8
	db $A3
	Rest
	db $A2
	Rest
	Notes B_, 7
	Rest
	Notes A_, 7
	Rest
	Notes F_, 7
	Rest
	Notes A_, 7
	db $A1
	Notes F_, 7, A_, 7
	db $A2
	Notes F_, 7, F_, 7, G_, 6
	db $A3
	Notes B_, 6
	Rest
	db $A2
	Notes C#, 7, F_, 7, C#, 7, F_, 7, B_, 6, B_, 8
	db $A3
	Rest
	db $A2
	Notes C#, 7, F_, 7, C#, 7, F_, 7, B_, 6, B_, 8
	db $A3
	Rest
	db $A2
	Rest
	Notes C#, 9
	Rest
	Notes B_, 8
	Rest
	Notes B_, 8
	Rest
	Notes G_, 8
	db $A2
	Rest
	db $A1
	Notes G_, 8, B_, 8
	db $A2
	Notes G_, 8, F_, 8
	db $A3
	Notes G_, 8
	Rest
	EndSection

Section_TitleScreen2::
	db $9D, $74, $00, $80
	db $A2
	Notes F_, 6, G_, 6, F_, 6, G_, 6, A_, 5, C#, 7
	db $A3
	Rest
	db $A2
	Notes F_, 6, G_, 6, F_, 6, G_, 6, A_, 5, C#, 7
	db $A3
	Rest
	db $A2
	Rest
	Notes F_, 6
	Rest
	Notes F_, 6
	Rest
	Notes C#, 6
	Rest
	Notes F_, 6, F_, 6, C#, 6, C#, 6, B_, 5
	db $A3
	Notes F_, 6
	Rest
	db $A2
	Notes G_, 6, B_, 6, G_, 6, B_, 6, F_, 6, F_, 8
	db $A3
	Rest
	db $A2
	Notes G_, 6, B_, 6, G_, 6, B_, 6, F_, 6, F_, 8
	db $A3
	Rest
	db $A2
	Rest
	Notes G_, 8
	Rest
	Notes F_, 8
	Rest
	Notes A_, 7
	Rest
	Notes A_, 7
	db $A2
	Rest
	db $A1
	Notes B_, 7, F_, 8
	db $A2
	Notes B_, 7, A_, 7
	db $A3
	Notes D#, 7
	Rest
	EndSection

Section_TitleScreen3::
	db $9D, LOW(DefaultWavePattern), HIGH(DefaultWavePattern), $20
	db $A2
	Notes B_, 7, A_, 7, B_, 7, A_, 7, C#, 7, G_, 4
	db $A3
	Rest
	db $A2
	Notes B_, 7, A_, 7, B_, 7, A_, 7, C#, 7, G_, 4
	db $A3
	Rest
	db $A2
	Notes A_, 5, B_, 6, A_, 5, B_, 4, B_, 4, B_, 4, B_, 4, B_, 6, F_, 5, C#, 7, F_, 5, C#, 7
	db $A6
	Notes A_, 5
	db $A3
	Rest
	db $A1
	Rest
	db $A2
	Notes B_, 7, A_, 7, B_, 7, A_, 7, A_, 5, A_, 5
	db $A3
	Rest
	db $A2
	Notes B_, 7, A_, 7, B_, 7, A_, 7, A_, 5, A_, 5
	db $A3
	Rest
	db $A2
	Notes F_, 5, B_, 6, F_, 5, B_, 6, A_, 5, C#, 7, A_, 5, C#, 7, A_, 5, F_, 7, A_, 5, F_, 7
	db $A6
	Notes G_, 6
	db $A3
	Rest
	db $A1
	Rest
	EndSection

Section_TitleScreen4::
	db $A8
	Rest
	db $A2
	Noise 1, 2
	db $A8
	Rest
	db $A2
	Noise 1, 2
	db $A5
	Rest
	Rest
	EndSection

Section_TitleScreen5::
	db $9D, $C5, $00, $80
	db $A1
	Notes A_, 7, C#, 8
	db $A4
	Notes A_, 7
	db $A2
	Rest
	db $A3
	Notes G_, 8
	db $A8
	Notes C#, 8
	db $A3
	Rest
	db $A1
	Notes F_, 7, A_, 7
	db $A4
	Notes F_, 7
	db $A2
	Rest
	db $A3
	Notes F_, 8
	db $A1
	Notes F_, 8, G_, 8
	db $A4
	Notes A_, 7
	db $A7
	Rest
	db $A1
	Notes D#, 7, A_, 7
	db $A4
	Notes D#, 7
	db $A2
	Rest
	db $A3
	Notes A_, 7
	db $A1
	Notes A_, 7, C#, 8
	db $A4
	Notes F_, 7
	db $A7
	Rest
	db $A1
	Notes F_, 6, G_, 6
	db $A4
	Notes F_, 6
	db $A2
	Rest
	db $A3
	Notes B_, 6
	db $A7
	Notes F_, 7
	db $A4
	Notes D#, 7
	db $A2
	Rest
	EndSection

Section_TitleScreen6::
	db $9D, $84, $00, $41
	db $A1
	Notes D#, 7, F_, 7
	db $A4
	Notes D#, 7
	db $A2
	Rest
	db $A3
	Notes D#, 7
	db $A8
	Notes F_, 7
	db $A3
	Rest
	db $A1
	Notes B_, 6, D#, 7
	db $A4
	Notes B_, 6
	db $A2
	Rest
	db $A3
	Notes B_, 6
	db $A1
	Notes B_, 6, D#, 7
	db $A4
	Notes D#, 7
	db $A7
	Rest
	db $A1
	Notes F_, 6, C#, 6
	db $A4
	Notes A_, 5
	db $A2
	Rest
	db $A3
	Notes D#, 7
	db $A1
	Notes F_, 6, G_, 6
	db $A4
	Notes C#, 6
	db $A7
	Rest
	db $A1
	Notes A_, 5, C#, 6
	db $A4
	Notes A_, 5
	db $A2
	Rest
	db $A3
	Notes F_, 5
	db $A7
	Notes B_, 5
	db $A4
	Notes A_, 5
	db $A2
	Rest
	EndSection

Section_TitleScreen7::
	db $A2
	Notes G_, 6, G_, 6
	Rest
	Notes G_, 6, G_, 6, G_, 6
	Rest
	Notes G_, 6
	EndSection
Section_TitleScreen8::
	Notes A_, 5, A_, 5
	Rest
	Notes A_, 5, A_, 5, A_, 5
	Rest
	Notes A_, 5
	EndSection
Section_TitleScreen9::
	Notes F_, 5, F_, 5
	Rest
	Notes F_, 5, F_, 5, F_, 5
	Rest
	Notes F_, 5
	EndSection
Section_TitleScreen10::
	db $A2
	Notes G_, 6, G_, 6
	Rest
	Notes G_, 6, F_, 6, F_, 6
	Rest
	Notes F_, 6
	EndSection
Section_TitleScreen11::
	Notes C#, 6, C#, 6
	Rest
	Notes C#, 6, A_, 5, A_, 5
	Rest
	Notes A_, 5
	EndSection

Section_TitleScreen12::
	db $A2
	Noise 1, 2
	Rest
	Noise 1, 1, 2
	Rest
	Noise 1, 1, 2
	Rest
	Noise 1, 1, 2
	Rest
	Noise 1, 1, 2
	Rest
	Noise 1, 1, 2
	Rest
	Noise 1, 1, 2
	Rest
	Noise 1
	Rest
	Noise 2
	Rest
	Noise 2
	EndSection

Section_TitleScreen13::
	db $9D, $66, $00, $81
	db $A7
	Notes D#, 9, F_, 9
	db $A3
	Notes D#, 9
	db $A7
	Notes A_, 9
	db $A4
	Notes F_, 9
	db $A2
	Rest
	db $A7
	Notes G_, 8, B_, 8
	db $A3
	Notes D#, 9
	db $A7
	Notes F_, 9
	db $A4
	Notes D#, 9
	db $A2
	Rest
	db $A7
	Notes G_, 8
	db $A3
	Notes F_, 8
	db $A7
	Notes F_, 8, D#, 9, B_, 8
	db $A3
	Notes C#, 8
	db $A7
	Notes F_, 9, A_, 9
	db $A3
	Notes F_, 9
	db $A7
	Notes B_, 8
	db $A4
	Notes G_, 8
	db $A2
	Rest
	EndSection

Channel1_TypeBJingle6::
	dw Section_TypeBJingle61, Section_TypeBJingle65, Section_TypeBJingle61, Section_TypeBJingle69, $0000

Channel2_TypeBJingle6::
	dw Section_TypeBJingle62, Section_TypeBJingle66, Section_TypeBJingle62, Section_TypeBJingle610

Channel3_TypeBJingle6::
	dw Section_TypeBJingle63, Section_TypeBJingle67, Section_TypeBJingle63, Section_TypeBJingle611

Channel4_TypeBJingle6::
	dw Section_TypeBJingle64, Section_TypeBJingle68, Section_TypeBJingle64, Section_TypeBJingle68

Section_TypeBJingle61::
	db $9D, $D1, $00, $80
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, F_, 9
	db $A2
	Notes G_, 9, G_, 9, C#, 9, A_, 8, F_, 8, C#, 9
	db $A2
	Notes A_, 8
	db $A1
	Notes A_, 8, G_, 8
	db $A2
	Notes A_, 8, A_, 8, D#, 8, B_, 7, G_, 7
	db $A1
	Notes D#, 8, A_, 8
	EndSection

Section_TypeBJingle62::
	db $9D, $B2, $00, $80
	db $A2
	Notes A_, 8
	db $A1
	Notes A_, 8, A_, 8
	db $A2
	Notes A_, 8
	db $A1
	Notes A_, 8, A_, 8
	db $A2
	Notes G_, 7
	db $A1
	Notes G_, 7, G_, 7
	db $A2
	Notes G_, 7
	Rest
	Notes D#, 8
	db $A1
	Notes D#, 8, D#, 8
	db $A2
	Notes D#, 8
	db $A1
	Notes D#, 8, D#, 8
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes A_, 6
	Rest
	EndSection

Section_TypeBJingle63::
	db $9D, LOW(DefaultWavePattern), HIGH(DefaultWavePattern), $20
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, G_, 9
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, G_, 9
	db $A2
	Notes F_, 8
	db $A1
	Notes A_, 8, A_, 8
	db $A2
	Notes C#, 9
	Rest
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, G_, 9
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, G_, 9
	db $A2
	Notes G_, 7
	db $A1
	Notes B_, 7, B_, 7
	db $A2
	Notes D#, 8
	Rest
	EndSection

Section_TypeBJingle64::
	db $A2
	Noise 1
	db $A7
	Rest
	db $A2
	Noise 2, 2, 2
	Rest
	db $A2
	Noise 1
	db $A7
	Rest
	db $A2
	Noise 2, 2, 2
	Rest
	EndSection

Section_TypeBJingle65::
	db $A2
	Notes B_, 7
	db $A1
	Notes B_, 7, A_, 8
	db $A2
	Notes G_, 7
	db $A1
	Notes G_, 7, A_, 8
	db $A2
	Notes F_, 7
	db $A1
	Notes F_, 7, A_, 8
	db $A2
	Notes B_, 7
	db $A1
	Notes B_, 7, A_, 8
	db $A2
	Notes D#, 8
	db $A1
	Notes D#, 8, A_, 8
	db $A2
	Notes G_, 7
	db $A1
	Notes G_, 7, A_, 8
	db $A2
	Notes B_, 7, G_, 7
	db $A1
	Notes B_, 7, A_, 8, C#, 9, F_, 9
	EndSection

Section_TypeBJingle66::
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes F_, 6
	db $A1
	Notes F_, 6, F_, 6
	db $A2
	Notes F_, 6
	Rest
	EndSection

Section_TypeBJingle67::
	Notes B_, 7
	db $A1
	Notes B_, 7, B_, 7
	db $A2
	Notes B_, 7
	db $A1
	Notes B_, 7, B_, 7
	db $A2
	Notes B_, 7
	db $A1
	Notes B_, 7, B_, 7
	db $A2
	Notes B_, 7
	db $A1
	Notes B_, 7, B_, 7
	db $A2
	Notes G_, 7
	db $A1
	Notes G_, 7, G_, 7
	db $A2
	Notes G_, 7
	db $A1
	Notes G_, 7, G_, 7
	db $A2
	Notes F_, 7
	db $A1
	Notes F_, 7, F_, 7
	db $A2
	Notes F_, 7
	Rest
	EndSection

Section_TypeBJingle68::
	db $A2
	Rest
	Noise 2
	Rest
	Noise 2
	Rest
	Noise 2
	Rest
	Noise 2
	Rest
	Noise 2
	Rest
	Noise 2
	Rest
	Noise 2, 2
	Rest
	EndSection

Section_TypeBJingle69::
	db $A2
	Notes B_, 7
	db $A1
	Notes B_, 7, A_, 8
	db $A2
	Notes G_, 7
	db $A1
	Notes G_, 7, A_, 8
	db $A2
	Notes F_, 7
	db $A1
	Notes F_, 7, A_, 8
	db $A2
	Notes B_, 7
	db $A1
	Notes B_, 7, A_, 8
	db $A2
	Notes D#, 8
	db $A1
	Notes D#, 8, A_, 8
	db $A2
	Notes B_, 7
	db $A1
	Notes B_, 7, A_, 8
	db $A2
	Notes G_, 7, A_, 8
	db $A3
	Notes G_, 9
	EndSection

Section_TypeBJingle610::
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Notes A_, 6
	db $A1
	Notes A_, 6, A_, 6
	db $A2
	Rest
	Notes A_, 6
	db $A3
	Notes D#, 8
	EndSection

Section_TypeBJingle611::
	Notes B_, 7
	db $A1
	Notes B_, 7, B_, 7
	db $A2
	Notes B_, 7
	db $A1
	Notes B_, 7, B_, 7
	db $A2
	Notes B_, 7
	db $A1
	Notes B_, 7, B_, 7
	db $A2
	Notes B_, 7
	db $A1
	Notes B_, 7, B_, 7
	db $A2
	Notes G_, 7
	db $A1
	Notes G_, 7, G_, 7
	db $A2
	Notes G_, 7
	db $A1
	Notes G_, 7, G_, 7
	db $A2
	Rest
	Notes D#, 8
	db $A3
	Notes G_, 7
	EndSection

Channel2_TypeBJingle1::
	dw Section_TypeBJingle11, $0000

Section_TypeBJingle11::
	db $9D, $C2, $00, $40
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, F_, 9
	db $A2
	Notes G_, 9, G_, 9, C#, 9, A_, 8, F_, 8, C#, 9
	db $A2
	Notes A_, 8
	db $A1
	Notes A_, 8, G_, 8
	db $A2
	Notes A_, 8, A_, 8, D#, 8, B_, 7
	db $A1
	Notes G_, 7, F_, 7
	db $A2
	Notes G_, 7
	db $A4
	Rest
	EndSection

Channel2_TypeBJingle2::
	dw Section_TypeBJingle21, $0000

Channel3_TypeBJingle2::
	dw Section_TypeBJingle22

Section_TypeBJingle21::
	db $9D, $C2, $00, $80
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, F_, 9
	db $A2
	Notes G_, 9, G_, 9, C#, 9, A_, 8, F_, 8, C#, 9
	db $A2
	Notes A_, 8
	db $A1
	Notes A_, 8, G_, 8
	db $A2
	Notes A_, 8, D#, 8, G_, 7, A_, 8
	db $A3
	Notes G_, 9
	db $A4
	Rest
	EndSection

Section_TypeBJingle22::
	db $9D, LOW(DefaultWavePattern), HIGH(DefaultWavePattern), $20
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, G_, 9
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, G_, 9
	db $A2
	Notes F_, 8, A_, 8, C#, 9
	Rest
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, G_, 9
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, G_, 9
	db $A2
	Notes A_, 8, D#, 8, G_, 7
	Rest
	db $A5
	Rest

Channel2_TypeBJingle3::
	dw Section_TypeBJingle31, $0000

Channel1_TypeBJingle3::
	dw Section_TypeBJingle32

Channel3_TypeBJingle3::
	dw Section_TypeBJingle33

Section_TypeBJingle31::
	db $9D, $C2, $00, $80
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, F_, 9
	db $A2
	Notes G_, 9, G_, 9, C#, 9, A_, 8, F_, 8, C#, 9
	db $A2
	Notes A_, 8
	db $A1
	Notes A_, 8, G_, 8
	db $A2
	Notes A_, 8, D#, 8, G_, 7, A_, 8
	db $A3
	Notes G_, 9
	db $A4
	Rest
	EndSection

Section_TypeBJingle32::
	db $9D, $C2, $00, $40
	db $A2
	Notes F_, 8
	db $A1
	Notes F_, 8, A_, 8
	db $A2
	Notes C#, 9, F_, 8
	db $A3
	Notes B_, 7, B_, 7
	db $A2
	Notes D#, 8
	db $A1
	Notes D#, 8, C#, 8
	db $A2
	Notes D#, 8, G_, 7, D#, 6, D#, 8
	db $A3
	Notes D#, 8
	db $A5
	Rest
	EndSection

Section_TypeBJingle33::
	db $9D, LOW(DefaultWavePattern), HIGH(DefaultWavePattern), $20
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, G_, 9
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, G_, 9
	db $A2
	Notes F_, 8, A_, 8
	db $A1
	Notes C#, 9, C#, 9
	db $A2
	Notes C#, 9
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, G_, 9
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, G_, 9
	db $A2
	Notes A_, 8, D#, 8
	db $A1
	Notes G_, 7, G_, 7
	db $A2
	Rest
	db $A5
	Rest
	EndSection

Channel1_TypeBJingle4::
	dw Section_TypeBJingle41, $0000

Channel2_TypeBJingle4::
	dw Section_TypeBJingle42

Channel3_TypeBJingle4::
	dw Section_TypeBJingle43

Channel4_TypeBJingle4::
	dw Section_TypeBJingle44

Section_TypeBJingle41::
	db $9D, $C2, $00, $80
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, F_, 9
	db $A2
	Notes G_, 9, G_, 9, C#, 9, A_, 8, F_, 8, C#, 9
	db $A2
	Notes A_, 8
	db $A1
	Notes A_, 8, G_, 8
	db $A2
	Notes A_, 8, D#, 8, G_, 7, A_, 8
	db $A3
	Notes G_, 9
	db $A4
	Rest
	EndSection

Section_TypeBJingle42::
	db $9D, $B2, $00, $80
	db $A2
	Notes F_, 8
	db $A1
	Notes F_, 8, A_, 8
	db $A2
	Notes C#, 9, F_, 8
	db $A3
	Notes B_, 7, B_, 7
	db $A2
	Notes D#, 8
	db $A1
	Notes D#, 8, C#, 8
	db $A2
	Notes D#, 8, G_, 7, D#, 6, D#, 8
	db $A3
	Notes D#, 8
	db $A5
	Rest

Section_TypeBJingle43::
	db $9D, LOW(DefaultWavePattern), HIGH(DefaultWavePattern), $20
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, G_, 9
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, G_, 9, F_, 8, C#, 9, G_, 9, C#, 9, F_, 8, G_, 7, C#, 7, G_, 7
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, G_, 9
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, G_, 9, A_, 8, D#, 8, G_, 7, D#, 8, G_, 9
	Rest
	db $A2
	Rest
	db $A5
	Rest

Section_TypeBJingle44::
	db $A2
	Noise 2, 2, 2, 2
	db $A2
	Noise 2, 2, 2
	Rest
	db $A2
	Noise 2, 2, 2, 2
	db $A2
	Noise 2, 2, 2
	Rest
	db $A5
	Rest

Channel1_TypeBJingle5::
	dw Section_TypeBJingle51, Section_TypeBJingle55, $0000

Channel2_TypeBJingle5::
	dw Section_TypeBJingle52, Section_TypeBJingle56

Channel3_TypeBJingle5::
	dw Section_TypeBJingle53, Section_TypeBJingle57

Channel4_TypeBJingle5::
	dw Section_TypeBJingle54, Section_TypeBJingle58

Section_TypeBJingle51::
	db $9D, $D1, $00, $80
	db $A2
	Notes G_, 9
	db $A1
	Notes G_, 9, F_, 9
	db $A2
	Notes G_, 9, G_, 9, C#, 9, A_, 8, F_, 8, C#, 9
	db $A2
	Notes A_, 8
	db $A1
	Notes A_, 8, G_, 8
	db $A2
	Notes A_, 8, A_, 8, D#, 8, B_, 7, G_, 7
	db $A1
	Notes D#, 8, A_, 8
	EndSection

Section_TypeBJingle52::
	db $A2
	Notes A_, 8
	db $A7
	Rest
	db $A2
	Notes G_, 7, G_, 7, G_, 7
	Rest
	Notes D#, 8
	db $A7
	Rest
	db $A2
	Notes A_, 6, A_, 6, A_, 6
	Rest
	EndSection

Section_TypeBJingle53::
	db $A2
	Notes G_, 9
	db $A7
	Rest
	db $A2
	Notes F_, 8, A_, 8, C#, 9
	Rest
	db $A2
	Notes G_, 9
	db $A7
	Rest
	db $A2
	Notes G_, 7, B_, 7, D#, 8
	Rest
	EndSection

Section_TypeBJingle54::
	db $A2
	Noise 1
	db $A7
	Rest
	db $A2
	Noise 2, 2, 2
	Rest
	db $A2
	Noise 1
	db $A7
	Rest
	db $A2
	Noise 2, 2, 2
	Rest
	EndSection

Section_TypeBJingle55::
	db $A2
	Notes B_, 7
	db $A1
	Notes B_, 7, A_, 8
	db $A2
	Notes G_, 7
	db $A1
	Notes G_, 7, A_, 8
	db $A2
	Notes F_, 7
	db $A1
	Notes F_, 7, A_, 8
	db $A2
	Notes B_, 7
	db $A1
	Notes B_, 7, A_, 8
	db $A2
	Notes D#, 8
	db $A1
	Notes D#, 8, A_, 8
	db $A2
	Notes B_, 7
	db $A1
	Notes B_, 7, A_, 8
	db $A2
	Notes G_, 9, A_, 8
	db $A3
	Notes G_, 9
	EndSection

Section_TypeBJingle56::
	Rest
	Notes A_, 6
	Rest
	Notes A_, 6
	Rest
	Notes A_, 6
	Rest
	Notes A_, 6
	Rest
	Notes A_, 6
	Rest
	Notes A_, 6
	Rest
	Notes A_, 6
	db $A3
	Notes D#, 6
Section_TypeBJingle57::
	Rest
	Notes B_, 7
	Rest
	Notes B_, 7
	Rest
	Notes B_, 7
	Rest
	Notes B_, 7
	Rest
	Notes G_, 7
	Rest
	Notes G_, 7
	Rest
	Notes D#, 8
	db $A3
	Notes G_, 7
Section_TypeBJingle58::
	db $A2
	Rest
	Noise 2
	Rest
	Noise 2
	Rest
	Noise 2
	Rest
	Noise 2
	Rest
	Noise 2
	Rest
	Noise 2
	db $A2
	Rest
	Noise 2, 2
	Rest

Channel2_MultiplayerRoundOver::
	dw Section_MultiplayerRoundOver1, $0000

Channel1_MultiplayerRoundOver::
	dw Section_MultiplayerRoundOver2

Channel3_MultiplayerRoundOver::
	dw Section_MultiplayerRoundOver3

Channel4_MultiplayerRoundOver::
	dw Section_MultiplayerRoundOver4

Section_MultiplayerRoundOver1::
	db $9D, $B3, $00, $80
	db $A6
	Notes A_, 8
	db $A1
	Notes G_, 8
	db $A6
	Notes A_, 8
	db $A1
	Notes G_, 8
	db $A6
	Notes A_, 8
	db $A1
	Notes B_, 7
	db $A3
	Rest
	db $A6
	Notes D#, 8
	db $A1
	Notes C#, 8
	db $A6
	Notes D#, 8
	db $A1
	Notes C#, 8
	db $A6
	Notes D#, 8
	db $A1
	Notes F_, 7
	db $A3
	Rest
	db $A6
	Notes C#, 7
	db $A1
	Notes F_, 7
	db $A6
	Notes G_, 7
	db $A1
	Notes B_, 7
	db $A6
	Notes D#, 8
	db $A1
	Notes G_, 8
	db $A6
	Notes A_, 8
	db $A1
	Notes C#, 9
	db $A6
	Notes A_, 8
	db $A1
	Notes A_, 10
	EndSection

Section_MultiplayerRoundOver2::
	db $9D, $93, $00, $C0
	db $A6
	Notes F_, 7
	db $A1
	Notes D#, 7
	db $A6
	Notes F_, 7
	db $A1
	Notes D#, 7
	db $A6
	Notes F_, 7
	db $A1
	Notes F_, 7
	db $A3
	Rest
	db $A6
	Notes A_, 6
	db $A1
	Notes G_, 6
	db $A6
	Notes A_, 6
	db $A1
	Notes G_, 6
	db $A6
	Notes A_, 6
	db $A1
	Notes A_, 6
	db $A3
	Rest
	db $A6
	Notes G_, 6
	db $A1
	Notes G_, 6
	db $A6
	Notes A_, 6
	db $A1
	Notes C#, 7
	db $A6
	Notes F_, 7
	db $A1
	Notes G_, 7
	db $A6
	Notes B_, 7
	db $A1
	Notes D#, 8
	db $A6
	Notes F_, 7
	db $A1
	Notes F_, 7
Section_MultiplayerRoundOver3::
	db $9D, LOW(DefaultWavePattern), HIGH(DefaultWavePattern), $A0
	db $A6
	Notes B_, 7
	db $A1
	Notes A_, 7
	db $A6
	Notes B_, 7
	db $A1
	Notes A_, 7
	db $A6
	Notes B_, 7
	db $A1
	Notes A_, 8
	db $A3
	Rest
	db $A6
	Notes G_, 7
	db $A1
	Notes F_, 7
	db $A6
	Notes G_, 7
	db $A1
	Notes F_, 7
	db $A6
	Notes G_, 7
	db $A1
	Notes D#, 8
	db $A3
	Rest
	db $A6
	Notes B_, 7
	db $A1
	Notes A_, 6
	db $A6
	Notes C#, 7
	db $A1
	Notes F_, 7
	db $A6
	Notes G_, 7
	db $A1
	Notes B_, 7
	db $A6
	Notes D#, 8
	db $A1
	Notes G_, 8
	db $A6
	Notes A_, 8
	db $A1
	Notes A_, 6
Section_MultiplayerRoundOver4::
	db $A6
	Noise 2
	db $A1
	Noise 1
	db $A6
	Noise 2
	db $A1
	Noise 1
	db $A6
	Noise 2
	db $A1
	Noise 1
	db $A3
	Rest
	db $A6
	Noise 2
	db $A1
	Noise 1
	db $A6
	Noise 2
	db $A1
	Noise 1
	db $A6
	Noise 2
	db $A1
	Noise 1
	db $A3
	Rest
	db $A6
	Noise 2
	db $A1
	Noise 1
	db $A6
	Noise 2
	db $A1
	Noise 1
	db $A6
	Noise 2
	db $A1
	Noise 1
	db $A3
	Rest
	db $A6
	Noise 2
	db $A1
	Noise 1

; Channel 1 plays the melody of channel 2 at a lower volume after a pause (echo effect)
Channel1_TopScore::
	dw Section_TopScore2, $FFFF, Channel2_TopScore.main

Channel2_TopScore::
	dw Section_TopScore1
.main
	dw Section_TopScore3
.loop
	dw Section_TopScore7, Section_TopScore9, Section_TopScore7, Section_TopScore11, Section_TopScore13, $FFFF, Channel2_TopScore.loop

Channel3_TopScore::
	dw Section_TopScore4
.loop
	dw Section_TopScore8, Section_TopScore10, Section_TopScore8, Section_TopScore12, Section_TopScore14, $FFFF, Channel3_TopScore.loop

Channel4_TopScore::
	dw Section_TopScore5
.loop
	dw Section_TopScore6, $FFFF, Channel4_TopScore.loop

Section_TopScore1::
	db $9D, $60, $00, $81
	EndSection

Section_TopScore2::
	db $9D, $20, $00, $81
	db $AA
	Rest
	EndSection

Section_TopScore3::
	db $A3
	Rest
	Notes G_, 8, B_, 8, D#, 9
	EndSection

Section_TopScore4::
	db $A5
	Rest
	EndSection

Section_TopScore5::
	db $A5
	Rest
	EndSection

Section_TopScore6::
	db $A3
	Rest
	Noise 1
	Rest
	Noise 1
	Rest
	db $A2
	Noise 1, 1
	db $A3
	Rest
	Noise 1
	db $A3
	Rest
	Noise 1
	Rest
	Noise 1
	Rest
	db $A2
	Noise 1, 1
	Rest
	Rest
	Noise 1, 1
	EndSection

Section_TopScore7::
	db $A7
	Notes F_, 9
	db $A2
	Notes A_, 9
	db $A7
	Notes F_, 9
	db $A2
	Notes D#, 9
	db $A7
	Notes D#, 9
	db $A2
	Notes B_, 8
	db $A7
	Notes D#, 9
	db $A2
	Notes B_, 8
	EndSection

Section_TopScore8::
	db $9D, LOW(KorobeinikiWavePattern), HIGH(KorobeinikiWavePattern), $20
	db $A2
	Notes F_, 9, C#, 10, G_, 10, D#, 11, F_, 9, C#, 10, G_, 10, D#, 11, F_, 9, D#, 10, F_, 10, B_, 10, F_, 9, D#, 10, F_, 10, B_, 10
	EndSection

Section_TopScore9::
	db $A7
	Notes B_, 8
	db $A2
	Notes G_, 8
	db $A7
	Notes B_, 8
	db $A2
	Notes G_, 8
	db $A7
	Notes G_, 8
	db $A2
	Notes D#, 8
	db $A7
	Notes C#, 8
	db $A2
	Notes G_, 8
	EndSection

Section_TopScore10::
	Notes D#, 9, A_, 9, D#, 10, B_, 10, D#, 9, A_, 9, D#, 10, B_, 10, G_, 8, B_, 8, D#, 9, A_, 9, G_, 8, D#, 9, A_, 9, D#, 10
	EndSection

Section_TopScore11::
	db $A7
	Notes B_, 8
	db $A2
	Notes G_, 8
	db $A7
	Notes B_, 8
	db $A2
	Notes G_, 8
	db $A7
	Notes G_, 8
	db $A2
	Notes D#, 8
	db $A7
	Notes C#, 8
	db $A2
	Notes A_, 7
	EndSection

Section_TopScore12::
	Notes D#, 9, A_, 9, D#, 10, B_, 10, D#, 9, A_, 9, D#, 10, B_, 10, G_, 8, B_, 8, D#, 9, A_, 9, G_, 8, D#, 9, A_, 9, D#, 10
	EndSection

Section_TopScore13::
	db $A7
	Notes C#, 8
	db $A2
	Notes D#, 8
	db $A7
	Notes C#, 8
	db $A2
	Notes A_, 7
	db $A7
	Notes A_, 7
	db $A2
	Notes G_, 7
	db $A7
	Notes A_, 7
	db $A2
	Notes C#, 8
	db $A7
	Notes D#, 8
	db $A2
	Notes G_, 8
	db $A7
	Notes D#, 8
	db $A2
	Notes C#, 8
	db $A7
	Notes C#, 8
	db $A2
	Notes A_, 7
	db $A7
	Notes C#, 8
	db $A2
	Notes D#, 8
	db $A7
	Notes G_, 8
	db $A2
	Notes F_, 8
	db $A7
	Notes G_, 8
	db $A2
	Notes A_, 8
	db $A7
	Notes D#, 9
	db $A2
	Notes B_, 8
	db $A7
	Notes F_, 9
	db $A2
	Notes B_, 8
	db $A7
	Notes A_, 8
	db $A2
	Notes G_, 8
	db $A7
	Notes D#, 8
	db $A2
	Notes C#, 8
	db $A2
	Notes F_, 7, G_, 6, B_, 6, C#, 8
	db $A3
	Notes F_, 7
	Rest
	EndSection

Section_TopScore14::
	Notes C#, 8, A_, 8, D#, 9, A_, 9, C#, 8, D#, 9, A_, 9, C#, 10, B_, 8, C#, 10, G_, 10, B_, 10, B_, 8, C#, 10, G_, 10, B_, 10, A_, 7, D#, 8, B_, 8, A_, 9, A_, 7, D#, 8, B_, 8, F_, 9, G_, 8, D#, 9, A_, 9, D#, 10, G_, 8, A_, 9, D#, 10, B_, 10, C#, 8, G_, 8, D#, 9, A_, 9, C#, 8, D#, 9, A_, 9, C#, 10, F_, 8, B_, 8, F_, 9, C#, 10, F_, 8, B_, 8, F_, 9, F_, 10, G_, 8, D#, 9, A_, 9, D#, 10, G_, 8, A_, 9, D#, 10, G_, 10
	db $A8
	Notes F_, 9
	db $A3
	Rest
	EndSection

Channel2_StageClear::
	dw Section_StageClear1, $0000

Channel1_StageClear::
	dw Section_StageClear2

Channel3_StageClear::
	dw Section_StageClear3

Channel4_StageClear::
	dw Section_StageClear4

Section_StageClear1::
	db $9D, $B1, $00, $80
	db $A7
	Rest
	db $A1
	Notes A_, 9, A_, 9
	db $A6
	Notes G_, 10
	db $A1
	Notes A_, 9
	db $A4
	Notes G_, 10
	EndSection

Section_StageClear2::
	db $9D, $91, $00, $80
	db $A7
	Rest
	db $A1
	Notes B_, 8, B_, 8
	db $A6
	Notes A_, 9
	db $A1
	Notes D#, 9
	db $A4
	Notes A_, 9
Section_StageClear3::
	db $9D, LOW(DefaultWavePattern), HIGH(DefaultWavePattern), $20
	db $A7
	Rest
	db $A1
	Notes F_, 8, F_, 8
	db $A6
	Notes D#, 9
	db $A1
	Notes G_, 8
	db $A3
	Notes D#, 9
	Rest

Section_StageClear4::
	db $A7
	Rest
	db $A1
	Noise 1, 1
	db $A6
	Noise 2
	db $A1
	Noise 1
	db $A0
	Noise 1, 1, 1, 1, 1, 1, 1, 1
	db $A3
	Rest

Channel2_Danger::
	dw Section_Danger1, Section_Danger5, Section_Danger1, Section_Danger8, $FFFF, Channel2_Danger

Channel1_Danger::
	dw Section_Danger2, Section_Danger6, Section_Danger2, Section_Danger9, $FFFF, Channel1_Danger

Channel3_Danger::
	dw Section_Danger3, Section_Danger7, Section_Danger3, Section_Danger10, $FFFF, Channel3_Danger

Channel4_Danger::
	dw Section_Danger4, $FFFF, Channel4_Danger

Section_Danger1::
	db $9D, $82, $00, $80
	db $A2
	Notes B_, 8
	db $A1
	Notes B_, 8, B_, 8, B_, 8, C#, 8, A_, 7, C#, 8
	db $A2
	Notes B_, 8
	db $A1
	Notes B_, 8, B_, 8, B_, 8, D#, 9, G_, 9, D#, 9
	db $A2
	Notes B_, 8
	db $A1
	Notes B_, 8, B_, 8, D#, 9, B_, 8, A_, 8, B_, 8
	db $A1
	Notes D#, 9, G_, 9, D#, 9, G_, 9
	db $A2
	Notes D#, 9
	db $A1
	Notes C#, 9, D#, 9
	EndSection

Section_Danger2::
	db $9D, $62, $00, $80
	db $A2
	Rest
	Notes G_, 7
	Rest
	Notes D#, 7
	Rest
	Notes G_, 7
	Rest
	Notes A_, 7
	Rest
	Notes G_, 7
	Rest
	Notes G_, 7
	Rest
	Notes D#, 7
	Rest
	Notes D#, 7
	EndSection

Section_Danger3::
	db $9D, LOW(DefaultWavePattern), HIGH(DefaultWavePattern), $20
	db $A2
	Notes B_, 8, B_, 8, C#, 8, A_, 8, B_, 8, B_, 8, C#, 8, D#, 9, B_, 8, B_, 8, A_, 8, B_, 8, F_, 8, B_, 8, C#, 8, A_, 8
	EndSection

Section_Danger4::
	db $A2
	Noise 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1
	db $A1
	Noise 2, 2, 1
	db $A2
	Noise 2
	db $A1
	Noise 1
	EndSection

Section_Danger5::
	db $A2
	Notes A_, 9
	db $A1
	Notes A_, 9, A_, 9, A_, 9, B_, 8, G_, 8, B_, 8
	db $A2
	Notes A_, 9
	db $A1
	Notes A_, 9, A_, 9, A_, 9, C#, 10, F_, 10, C#, 10
	db $A2
	Notes A_, 9
	db $A1
	Notes A_, 9, G_, 9
	db $A2
	Notes D#, 9
	db $A1
	Notes D#, 9, B_, 8
	db $A1
	Notes A_, 8, B_, 8, A_, 8, B_, 8
	db $A2
	Notes A_, 8
	db $A1
	Notes F_, 8, A_, 8
	EndSection

Section_Danger6::
	db $A2
	Rest
	Notes A_, 7
	Rest
	Notes C#, 8
	Rest
	Notes A_, 7
	Rest
	Notes C#, 8
	Rest
	Notes A_, 7
	Rest
	Notes A_, 7
	Rest
	Notes A_, 7
	Rest
	Notes A_, 7
	EndSection

Section_Danger7::
	db $A2
	Notes A_, 7, B_, 8, B_, 8, B_, 8, A_, 7, B_, 8, B_, 8, B_, 8, A_, 7, B_, 8, A_, 8, D#, 9, G_, 7, A_, 8, C#, 8, D#, 9
	EndSection

Section_Danger8::
	db $A2
	Notes C#, 10
	db $A1
	Notes C#, 10, C#, 10, C#, 10, A_, 9, F_, 9, A_, 9
	db $A2
	Notes C#, 10
	db $A1
	Notes C#, 10, C#, 10, C#, 10, A_, 9, F_, 9, A_, 9
	db $A2
	Notes C#, 10
	db $A1
	Notes C#, 8, F_, 8
	db $A2
	Notes A_, 8
	db $A1
	Notes C#, 8, G_, 9
	db $A3
	Notes D#, 9
	db $A1
	Notes B_, 8
	db $A6
	Notes B_, 10
	EndSection

Section_Danger9::
	db $A2
	Rest
	Notes C#, 8
	Rest
	Notes C#, 8
	Rest
	Notes C#, 8
	Rest
	Notes C#, 8
	Rest
	db $A1
	Notes A_, 7, A_, 7
	db $A2
	Notes A_, 7
	db $A1
	Notes A_, 7, A_, 7
	db $A3
	Notes A_, 7
	db $A2
	Notes G_, 7
	Rest
	EndSection

Section_Danger10::
	db $A2
	Notes F_, 7, F_, 9, G_, 8, F_, 9, F_, 7, F_, 9, G_, 8, F_, 9, C#, 8
	db $A1
	Notes A_, 8, A_, 8
	db $A2
	Notes A_, 8
	db $A1
	Notes A_, 8, A_, 8
	db $A3
	Notes A_, 8
	db $A2
	Notes B_, 8
	Rest
	EndSection

