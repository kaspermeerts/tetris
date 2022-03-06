INCLUDE "music_macros.asm"

SECTION "Music", ROM0[$6F3F]

Song_6F3F::
	db $00
	dw $6F0E
	dw Channel1_7CF9
	dw Channel2_7CFF
	dw Channel3_7D11
	dw Channel4_7D21

Song_6F4A::
	db $00
	dw $6F05
	dw Channel1_7E48
	dw Channel2_7E44
	dw Channel3_7E4A
	dw Channel4_7E4C

Song_6F55::
	db $00
	dw $6F0E
	dw Channel1_763B
	dw Channel2_7633
	dw Channel3_7641
	dw Channel4_7663

Song_6F60::
	db $00
	dw $6EF9
	dw Channel1_7600
	dw Channel2_75FC
	dw Channel3_7602
	dw $0000

Song_6F6B::
	db $00
	dw $6F0E
	dw Channel1_714C
	dw Channel2_7142
	dw Channel3_7156
	dw Channel4_7162

Song_6F76::
	db $00
	dw $6F0E
	dw Channel1_72C6
	dw Channel2_72B8
	dw Channel3_72D4
	dw Channel4_7302

Song_6F81::
	db $00
	dw $6F0E
	dw Channel1_7008
	dw Channel2_6FFA
	dw $0000
	dw $0000

Song_6F8C::
	db $00
	dw $6F05
	dw Channel1_7E9D
	dw Channel2_7E91
	dw Channel3_7EA9
	dw Channel4_7EB5

Song_6F97::
	db $00
	dw $6F0E
	dw Channel1_7C28
	dw Channel2_7C24
	dw Channel3_7C2A
	dw Channel4_7C2C

Song_6FA2::
	db $00
	dw $6F0E
	dw $0000
	dw Channel2_7A00
	dw $0000
	dw $0000

Song_6FAD::
	db $00
	dw $6F0E
	dw $0000
	dw Channel2_7A26
	dw Channel3_7A2A
	dw $0000

Song_6FB8::
	db $00
	dw $6F0E
	dw Channel1_7A73
	dw Channel2_7A6F
	dw Channel3_7A75
	dw $0000

Song_6FC3::
	db $00
	dw $6F0E
	dw Channel1_7ADF
	dw Channel2_7AE3
	dw Channel3_7AE5
	dw Channel4_7AE7

Song_6FCE::
	db $00
	dw $6F0E
	dw Channel1_7B65
	dw Channel2_7B6B
	dw Channel3_7B6F
	dw Channel4_7B73

Song_6FD9::
	db $00
	dw $6F0E
	dw Channel1_786C
	dw Channel2_7876
	dw Channel3_787E
	dw Channel4_7886

Song_6FE4::
	db $00
	dw $6F2B
	dw Channel1_7543
	dw Channel2_754B
	dw Channel3_7551
	dw $0000

Song_6FEF::
	db $00
	dw $6F0E
	dw Channel1_758D
	dw Channel2_7595
	dw Channel3_759B
	dw $0000

Channel2_6FFA::
	dw Section_7016, Section_7034, Section_7016, Section_704D, Section_7093, $FFFF, $6FFA

Channel1_7008::
	dw Section_7062, Section_7074, Section_7062, Section_7085, Section_70F4, $FFFF, $7008

Section_7016::
	db $9D, $74, $00, $41
	db $A2
	Notes G_, 7, D#, 8, C#, 9, D#, 8, F_, 7, D#, 8, G_, 7, D#, 8, C#, 7, D#, 8, B_, 6, D#, 8, G_, 7, D#, 8, C#, 9, D#, 8, F_, 7, D#, 8, G_, 7, D#, 8, C#, 7, D#, 8, B_, 6, D#, 8
	EndSection

Section_7034::
	Notes G_, 7, D#, 8, G_, 7, C#, 7, F_, 8, B_, 7, F_, 7, B_, 7, F_, 7, A_, 6, D#, 8, G_, 7, C#, 7, D#, 8, B_, 7, G_, 7, F_, 7, C#, 7, B_, 6, D#, 6, B_, 6, F_, 7, D#, 8, B_, 7
	EndSection

Section_704D::
	Notes G_, 7, D#, 8, G_, 7, C#, 7, F_, 8, B_, 7, F_, 7, B_, 7, F_, 7, A_, 6, A_, 8, B_, 7, D#, 8, A_, 8, D#, 8, G_, 7, A_, 6, F_, 7
	db $A8
	Notes G_, 7
	EndSection

Section_7062::
	db $9D, $64, $00, $41
	db $A3
	Notes C#, 5, C#, 7, B_, 6, C#, 5, G_, 5, D#, 6, C#, 7, F_, 6, D#, 6, C#, 7, G_, 5, D#, 6
	EndSection

Section_7074::
	Notes C#, 5, C#, 7, B_, 5, A_, 4, A_, 6, G_, 5, F_, 4, F_, 6, B_, 5
	db $A2
	Notes D#, 6, F_, 6, D#, 6, B_, 5, G_, 5, F_, 5
	EndSection

Section_7085::
	db $A3
	Notes C#, 5, C#, 7, B_, 5, A_, 4, A_, 6, F_, 5, G_, 5, D#, 6, D#, 6, G_, 5, A_, 4, G_, 3
	EndSection

Section_7093::
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

Section_70F4::
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

Channel2_7142::
	dw Section_7168, Section_7168, Section_71AE, $FFFF, $7142

Channel1_714C::
	dw Section_71CB, Section_71CB, Section_721D, $FFFF, $714C

Channel3_7156::
	dw Section_723A, Section_723A, Section_727F, Section_727F, $FFFF, $7156

Channel4_7162::
	dw Section_72A3, $FFFF, $7162

Section_7168::
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

Section_71AE::
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

Section_71CB::
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

Section_721D::
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

Section_723A::
	db $9D, $C9, $6E, $20
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

Section_727F::
	db $9D, $C9, $6E, $20
	db $A2
	Notes G_, 7, A_, 8, G_, 7, A_, 8, G_, 7, A_, 8, G_, 7, A_, 8, F_, 7, A_, 8, F_, 7, A_, 8, F_, 7, A_, 8, F_, 7, A_, 8, G_, 7, A_, 8, G_, 7, A_, 8, G_, 7, A_, 8, G_, 7, A_, 8, F_, 7, A_, 8, F_, 7, A_, 8
	db $A4
	Rest
	EndSection

Section_72A3::
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

Channel2_72B8::
	dw Section_730B, Section_733F, Section_7367, Section_7367, Section_73C9, $FFFF, $72B8

Channel1_72C6::
	dw Section_7308, Section_733C, Section_738E, Section_738E, Section_744B, $FFFF, $72C6

Channel3_72D4::
	dw Section_731F, Section_7353, Section_73B5, Section_73B5, Section_73B5, Section_73B5, Section_73B5, Section_73B5, Section_74C0, Section_74DE, Section_74DE, Section_74DE, Section_74EE, Section_74FE, Section_74FE, Section_750E, Section_750E, Section_751E, Section_751E, Section_750E, Section_752E, $FFFF, $72D4

Channel4_7302::
	dw Section_7333, $FFFF, $7302

Section_7308::
	db $A5
	Rest
	EndSection

Section_730B::
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

Section_731F::
	db $9D, $E9, $6E, $A0
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

Section_7333::
	db $A2
	Noise 1
	db $A1
	Noise 1, 1
	db $A2
	Noise 1, 1
	EndSection

Section_733C::
	db $A5
	Rest
	EndSection

Section_733F::
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

Section_7353::
	db $9D, $E9, $6E, $A0
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

Section_7367::
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

Section_738E::
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

Section_73B5::
	db $9D, $E9, $6E, $A0
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

Section_73C9::
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

Section_744B::
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

Section_74C0::
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

Section_74DE::
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

Section_74EE::
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

Section_74FE::
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

Section_750E::
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

Section_751E::
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

Section_752E::
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
Channel1_7543::
	dw Section_7557, Section_7562, $FFFF, $7545

Channel2_754B::
	dw Section_755E, $FFFF, $754B

Channel3_7551::
	dw Section_757C, $FFFF, $7551

Section_7557::
	db $9D, $20, $00, $81
	db $AA
	Rest
	EndSection

Section_755E::
	db $9D, $70, $00, $81

Section_7562::
	db $A2
	Notes F_, 7, C#, 6, G_, 6, F_, 7, A_, 7, D#, 6, B_, 6, A_, 7, C#, 8, G_, 6, F_, 7, C#, 8, D#, 8, B_, 6, F_, 7, D#, 8, A_, 7, D#, 6, B_, 6, A_, 7, D#, 7, A_, 5, D#, 6, D#, 7
	EndSection

Section_757C::
	db $9D, $E9, $6E, $21
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

Channel1_758D::
	dw Section_75A1, Section_75AC, $FFFF, $758F

Channel2_7595::
	dw Section_75A8, $FFFF, $7595

Channel3_759B::
	dw Section_75EE, $FFFF, $759B

Section_75A1::
	db $9D, $20, $00, $81
	db $AA
	Rest
	EndSection

Section_75A8::
	db $9D, $70, $00, $81

Section_75AC::
	db $A2
	Notes D#, 8, F_, 7, G_, 8, F_, 7, B_, 8, F_, 7, G_, 8, F_, 7, C#, 9, F_, 7, B_, 8, F_, 7, G_, 8, F_, 7, B_, 8, F_, 7, D#, 8, F_, 7, G_, 8, F_, 7, B_, 8, F_, 7, G_, 8, F_, 7, C#, 9, F_, 7, B_, 8, F_, 7, G_, 8, F_, 7, B_, 8, F_, 7, F_, 9, A_, 7, C#, 9, A_, 7, B_, 8, A_, 7, G_, 8, A_, 7, F_, 8, A_, 7, G_, 8, A_, 7, B_, 8, A_, 7, G_, 8, A_, 7, G_, 8, C#, 7, D#, 8, C#, 7, D#, 8, C#, 7, C#, 8, C#, 7, C#, 8, C#, 7, A_, 7, C#, 7, C#, 8, C#, 7, G_, 8, C#, 7
	EndSection

Section_75EE::
	db $9D, $E9, $6E, $21
	db $A5
	Notes D#, 8, C#, 8, A_, 7, F_, 7, G_, 6, C#, 7, F_, 7, F_, 7
	EndSection

Channel2_75FC::
	dw Section_7604, $0000

Channel1_7600::
	dw Section_7614

Channel3_7602::
	dw Section_7623

Section_7604::
	db $9D, $B2, $00, $80
	db $A2
	Notes B_, 9, G_, 9, B_, 9, G_, 9, B_, 9, C#, 10, B_, 9, G_, 9
	db $A4
	Notes B_, 9
	EndSection

Section_7614::
	db $9D, $92, $00, $80
	db $A2
	Notes A_, 8, F_, 8, A_, 8, F_, 8, A_, 8, B_, 8, A_, 8, F_, 8
	db $A4
	Notes A_, 8
Section_7623::
	db $9D, $E9, $6E, $20
	db $A2
	Notes C#, 10, B_, 9, C#, 10, B_, 9, C#, 10, F_, 10, C#, 10, B_, 9
	db $A3
	Notes C#, 10
	Rest

Channel2_7633::
	dw Section_766F, Section_7769, Section_7769, $0000

Channel1_763B::
	dw Section_76BF, Section_77AA, Section_783C

Channel3_7641::
	dw Section_770C, Section_77EB, Section_77EB, Section_77F5, Section_77EB, Section_77EB, Section_77FE, Section_77F5, Section_77EB, Section_77EB, Section_77FE, Section_77F5, Section_7807, Section_7811, Section_77FE, Section_77F5, Section_77EB

Channel4_7663::
	dw Section_775B, Section_775B, Section_781A, Section_781A, Section_781A, Section_781A

Section_766F::
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

Section_76BF::
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

Section_770C::
	db $9D, $E9, $6E, $20
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

Section_775B::
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

Section_7769::
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

Section_77AA::
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

Section_77EB::
	db $A2
	Notes G_, 6, G_, 6
	Rest
	Notes G_, 6, G_, 6, G_, 6
	Rest
	Notes G_, 6
	EndSection
Section_77F5::
	Notes A_, 5, A_, 5
	Rest
	Notes A_, 5, A_, 5, A_, 5
	Rest
	Notes A_, 5
	EndSection
Section_77FE::
	Notes F_, 5, F_, 5
	Rest
	Notes F_, 5, F_, 5, F_, 5
	Rest
	Notes F_, 5
	EndSection
Section_7807::
	db $A2
	Notes G_, 6, G_, 6
	Rest
	Notes G_, 6, F_, 6, F_, 6
	Rest
	Notes F_, 6
	EndSection
Section_7811::
	Notes C#, 6, C#, 6
	Rest
	Notes C#, 6, A_, 5, A_, 5
	Rest
	Notes A_, 5
	EndSection

Section_781A::
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

Section_783C::
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

Channel1_786C::
	dw Section_788E, Section_7911, Section_788E, Section_7996, $0000

Channel2_7876::
	dw Section_78AD, Section_7938, Section_78AD, Section_79BA

Channel3_787E::
	dw Section_78D5, Section_795E, Section_78D5, Section_79DD

Channel4_7886::
	dw Section_78FE, Section_7984, Section_78FE, Section_7984

Section_788E::
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

Section_78AD::
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

Section_78D5::
	db $9D, $E9, $6E, $20
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

Section_78FE::
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

Section_7911::
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

Section_7938::
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

Section_795E::
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

Section_7984::
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

Section_7996::
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

Section_79BA::
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

Section_79DD::
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

Channel2_7A00::
	dw Section_7A04, $0000

Section_7A04::
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

Channel2_7A26::
	dw Section_7A2C, $0000

Channel3_7A2A::
	dw Section_7A4B

Section_7A2C::
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

Section_7A4B::
	db $9D, $E9, $6E, $20
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

Channel2_7A6F::
	dw Section_7A77, $0000

Channel1_7A73::
	dw Section_7A96

Channel3_7A75::
	dw Section_7AB4

Section_7A77::
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

Section_7A96::
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

Section_7AB4::
	db $9D, $E9, $6E, $20
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

Channel1_7ADF::
	dw Section_7AE9, $0000

Channel2_7AE3::
	dw Section_7B08

Channel3_7AE5::
	dw Section_7B25

Channel4_7AE7::
	dw Section_7B4F

Section_7AE9::
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

Section_7B08::
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

Section_7B25::
	db $9D, $E9, $6E, $20
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

Section_7B4F::
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

Channel1_7B65::
	dw Section_7B77, Section_7BCE, $0000

Channel2_7B6B::
	dw Section_7B96, Section_7BF2

Channel3_7B6F::
	dw Section_7BA8, Section_7C02

Channel4_7B73::
	dw Section_7BBB, Section_7C12

Section_7B77::
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

Section_7B96::
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

Section_7BA8::
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

Section_7BBB::
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

Section_7BCE::
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

Section_7BF2::
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
Section_7C02::
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
Section_7C12::
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

Channel2_7C24::
	dw Section_7C2E, $0000

Channel1_7C28::
	dw Section_7C63

Channel3_7C2A::
	dw Section_7C97

Channel4_7C2C::
	dw Section_7CCB

Section_7C2E::
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

Section_7C63::
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
Section_7C97::
	db $9D, $E9, $6E, $A0
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
Section_7CCB::
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
Channel1_7CF9::
	dw Section_7D2E, $FFFF, $7D01

Channel2_7CFF::
	dw Section_7D29, Section_7D35, Section_7D5B, Section_7D82, Section_7D5B, Section_7DA4, Section_7DC6, $FFFF, $7D03

Channel3_7D11::
	dw Section_7D3B, Section_7D6C, Section_7D93, Section_7D6C, Section_7DB5, Section_7E07, $FFFF, $7D13

Channel4_7D21::
	dw Section_7D3E, Section_7D41, $FFFF, $7D23

Section_7D29::
	db $9D, $60, $00, $81
	EndSection

Section_7D2E::
	db $9D, $20, $00, $81
	db $AA
	Rest
	EndSection

Section_7D35::
	db $A3
	Rest
	Notes G_, 8, B_, 8, D#, 9
	EndSection

Section_7D3B::
	db $A5
	Rest
	EndSection

Section_7D3E::
	db $A5
	Rest
	EndSection

Section_7D41::
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

Section_7D5B::
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

Section_7D6C::
	db $9D, $C9, $6E, $20
	db $A2
	Notes F_, 9, C#, 10, G_, 10, D#, 11, F_, 9, C#, 10, G_, 10, D#, 11, F_, 9, D#, 10, F_, 10, B_, 10, F_, 9, D#, 10, F_, 10, B_, 10
	EndSection

Section_7D82::
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

Section_7D93::
	Notes D#, 9, A_, 9, D#, 10, B_, 10, D#, 9, A_, 9, D#, 10, B_, 10, G_, 8, B_, 8, D#, 9, A_, 9, G_, 8, D#, 9, A_, 9, D#, 10
	EndSection

Section_7DA4::
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

Section_7DB5::
	Notes D#, 9, A_, 9, D#, 10, B_, 10, D#, 9, A_, 9, D#, 10, B_, 10, G_, 8, B_, 8, D#, 9, A_, 9, G_, 8, D#, 9, A_, 9, D#, 10
	EndSection

Section_7DC6::
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

Section_7E07::
	Notes C#, 8, A_, 8, D#, 9, A_, 9, C#, 8, D#, 9, A_, 9, C#, 10, B_, 8, C#, 10, G_, 10, B_, 10, B_, 8, C#, 10, G_, 10, B_, 10, A_, 7, D#, 8, B_, 8, A_, 9, A_, 7, D#, 8, B_, 8, F_, 9, G_, 8, D#, 9, A_, 9, D#, 10, G_, 8, A_, 9, D#, 10, B_, 10, C#, 8, G_, 8, D#, 9, A_, 9, C#, 8, D#, 9, A_, 9, C#, 10, F_, 8, B_, 8, F_, 9, C#, 10, F_, 8, B_, 8, F_, 9, F_, 10, G_, 8, D#, 9, A_, 9, D#, 10, G_, 8, A_, 9, D#, 10, G_, 10
	db $A8
	Notes F_, 9
	db $A3
	Rest
	EndSection

Channel2_7E44::
	dw Section_7E4E, $0000

Channel1_7E48::
	dw Section_7E5E

Channel3_7E4A::
	dw Section_7E6D

Channel4_7E4C::
	dw Section_7E7D

Section_7E4E::
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

Section_7E5E::
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
Section_7E6D::
	db $9D, $E9, $6E, $20
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

Section_7E7D::
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

Channel2_7E91::
	dw Section_7EBB, Section_7F28, Section_7EBB, Section_7F73, $FFFF, $7E91

Channel1_7E9D::
	dw Section_7EE5, Section_7F4F, Section_7EE5, Section_7F96, $FFFF, $7E9D

Channel3_7EA9::
	dw Section_7EFB, Section_7F61, Section_7EFB, Section_7FAE, $FFFF, $7EA9

Channel4_7EB5::
	dw Section_7F11, $FFFF, $7EB5

Section_7EBB::
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

Section_7EE5::
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

Section_7EFB::
	db $9D, $E9, $6E, $20
	db $A2
	Notes B_, 8, B_, 8, C#, 8, A_, 8, B_, 8, B_, 8, C#, 8, D#, 9, B_, 8, B_, 8, A_, 8, B_, 8, F_, 8, B_, 8, C#, 8, A_, 8
	EndSection

Section_7F11::
	db $A2
	Noise 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1
	db $A1
	Noise 2, 2, 1
	db $A2
	Noise 2
	db $A1
	Noise 1
	EndSection

Section_7F28::
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

Section_7F4F::
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

Section_7F61::
	db $A2
	Notes A_, 7, B_, 8, B_, 8, B_, 8, A_, 7, B_, 8, B_, 8, B_, 8, A_, 7, B_, 8, A_, 8, D#, 9, G_, 7, A_, 8, C#, 8, D#, 9
	EndSection

Section_7F73::
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

Section_7F96::
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

Section_7FAE::
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

