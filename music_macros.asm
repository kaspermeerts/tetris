Noise: MACRO
	REPT _NARG
		IF \1 == 0
			db 1
		ELIF \1 == 1
			db 6
		ELIF \1 == 2
			db 11
		ELIF \1 == 3
			db 16
		ELSE
			FAIL "Unknown noise note"
		ENDC
		SHIFT
	ENDR
ENDM

C_ EQU 1
C# EQU 2
D_ EQU 3
D# EQU 4
E_ EQU 5
F_ EQU 6
F# EQU 7
G_ EQU 8
G# EQU 9
A_ EQU 10
A# EQU 11
B_ EQU 12

Notes: MACRO
	IF _NARG % 2 != 0
		FAIL "Note list needs an even number of arguments"
	ENDC
	REPT _NARG / 2
		db \1 + (\2 - 2) * 12
		SHIFT
		SHIFT
	ENDR
ENDM

Rest: MACRO
	db $01
ENDM

EndSection: MACRO
	db $00
ENDM
