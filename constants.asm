MASTER EQU $29
SLAVE  EQU $55

SERIAL_TRANSFER_EXTERNAL_CLOCK EQU $80
SERIAL_TRANSFER_INTERNAL_CLOCK EQU $81

; The stages of locking a piece, held in hLockdownState.
LOCKDOWN_NONE          EQU 0   ; The piece is falling
LOCKDOWN_TO_BACKGROUND EQU 1
LOCKDOWN_CHECK_ROWS    EQU 2
LOCKDOWN_CLEAR_ROWS    EQU 3

; The stages of counting the score, held in wScoreTallyPhase.
SCORE_TALLY_NONE  EQU 0   ; Nothing to count
SCORE_TALLY_LINES EQU 1
SCORE_TALLY_TOTAL EQU 2
