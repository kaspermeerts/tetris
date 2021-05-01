SECTION "WRAM", WRAM0

wOAMBuffer:: ds $A0

wScore:: ds 3

; This list is zero-delimited, so keep at least a single empty byte at the end
wLineClearsList::
    dw
    dw
    dw
    dw
    db

wLineClearStats::
wSinglesCount::
    db

ds 4

wDoublesCount::
    db

ds 4

wTriplesCount::
    db

ds 4

wTetrisCount::
    db

ds 4

wSoftDropPoints::
	dw

wSoftDropPointsBCD::
    ds 3

wScoreboardState::
    db

ds $C0DE - $C0C6

wHidePreviewPiece::
	db

ds $C300 - $C0DF

wPieceList::
	ds $100

ds $D000 - $C400
wTypeBTopScores::
    ds 10*6*3*(6+3)     ; 10 levels, 6 starting heights, 3 entries, 6 bytes for the name and 3 for the score

wTypeATopScores::
    ds 10*3*(6+3)

ds $E000 - $D762
