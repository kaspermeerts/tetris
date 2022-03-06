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

ds $DF70 - $D762
SECTION "Audio RAM", WRAM0[$DF70]

wMusicCurrentChannel::
	db
ds $4
wPanFrameCounter::
	db
wPanInterval::
	db
wPanCounter::
	db
wMonoOrStereo::
	db
wChannelEnableMask1::
	db
wChannelEnableMask2::
	db
ds $E - $B
wPauseTuneTimer::
	db
wPauseUnpauseSound::
	db

;DF80
ds $DFE0 - $DF80

; DFE0
wNewSquareSFXID::
	db
wCurrentSquareSFXID::
	db
wSquareSFXCounter::
	db
wSquareSFXNoteLength::
	db
wSquareSFXNoteCounter::
	db

	ds 3

; DFE8
wNewMusicID::
	db
wCurrentMusicID::
	db

ds 6

; DFF0
wNewWaveSFXID::
	db
wCurrentWaveSFXID::
	db
wWaveSFXCounter::
	db
wWaveSFXNoteLength::
	db

ds 4

; DFF8
wNewNoiseSFXID::
	db
wCurrentNoiseSFXID::
	db

