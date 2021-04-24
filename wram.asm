SECTION "WRAM", WRAM0

wOAMBuffer:: ds $A0

wScore:: ds 3

ds $C0C0 - $C0A3

wSoftDropPoints::
	dw

ds $C0DE - $C0C2

wHidePreviewPiece::
	db

ds $C300 - $C0DF

wPieceList::
	ds $100
