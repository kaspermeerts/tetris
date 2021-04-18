SECTION "HRAM", HRAM

hJoyHeld:: ; Currently pressed keys
    db

hJoyPressed:: ; Newly pressed keys
    db

ds $FF85 - $FF82

hVBlankInterruptTriggered:
    db

ds $FF8D - $FF86

hSpriteRendererOAMHi::
    db
hSpriteRendererOAMLo::
    db

hSpriteRendererCount::
    db

; These are the offsets of the first tile from the 
hSpriteRendererOffsetY::
    db
hSpriteRendererOffsetX::
    db

; Although everywhere else Y comes first, here it's X?
hSpriteRendererObjX::
    db
hSpriteRendererObjY::
    db

;hFF94::
    db

; 0 = visible
hSpriteRendererVisible::
    db

hSpriteRendererSpriteHi::
    db
hSpriteRendererSpriteLo::
    db

;hFF98::
    db

hDropTimer::
    db

hFramesPerDrop::
    db

ds $FF9E - $FF9B

hLines::
    dw

;hFFA0::
    db

hSavedIE::
    db

ds $FFA6 - $FFA2

hTimer1:: ; Frame based timer
    db
hTimer2:: ; Unused?
    db

;hFFA8::
    db

hLevel::
    db

hKeyRepeatTimer::
    db

ds $FFAC - $FFAB

hMarioStartHeight::
	db

hLuigiStartHeight::
	db

; The piece to come after the current preview piece. This is invisible to the
; player
hNextPreviewPiece::
    db

;hFFAF::
    db

; Only incremented when determinism is required, i.e. demos and multiplayer
hNumPiecesPlayed::
    db

ds $FFB6 - $FFB1

hDMARoutine::
    ds $A

hGameType::
    db

hMusicType::
    db

hTypeALevel::
    db

hTypeBLevel::
    db

hTypeBStartHeight::
    db

hIsMultiplayer::
    db

;hFFC6::
    db

hNewTopScore::
    db

ds $FFCB - $FFC8

hSerialRole::
    db

hSerialInterruptTriggered::
    db

; hFFCD::
    db

; hFFCE::
    db

hSerialTx::
    db

hSerialRx::
    db

ds $FFD7 - $FFD1

hMarioWins::
	db

hLuigiWins::
	db

ds $FFE1 - $FFD9

hGameState:: db

; Incremented every VBlank
hFrameCounter:: db

hWipeCounter:: db

hDemoNumber::
    db

ds $FFE9 - $FFE5

hDemoRecording::
	db

hDemoJoypadTimer::
    db

hDemoJoypadDataHi::
    db

hDemoJoypadDataLo::
    db

hDemoJoypadHeld::
    db

hSavedJoyHeld::
	db

ds $FFF4 - $FFEF

hHeartMode:: ; Heart mode. Get it?
    db

ds $FFFB - $FFF5

hTopScorePointerHi::
    db

hTopScorePointerLo::
hTempPreviewPiece::     ; TODO Explain
	db
