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

ds $FF95 - $FF94

; 0 = visible
hSpriteRendererVisible::
    db

hSpriteRendererSpriteHi::
    db
hSpriteRendererSpriteLo::
    db

ds $FF99 - $FF98

hDropTimer::
    db

hFramesPerDrop::
    db

ds $FF9E - $FF9B

hLines::
    dw

ds $FFA6 - $FFA0

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

ds $FFAE - $FFAB

; The piece to come after the current preview piece. This is invisible to the
; player
hNextPreviewPiece::
    db

ds $FFB0 - $FFAF

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

hTypeBHigh::
    db

hIsMultiplayer::
    db

;hFFC6::
    db

hNewTopScore::
    db

ds $FFCC - $FFC8

hSerialInterruptTriggered::
    db

ds $FFE1 - $FFCD

hGameState:: db

; Incremented every VBlank
hFrameCounter:: db

hWipeCounter:: db

hDemoNumber::
    db

ds $FFF4 - $FFE5

hHeartMode:: ; Heart mode. Get it?
    db

ds $FFFB - $FFF5

hTopScorePointer::
    dw
