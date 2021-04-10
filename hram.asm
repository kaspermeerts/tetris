SECTION "HRAM", HRAM

hJoyHeld:: ; Currently pressed keys
    db

hJoyPressed:: ; Newly pressed keys
    db

ds $FF85 - $FF82

hVBlankInterruptTriggered:
    db

ds $FF99 - $FF86

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

ds $FFB6 - $FFAB

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
