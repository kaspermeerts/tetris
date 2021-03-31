SECTION "HRAM", HRAM

hJoyHeld:: ; Currently pressed keys
    db

hJoyPressed:: ; Newly pressed keys
    db

ds $FF85 - $FF82

hVBlankInterruptTriggered:
    db

ds $FF9E - $FF86

hLinesLeft::
    db

ds $FFA6 - $FF9F

hTimer1:: ; Frame based timer
    db
hTimer2:: ; Unused?
    db

hFFA8:: db

hLevel::
    db

ds $FFB6 - $FFAA

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

ds $FFCC - $FFC6

hSerialInterruptTriggered::
    db

ds $FFE1 - $FFCD

hGameState:: db

; Incremented every VBlank
hFrameCounter:: db

hFFE3:: db

hDemoNumber::
    db

ds $FFF4 - $FFE5

hHeartMode:: ; Heart mode. Get it?
    db
