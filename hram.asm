SECTION "HRAM", HRAM

hJoyHeld:: ; Currently pressed keys
    db

hJoyPressed:: ; Newly pressed keys
    db

ds $FFB6 - $FF82

hDMARoutine:: ds $C

ds $FFE2 - ($FFB6 + $C)

; Incremented every VBlank
hFrameCounter:: db
