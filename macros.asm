WAIT_FOR_SERIAL_INTERRUPT: MACRO
.wait\@
    ldh a, [hSerialInterruptTriggered]
    and a
    jr z, .wait\@
ENDM
