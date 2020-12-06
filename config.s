;-------------------------------------------------------------------------------
; Covert Bitops Loadersystem V3.x, configuration
;-------------------------------------------------------------------------------

        ; Conditionals
        
USETURBOMODE    = 1             ;Can set to zero if you are not going to use
                                ;SCPU/C128 fast mode

        ; Zeropage config

zpBase          = $02           ;Base for zeropage vars

fileOpen        = zpBase
loadTempReg     = zpBase+1
loadBufferPos   = zpBase+2
fastLoadEor     = zpBase+3      ;Needed for NTSC version of 2-bit protocol
zpLenLo         = zpBase+4      ;For depackers & save
zpSrcLo         = zpBase+5
zpSrcHi         = zpBase+6
zpBitsLo        = zpBase+7
zpBitsHi        = zpBase+8
zpBitBuf        = zpBase+9
zpDestLo        = zpBase+10
zpDestHi        = zpBase+11

        ; Non-zeropage memory defines; loadBuffer can be relocated if needed, but
        ; note that all of $0200-$02ff cannot be used by your program with Kernal 
        ; loading

loadBuffer      = $0200
ELoadHelper     = $0200
StopIrq         = $02a7
