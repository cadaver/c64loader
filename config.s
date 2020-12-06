        ; Zeropage config

zpBase          = $02           ;Base for zeropage vars

fileOpen        = zpBase
loadTempReg     = zpBase+1
loadBufferPos   = zpBase+2
fastLoadEor     = zpBase+3      ;Needed for NTSC version of 2-bit protocol
zpSrcLo         = zpBase+4      ;For depackers & save
zpSrcHi         = zpBase+5
zpBitsLo        = zpBase+6
zpBitsHi        = zpBase+7
zpBitBuf        = zpBase+8
zpDestLo        = zpBase+9
zpDestHi        = zpBase+10

USETURBOMODE    = 1             ;Set to nonzero if you are not going to use SCPU/C128 fast mode
