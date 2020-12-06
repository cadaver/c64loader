;-------------------------------------------------------------------------------
; Covert Bitops Loadersystem V3.x, unpacked loading
;-------------------------------------------------------------------------------

        ; Load unpacked file with specified load address
        ;
        ; Parameters: A filenumber, X/Y load address
        ; Returns: C=0 if OK, C=1 if failed to load
        ; Modifies: A,X,Y,zpDestLo-Hi

LoadUnpackedRaw:stx zpDestLo
                sty zpDestHi
                jsr OpenFile
                lda fileOpen
                beq LoadUnpackedError
                bne LoadUnpackedCommon

        ; Load unpacked file with load address from 2 first bytes of file
        ;
        ; Parameters: A filenumber
        ; Returns: C=0 if OK, C=1 if failed to load
        ; Modifies: A,X,Y,zpDestLo-Hi

LoadUnpacked:   jsr OpenFile
                lda fileOpen
                beq LoadUnpackedError
                jsr GetByte
                sta zpDestLo
                jsr GetByte
                sta zpDestHi
LoadUnpackedCommon:
                ldy #$00
LoadUnpackedLoop:
                lda fileOpen
                beq LoadUnpackedDone
                jsr GetByte
                sta (zpDestLo),y
                iny
                bne LoadUnpackedLoop
                inc zpDestHi
                bne LoadUnpackedLoop
LoadUnpackedDone:
                clc
                rts
LoadUnpackedError:
                sec
                rts
