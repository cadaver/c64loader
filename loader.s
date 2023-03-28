;-------------------------------------------------------------------------------
; Covert Bitops Loadersystem V3.x, runtime
;-------------------------------------------------------------------------------

        ; NOTE: include config.s before the loader, and loaderinit.s elsewhere
        ; (where you want the disposable init part)

        ; Constants

LOAD_KERNAL     = $00           ;loaderMode: Load using Kernal, no IRQs
LOAD_FAKEFAST   = $01           ;loaderMode: Load using Kernal, IRQs allowed
LOAD_FAST       = $ff           ;loaderMode: Load using custom serial protocol

SPR_TOP_SAFETY  = 4
SPR_BOTTOM_SAFETY = 21

        ; Low-level runtime (default versions for fastloader)

loaderCodeStart:

        ; Open file
        ;
        ; Parameters: A filenumber
        ; Returns: fileOpen 0 if failed to open
        ; Modifies: A,X,Y

OpenFile:       jmp FastOpen

                if INCLUDESAVE > 0

        ; Save file
        ;
        ; Parameters: A filenumber, zpSrcLo-Hi startaddress, zpBitsLo-Hi amount of bytes
        ; Returns: -
        ; Modifies: A,X,Y

SaveFile:       jmp FastSave

                endif

        ; Read a byte from an opened file. Do not call after fileOpen becomes 0
        ;
        ; Parameters: -
        ; Returns: byte in A, fileOpen set to 0 after EOF
        ; Modifies: A

GetByte:        lda loadBuffer
                inc GetByte+1
                dec loadBufferPos
                beq FL_FillBuffer               ;Get next sector when reach the sector buffer end
FO_Done:        rts

FastOpen:       inc fileOpen
                ldy #$00
                if USETURBOMODE > 0
                sty $d07a                       ;SCPU to slow mode
                sty $d030                       ;C128 back to 1MHz mode
                endif
                jsr FL_SendFileNameAndCommand   ;Command 0 = load
FL_FillBuffer:  php
                pha                             ;Preserve A (the byte that was read if called from GetByte)
                stx loadTempReg
                ldx #$00
FL_FillBufferWait:
                bit $dd00                       ;Wait for 1541 to signal data ready by setting DATA high
                bpl FL_FillBufferWait
FL_FillBufferLoop:
FL_SpriteWait:  lda $d012                       ;Check for sprite Y-coordinate range
FL_MaxSprY:     cmp #$00                        ;(max & min values are filled in the
                bcs FL_NoSprites                ;raster interrupt)
FL_MinSprY:     cmp #$00
                bcs FL_SpriteWait
FL_NoSprites:   sei
FL_WaitBadLine: lda $d011
                clc
                sbc $d012
                and #$07
                beq FL_WaitBadLine
                lda #$10
                ora $dd00
                sta $dd00                       ;CLK=low to begin transfer
FL_Delay:       and #$03
                sta FL_Eor+1
                sta $dd00
FL_ReceiveByte: lda $dd00
                lsr
                lsr
                eor $dd00
                lsr
                lsr
                eor $dd00
                lsr
                lsr
                eor $dd00
                cli
FL_Eor:         eor #$00
                sta loadBuffer,x
                inx
                bne FL_FillBufferLoop
                ldx #$fe                        ;Assume full buffer
                lda loadBuffer
                bne FL_FullBuffer
                ldx loadBuffer+1
                bne FL_NoEOF
                stx fileOpen                    ;Endmark received, close file
FL_NoEOF:       dex
FL_FullBuffer:  stx loadBufferPos               ;Bytes left to read
                lda #$02
                sta GetByte+1                   ;Reset read position
                ldx loadTempReg
                pla
                plp
                rts

FL_SendFileNameAndCommand:
                jsr FL_SendByte
                tya
FL_SendByte:    sta loadTempReg
                ldx #$08                        ;Bit counter
FL_SendLoop:    bit $dd00                       ;Wait for both DATA & CLK to go high
                bpl FL_SendLoop
                bvc FL_SendLoop
                lsr loadTempReg                 ;Send one bit
                lda #$10
                ora $dd00
                bcc FL_ZeroBit
                eor #$30
FL_ZeroBit:     sta $dd00
                lda #$c0                        ;Wait for CLK & DATA low (diskdrive answers)
FL_SendAck:     and $dd00
                bne FL_SendAck
                lda #$ff-$30                    ;Set DATA and CLK high
                and $dd00
                sta $dd00
                dex
                bne FL_SendLoop
                rts

                if INCLUDESAVE > 0

FastSave:       ldy #$80                        ;Command $80 = save
                jsr FL_SendFileNameAndCommand
                lda zpBitsLo                    ;Send save length
                jsr FL_SendByte
                lda zpBitsHi
                jsr FL_SendByte
                ldy #$00
FS_Loop:        lda (zpSrcLo),y
                jsr FL_SendByte
                iny
                bne FS_NoMSB
                inc zpSrcHi
                dec zpBitsHi
FS_NoMSB:       cpy zpBitsLo
                bne FS_Loop
                lda zpBitsHi
                bne FS_Loop
                rts
                
                endif

FastLoadEnd:

loaderCodeEnd:

        ; Set sprite range for fastloading. Turns to no-op in Kernal loading.
        ;
        ; Parameters: A Y-coordinate of topmost sprite, X Y-coordinate of
        ;             bottommost sprite. If no sprites, call SetNoSprites
        ;             with A=0 instead
        ; Returns: -
        ; Modifies: A

SetSpriteRange: sec
                sbc #SPR_TOP_SAFETY
                sta FL_MinSprY+1
                txa
                adc #SPR_BOTTOM_SAFETY          ;C=1, add one more
SetNoSprites:   sta FL_MaxSprY+1
                rts

        ; Non-zeropage variables

loaderMode:     dc.b 0
ntscFlag:       dc.b 0

        ; Low-level runtime ends