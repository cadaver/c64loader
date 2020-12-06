;-------------------------------------------------------------------------------
; COVERT BITOPS Loader V3.0, disposable initialization
;-------------------------------------------------------------------------------

        ; Kernal zeropage variables

status          = $90
messages        = $9d
fa              = $ba

        ;Kernal routines

ScnKey          = $ff9f
CIOut           = $ffa8
Listen          = $ffb1
Second          = $ff93
UnLsn           = $ffae
Talk            = $ffb4
Tksa            = $ff96
UnTlk           = $ffab
ACPtr           = $ffa5
ChkIn           = $ffc6
ChkOut          = $ffc9
ChrIn           = $ffcf
ChrOut          = $ffd2
Close           = $ffc3
ClAll           = $ffe7
Open            = $ffc0
SetMsg          = $ff90
SetNam          = $ffbd
SetLFS          = $ffba
ClrChn          = $ffcc
GetIn           = $ffe4
Load            = $ffd5
Save            = $ffd8

        ; Drive defines

iddrv0          = $12           ;Disk drive ID (1541 only)
id              = $16           ;Disk ID (1541 only)

drvFileTrk      = $0300
drvBuf          = $0400         ;Sector data buffer
drvStart        = $0404
drvFileSct      = $0700
InitializeDrive = $d005         ;1541 only

MW_LENGTH       = 32            ;Bytes in one M-W command

        ; Init loader
        ; Can be overwritten after calling
        ;
        ; Parameters: C=0 detect drive, C=1 force Kernal safe loader
        ; Returns: loaderMode set, ntscFlag set, timer IRQ disabled, Kernal off ($35)
        ; Modifies: A,X,Y

InitLoader:     php
                sei
                ldx #$00
                stx fileOpen
                stx $d07a                       ;SCPU to slow mode
                lda #$7f
                sta $dc0d
                lda $dc0d                       ;Disable & acknowledge IRQ sources
                lda #<NMI
                sta $0318
                lda #>NMI
                sta $0319
                lda #$81                        ;Run CIA2 Timer A once to disable NMI from Restore keypress
                sta $dd0d                       ;Timer A interrupt source
                lda #$01                        ;Timer A count ($0001)
                sta $dd04
                stx $dd05
                lda #%00011001                  ;Run Timer A in one-shot mode
                sta $dd0e
                lda #$36                        ;Kernal on, Basic off
                sta $01
IL_DetectNtsc1: lda $d012                       ;Detect PAL/NTSC/Drean
IL_DetectNtsc2: cmp $d012
                beq IL_DetectNtsc2
                bmi IL_DetectNtsc1
                cmp #$20
                bcc IL_IsNtsc
IL_CountCycles: inx
                lda $d012
                bpl IL_CountCycles
                cpx #$8d
                bcc IL_IsPal
IL_IsDrean:     txa
IL_IsNtsc:      sta ntscFlag
                lda #$25                        ;Adjust 2-bit fastload transfer delay for NTSC / Drean
                sta FL_Delay
                lda #fastLoadEor
                sta FL_Delay+1
                lda #$03
                sta fastLoadEor
IL_IsPal:       plp
                bcc IL_TryUploadDriveCode       ;Check for forcing safe mode
                jmp IL_CopyLoader

        ; Drive detection stage 1: try to start the drivecode

IL_TryUploadDriveCode:
                lda #$aa
                sta $a5
                ldy #$00
                beq UDC_NextPacket
UDC_SendMW:     lda ilMWString,x                ;Send M-W command (backwards)
                jsr CIOut
                dex
                bpl UDC_SendMW
                ldx #MW_LENGTH
UDC_SendData:   lda ilDriveCode,y               ;Send one byte of drive code
                jsr CIOut
                iny
                bne UDC_NotOver
                inc UDC_SendData+2
UDC_NotOver:    inc ilMWString+2                ;Also, move the M-W pointer forward
                bne UDC_NotOver2
                inc ilMWString+1
UDC_NotOver2:   dex
                bne UDC_SendData
                jsr UnLsn                       ;Unlisten to perform the command
UDC_NextPacket: lda fa                          ;Set drive to listen
                jsr Listen
                lda status                      ;Quit to Kernal mode if error
                bmi IL_UseKernal
                lda #$6f
                jsr Second
                ldx #$05
                dec ilNumPackets                ;All "packets" sent?
                bpl UDC_SendMW
UDC_SendME:     lda ilMEString-1,x              ;Send M-E command (backwards)
                jsr CIOut
                dex
                bne UDC_SendME
                jsr UnLsn
IL_WaitDataLow: lda status
                bmi IL_UseKernal
                bit $dd00                       ;Wait for drivecode to signal activation with DATA=low
                bpl IL_FastLoadOK               ;If not detected within time window, use slow loading
                dex
                bne IL_WaitDataLow
                beq IL_NoDriveCode              ;Fastload start failed, continue with ID check
IL_FastLoadOK:  dec loaderMode                  ;Mark IRQ-loader mode in use
                bne IL_Done                     ;No loader runtime copy necessary

IL_UseKernal:   lda $a5
                cmp #$aa                        ;Serial bus used?
                bne IL_CopyLoader               ;If not, VICE virtual drive or IDE64, can allow interrupts
IL_NoSerial:    inc loaderMode
                lda #$60
                sta SetSpriteRange              ;SetSpriteRange disabled in Kernal-mode
                sta SetNoSprites
                sta StopIrq                     ;Default stopIRQ-code for Kernal mode: just RTS
IL_CopyLoader:  ldx #loaderCodeEnd-loaderCodeStart
IL_CopyLoaderLda:
                lda ilSlowLoadStart-1,x
                sta OpenFile-1,x
                dex
                bne IL_CopyLoaderLda
IL_Done:        dec $01                         ;Loader init done, switch Kernal off
                rts

        ; Drive detection stage 2: check ID to detect SD2IEC

IL_NoDriveCode: lda #$02                        ;Reset drive, read back ID, using only non-serial routines
                ldx #<ilUICmd
                ldy #>ilUICmd
                jsr SetNam
                lda #$0f
                tay
                ldx fa
                jsr SetLFS
                jsr Open
                ldx #$0f
                jsr ChkIn
                ldx #$1f
IL_ReadID:      jsr ChrIn
                sta ilIDBuffer,x
                dex
                bpl IL_ReadID
                lda #$0f
                jsr Close
                ldx #$1d
IL_CheckID:     lda ilIDBuffer+2,x
                cmp #"I"
                bne IL_CheckIDNext
                lda ilIDBuffer+1,x
                cmp #"E"
                bne IL_CheckIDNext
                lda ilIDBuffer,x
                cmp #"C"
                beq IL_HasSD2IEC
IL_CheckIDNext: dex
                bpl IL_CheckID
                bmi IL_UseKernal

IL_HasSD2IEC:   ldx #$00
IL_CopyELoadHelper:
                lda ilELoadHelper,x             ;Copy full 256 bytes (sector buffer) of ELoad helper code, exact length doesn't matter
                sta ELoadHelper,x
                inx
                bne IL_CopyELoadHelper
                dec loaderMode                  ;Mark IRQ-loader mode in use, also for ELoad
                lda #<(ilELoadStart-1)
                ldx #>(ilELoadStart-1)
                sta IL_CopyLoaderLda+1
                stx IL_CopyLoaderLda+2
                jmp IL_CopyLoader

        ; NMI routine for init
        
NMI:            rti

        ; Slow fileopen / getbyte / save routines

ilSlowLoadStart:

                rorg OpenFile

        ; Open file
        ;
        ; Parameters: A fileNumber
        ; Returns: fileOpen 0 if failed to open
        ; Modifies: A,X,Y

               jmp SlowOpen

        ; Save file
        ;
        ; Parameters: A filenumber, zpSrcLo-Hi startaddress, zpBitsLo-Hi amount of bytes
        ; Returns: -
        ; Modifies: A,X,Y

               jmp SlowSave

        ; Read a byte from an opened file. Do not call after fileOpen becomes 0
        ;
        ; Parameters: -
        ; Returns: byte in A, fileOpen set to 0 after EOF
        ; Modifies: A

SlowGetByte:    lda #$36                        ;Check for first buffered byte
                bmi SGB_BufferedByte
                sta $01
                php
                jsr ChrIn
                bit status
                bvs SGB_EOF
                plp
KernalOff:      dec $01
                rts
SGB_BufferedByte:
                php
                asl SlowGetByte+1
                lda loadTempReg
                plp
                rts

SGB_EOF:        pha
                stx loadTempReg
                sty loadBufferPos
                jsr CloseKernalFile
                ldx loadTempReg
                ldy loadBufferPos
                pla
                plp
                rts

SlowOpen:       jsr PrepareKernalIO
                ldy #$00
                jsr SetLFSOpen                  ;Open for read
                jsr ChkIn
                jsr ChrIn
                sta loadTempReg                 ;Store buffered first byte
                lda status                      ;If nonzero status after first byte -> error (game doesn't have 1-byte files)
                beq KernalOff
                bne CloseKernalFile

SlowSave:       jsr PrepareKernalIO
                ldy #$01                        ;Open for write
                jsr SetLFSOpen
                jsr ChkOut
                ldy #$00
SS_Loop:        lda (zpSrcLo),y
                jsr ChrOut
                iny
                bne SS_NoMSB
                inc zpSrcHi
                dec zpBitsHi
SS_NoMSB:       cpy zpBitsLo
                bne SS_Loop
                lda zpBitsHi
                bne SS_Loop
CloseKernalFile:lda #$02
                jsr Close
                lda #$00
                sta fileOpen
                beq KernalOff

PrepareKernalIO:pha                             ;Convert filenumber to filename
                ldx #$01
                and #$0f
                jsr CFN_Sub
                dex
                pla
                lsr
                lsr
                lsr
                lsr
                jsr CFN_Sub
                inc fileOpen                    ;Set fileopen indicator, raster delays are to be expected
                if USETURBOMODE > 0
                stx $d07a                       ;SCPU to slow mode
                stx $d030                       ;C128 back to 1MHz mode
                endif
                jsr StopIrq
                lda #$36/2+$80
                sta SlowGetByte+1               ;First buffered byte indicator + shifted $01 value
                asl
                sta $01
SetFileName:    lda #$05
                ldx #<slowReplace
                ldy #>slowReplace
                jmp SetNam

CFN_Sub:        ora #$30
                cmp #$3a
                bcc CFN_Number
                adc #$06
CFN_Number:     sta slowFileName,x
                rts

SetLFSOpen:     ldx fa
                lda #$02
                jsr SetLFS
                jsr Open
                ldx #$02
                rts

slowReplace:    dc.b "@"
slowUnitAndFileName:
                dc.b "0:"
slowFileName:   dc.b "  "

SlowLoadEnd:

                rend

ilSlowLoadEnd:

ilELoadStart:

        ; ELoad fileopen / getbyte / save routines

                rorg OpenFile

        ; Open file
        ;
        ; Parameters: A filenumber
        ; Returns: fileOpen 0 if failed to open
        ; Modifies: A,X,Y

                jmp ELoadOpen

        ; Save file
        ;
        ; Parameters: A filenumber, zpSrcLo-Hi startaddress, zpBitsLo-Hi amount of bytes
        ; Returns: -
        ; Modifies: A,X,Y

                jmp ELoadSave

        ; Read a byte from an opened file. Do not call after fileOpen becomes 0
        ;
        ; Parameters: -
        ; Returns: byte in A, fileOpen set to 0 after EOF
        ; Modifies: A

ELoadGetByte:   jmp EL_GetByteFast

EL_EOF:         jsr EL_CloseReadFile
                beq EL_NoEOF

ELoadOpen:      jsr EL_PrepareIO
                lda #$f0                        ;Open for read
                jsr EL_SendFileNameShort
                jsr EL_Init
                jsr EL_SendLoadCmdFast
                sta loadBufferPos               ;Dummy value (0) to prevent re-refill during initial refill
EL_Refill:      pha
                php
                jmp EL_FinishRefill             ;Convoluted jumping to get the sprite-related addresses to align with the original fastloader

EL_GetByteFast: php
EL_GetByteWait: bit $dd00                       ;Wait for drive to signal data ready with
                bmi EL_GetByteWait              ;DATA low
EL_SpriteWait:  lda $d012                       ;Check for sprite Y-coordinate range
EL_MaxSprY:     cmp #$00                        ;(max & min values are filled in the
                bcs EL_NoSprites                ;raster interrupt)
EL_MinSprY:     cmp #$00
                bcs EL_SpriteWait
EL_NoSprites:   sei
EL_BadLineWait: lda $d011
                clc
                sbc $d012
                and #7
                beq EL_BadLineWait
                lda #$10
                ora $dd00
                sta $dd00
                bit loadTempReg
                and #$03
                sta $dd00
                sta EL_Eor+1
                nop
                nop
                nop
                nop
                lda $dd00
                lsr
                lsr
                eor $dd00
                lsr
                lsr
                eor $dd00
                lsr
                lsr
EL_Eor:         eor #$00
                eor $dd00
                cli
                plp
                dec loadBufferPos
                beq EL_Refill
EL_NoRefill:    rts

EL_FinishRefill:jsr EL_GetByteFast              ; This will decrement loadBufferPos for the second time, but does not matter
                cmp #$00
                beq EL_EOF
                cmp #$ff
                beq EL_EOF
EL_NoEOF:       sta loadBufferPos               ; Bytes to read in the next "buffer"
                plp
                pla
                rts

ELoadSave:      jsr EL_PrepareIO
                lda #$f1                        ;Open for write
                ldy #<eloadReplace              ;Use the long filename with replace command
                jsr EL_SendFileName
                lda #$61
                jsr EL_ListenAndSecond          ;Open write stream
                ldy #$00
ES_Loop:        lda (zpSrcLo),y
                jsr EL_SendByte
                iny
                bne ES_NoMSB
                inc zpSrcHi
                dec zpBitsHi
ES_NoMSB:       cpy zpBitsLo
                bne ES_Loop
                lda zpBitsHi
                bne ES_Loop
                jsr EL_Unlisten
                lda #$e1                        ;Close the write stream
                jmp EL_CloseFile

EL_SendFileNameShort:
                ldy #<eloadFileName
EL_SendFileName:jsr EL_ListenAndSecond
                ldx #<eloadFileNameEnd
                jmp EL_SendBlock

EL_PrepareIO:   inc fileOpen
                pha
                ldx #$01
                and #$0f
                jsr EL_CFNSub
                dex
                if USETURBOMODE > 0
                stx $d07a                       ;SCPU to slow mode
                stx $d030                       ;C128 back to 1MHz mode
                endif
                pla
                lsr
                lsr
                lsr
                lsr
                jmp EL_CFNSub

ELoadEnd:
                rend

ilELoadEnd:

        ; ELoad helper code / IEC protocol implementation

ilELoadHelper:

                rorg ELoadHelper

        ; IEC communication routines

EL_ListenAndSecond:
                pha
                lda fa
                ora #$20
                jsr EL_SendByteATN
                pla
                jsr EL_SendByteATN
                lda $dd00
                and #$03
                ora #$10                        ;After the secondary address, just CLK low for further non-atn bytes
                sta $dd00
                rts

EL_SendBytePrepare:
                sta loadTempReg
                lda $dd00
                and #$03
                sta EL_SetLinesIdle+1
                rts

EL_SendByteATN: jsr EL_SendBytePrepare
                ora #$08
                bne EL_SendByteCommon

EL_SendByte:
EL_WaitDataLow: bit $dd00                       ;for non-ATN bytes, wait for DATA low before we continue
                bmi EL_WaitDataLow
                jsr EL_SendBytePrepare
EL_SendByteCommon:
                sta $dd00                       ;CLK high -> ready to send; wait for DATA high response
                jsr EL_WaitDataHigh
                pha
                lda #$60
                sec
EL_WaitEOI:     sbc #$01                        ;Wait until we are sure to have generated an EOI response
                cmp #$09                        ;It doesn't matter that every byte we send is with EOI
                bcs EL_WaitEOI
                jsr EL_WaitDataHigh             ;Wait until EOI response over
                sta loadBufferPos               ;Bit counter
                pla
EL_SendByteLoop:and #$08+$03                    ;CLK low
                ora #$10
                sei                             ;Timing of last CLK high (last bit) in ATN bytes is critical for SD2IEC,
                sta $dd00                       ;as delay means enabling JiffyDOS, which we don't want. For simplicity,
                dec loadBufferPos               ;disable IRQs for each bit sent, causing IRQ delay similar to the 2-bit transfer
                bmi EL_SendByteDone
                jsr EL_Delay27
                and #$08+$03
                lsr loadTempReg
                bcs EL_SendBitOne
                ora #$20                        ;CLK high + data bit
EL_SendBitOne:  sta $dd00
                cli
                jsr EL_Delay27
                jmp EL_SendByteLoop
EL_SendByteDone:cli
                rts

EL_Delay27:     jsr EL_Delay18
                jmp EL_Delay12

        ; Close file
        
EL_CloseReadFile:   
                lda #$e0
EL_CloseFile:   jsr EL_ListenAndSecond
                jsr EL_Unlisten                 ;Returns with A=0
EL_CloseFileDelay:
                adc #$01
                bne EL_CloseFileDelay           ;Delay for load - save - load -sequence with NTSC machine, which could hang up without
                sta fileOpen
                rts

        ; Init the eload1 drivecode

EL_Init:        lda #"W"
                ldx #<eloadMWStringEnd
                jsr EL_SendCommand
                lda #"E"
                ldx #<eloadMEStringEnd
EL_SendCommand: sta eloadMWString+2
                ldy #<eloadMWString
                lda #$6f
                jsr EL_ListenAndSecond
EL_SendBlock:   stx EL_SendEndCmp+1
EL_SendBlockLoop:
                lda ELoadHelper,y
                jsr EL_SendByte
                iny
EL_SendEndCmp:  cpy #$00
                bcc EL_SendBlockLoop
EL_Unlisten:    lda #$3f                        ;Unlisten command always after a block
                jsr EL_SendByteATN
                jsr EL_SetLinesIdle             ;Let go of DATA+CLK+ATN
EL_WaitDataHigh:bit $dd00                       ;Wait until device lets go of the DATA line
                bpl EL_WaitDataHigh             
                rts

        ; Send load command by fast protocol

EL_SendLoadCmdFast: 
                bit $dd00                       ;Wait for drive to signal ready to receive
                bvs EL_SendLoadCmdFast          ;with CLK low
                lda EL_SetLinesIdle+1
                ora #$20
                tax                             ;pull data low to acknowledge
                stx $dd00
EL_SendFastWait:bit $dd00                       ;Wait for drive to release CLK
                bvc EL_SendFastWait
EL_SendFastWaitBorder:  
                bit $d011                       ;Wait to be in border for no badlines
                bpl EL_SendFastWaitBorder
                jsr EL_SetLinesIdle             ;Waste cycles / send 0 bits
                jsr EL_SetLinesIdle
                jsr EL_Delay12                  ;Send the lower nybble (always 1)
                stx $dd00
                nop
                nop
EL_SetLinesIdle:lda #$00                        ;Rest of bits / idle value
EL_Delay18:     sta $dd00
                nop
EL_Delay12:     rts

        ; Subroutine for filename conversion

EL_CFNSub:      ora #$30
                cmp #$3a
                bcc EL_CFNNumber
                adc #$06
EL_CFNNumber:   sta eloadFileName,x
                rts

        ; Strings

eloadMWString:  dc.b "M-W"
                dc.w $0300
eloadMEStringEnd:
                dc.b 6
                dc.b "eload1"
eloadMWStringEnd:

eloadReplace:   dc.b "@0:"
eloadFileName:  dc.b "  "
eloadFileNameEnd:

ELoadHelperEnd:
                rend

ilELoadHelperEnd:

        ; Drivecode

ilDriveCode:
                rorg drvStart

        ; Drive patch data
                 
        ; 1MHz transfer routine

Drv1MHzSend:    lda drvSendTbl,x
Drv1MHzWait:    ldx $1800                       ;Wait for CLK=low
                beq Drv1MHzWait
                sta $1800
                asl
                and #$0f
                sta $1800
                lda drvSendTbl,y
                sta $1800
                asl
                and #$0f
                sta $1800
                inc Drv2MHzSend+1
                bne *-48
                beq *+13
Drv1MHzSendEnd:

drvFamily:      dc.b $43,$0d,$ff
drvIdByte:      dc.b "8","F","H"
drvIdLocLo:     dc.b $a4,$c6,$e9
drvIdLocHi:     dc.b $fe,$e5,$a6

drvPatchData:
drvJobTrkLo:    dc.b <$000d,<$000d,<$2802
drvJobTrkHi:    dc.b >$000d,>$000d,>$2802
drvJobSctLo:    dc.b <$000e,<$000e,<$2803
drvJobSctHi:    dc.b >$000e,>$000e,>$2803
drvExecLo:      dc.b <$ff54,<DrvFdExec,<$ff4e
drvExecHi:      dc.b >$ff54,>DrvFdExec,>$ff4e
drvLedBit:      dc.b $40,$40,$00
drvLedAdrHi:    dc.b $40,$40,$05
drvLedAdrHi2:   dc.b $40,$40,$05
drvDirTrkLo:    dc.b <$022b,<$54,<$2ba7
drvDirTrkHi:    dc.b >$022b,>$54,>$2ba7
drvDirSctLo:    dc.b <drv1581DirSct,<$56,<$2ba9
drvDirSctHi:    dc.b >drv1581DirSct,>$56,>$2ba9

drvPatchOfs:    dc.b DrvReadTrk+1-DrvReadTrk
                dc.b DrvReadTrk+2-DrvReadTrk
                dc.b DrvReadSct+1-DrvReadTrk
                dc.b DrvReadSct+2-DrvReadTrk
                dc.b DrvExecJsr+1-DrvReadTrk
                dc.b DrvExecJsr+2-DrvReadTrk
                dc.b DrvLed+1-DrvReadTrk
                dc.b DrvLedAcc0+2-DrvReadTrk
                dc.b DrvLedAcc1+2-DrvReadTrk
                dc.b DrvDirTrk+1-DrvReadTrk
                dc.b DrvDirTrk+2-DrvReadTrk
                dc.b DrvDirSct+1-DrvReadTrk
                dc.b DrvDirSct+2-DrvReadTrk
                dc.b 0
                
drv1800Lo:      dc.b <$4001,<$4001,<$8000
drv1800Hi:      dc.b >$4001,>$4001,>$8000

drv1800Ofs:     dc.b Drv2MHzSerialAcc1-Drv2MHzSerialAcc1+1
                dc.b Drv2MHzSerialAcc2-Drv2MHzSerialAcc1+1
                dc.b Drv2MHzSerialAcc3-Drv2MHzSerialAcc1+1
                dc.b Drv2MHzSerialAcc4-Drv2MHzSerialAcc1+1
                dc.b Drv2MHzSerialAcc5-Drv2MHzSerialAcc1+1
                dc.b Drv2MHzSerialAcc6-Drv2MHzSerialAcc1+1
                dc.b DrvSerialAcc7-Drv2MHzSerialAcc1+1
                dc.b DrvSerialAcc8-Drv2MHzSerialAcc1+1
                dc.b DrvSerialAcc9-Drv2MHzSerialAcc1+1
                dc.b DrvSerialAcc10-Drv2MHzSerialAcc1+1
                dc.b DrvSerialAcc11-Drv2MHzSerialAcc1+1
                dc.b 0

DrvDetect:      sei
                ldy #$01
DrvIdLda:       lda $fea0                       ;Recognize drive family
                ldx #$03                        ;(from Dreamload)
DrvIdLoop:      cmp drvFamily-1,x
                beq DrvFFound
                dex                             ;If unrecognized, assume 1541
                bne DrvIdLoop
                beq DrvIdFound
DrvFFound:      lda #<(drvIdByte-1)
                sta DrvIdLoop+1
                lda drvIdLocLo-1,x
                sta DrvIdLda+1
                lda drvIdLocHi-1,x
                sta DrvIdLda+2
                dey
                bpl DrvIdLda
DrvIdFound:     txa
                bne DrvNot1541
                lda #$2c                        ;On 1541/1571, patch out the flush ($a2) job call
                sta DrvFlushJsr
                lda #$7a                        ;Set data direction so that can compare against $1800 being zero
                sta $1802
                lda $e5c6
                cmp #$37
                bne DrvNot1571                  ;Recognize 1571 as a subtype
                jsr DrvNoData                   ;Set DATA=low to signal C64 we're here, before the slow 2MHz enable
                jsr $904e                       ;Enable 2Mhz mode, overwrites buffer at $700
                jmp DrvPatchDone
DrvNot1571:     ldy #Drv1MHzSendEnd-Drv1MHzSend-1 ;For non-1571, copy 1MHz transfer code
Drv1MHzCopy:    lda Drv1MHzSend,y
                sta DrvSendPatchStart,y
                dey
                bpl Drv1MHzCopy
                bmi DrvPatchDone
DrvNot1541:
DrvPatch1800Loop:
                ldy drv1800Ofs                  ;Patch $1800 accesses
                beq DrvPatch1800Done            ;Offset 0 = endmark
                lda drv1800Lo-1,x
                sta Drv2MHzSerialAcc1,y
                lda drv1800Hi-1,x
                sta Drv2MHzSerialAcc1+1,y
                inc DrvPatch1800Loop+1
                bne DrvPatch1800Loop
DrvPatch1800Done:
DrvPatchGeneralLoop:
                ldy drvPatchOfs
                beq DrvPatchGeneralDone         ;Offset 0 = endmark
                lda drvPatchData-1,x
                sta DrvReadTrk,y
                inx
                inx
                inx
                inc DrvPatchGeneralLoop+1
                bne DrvPatchGeneralLoop
DrvPatchGeneralDone:
                lda #$60                        ;Patch exit jump as RTS
                sta DrvExitJump
DrvPatchDone:   jsr DrvNoData                   ;Set DATA=low to signal C64 we're here
                tax
DrvBeginDelay:  inx                             ;Delay to make sure C64 catches the signal
                bne DrvBeginDelay

        ; Drive main loop

DrvMain:        jsr DrvGetByte                  ;Get filenumber
                sta DrvFileNumber+1
                jsr DrvGetByte                  ;Get command
                bpl DrvLoad

DrvSave:        jsr DrvGetByte                  ;Get amount of bytes to expect
                sta DrvSaveCountLo+1
                jsr DrvGetByte
                sta DrvSaveCountHi+1
                jsr DrvFindFile
                bne DrvSaveFound
                beq DrvSaveFinish               ;If file not found, just receive the bytes
DrvSaveSectorLoop:
                jsr DrvReadSector               ;First read the sector for T/S chain
DrvSaveFound:   ldy #$02
DrvSaveByteLoop:jsr DrvGetSaveByte              ;Then get bytes from C64 and write
                bcs DrvSaveSector               ;If last byte, save the last sector
                sta drvBuf,y
                iny
                bne DrvSaveByteLoop
DrvSaveSector:  lda #$90
                jsr DrvDoJob
                lda drvBuf+1                    ;Follow the T/S chain
                ldx drvBuf
                bne DrvSaveSectorLoop
DrvSaveFinish:  jsr DrvGetSaveByte              ;Make sure all bytes are received
                bcc DrvSaveFinish
DrvFlush:       lda #$a2                        ;Flush buffers (1581 and CMD drives)
DrvFlushJsr:    jsr DrvDoJob
                jmp DrvMain

DrvLoad:        jsr DrvFindFile
                bne DrvSendBlk
DrvFileNotFound:
DrvEndMark:     stx drvBuf
                stx drvBuf+1
                jmp DrvSendBlk

DrvSectorLoop:  jsr DrvReadSector               ;Read the data sector
DrvSendBlk:     ldx #$00
Drv2MHzSend:    lda drvBuf
                tay
                and #$0f
Drv2MHzSerialAcc1:
                stx $1800                       ;Set DATA=high to mark data available
                tax
                tya
                lsr
                lsr
                lsr
                lsr
                tay
DrvSendPatchStart:
                lda #$04                        ;Wait for CLK=low
Drv2MHzSerialAcc2:
                bit $1800
                beq Drv2MHzSerialAcc2
                lda drvSendTbl,x
                nop
                nop
Drv2MHzSerialAcc3:
                sta $1800
                asl
                and #$0f
                cmp ($00,x)
                nop
Drv2MHzSerialAcc4:
                sta $1800
                lda drvSendTbl,y
                cmp ($00,x)
                nop
Drv2MHzSerialAcc5:
                sta $1800
                asl
                and #$0f
                cmp ($00,x)
                nop
Drv2MHzSerialAcc6:
                sta $1800
                inc Drv2MHzSend+1
                bne DrvSendBlk
DrvSendDone:    jsr DrvNoData
                lda drvBuf+1                    ;Follow the T/S chain
                ldx drvBuf
                bne DrvSectorLoop
                tay                             ;If 2 first bytes are both 0,
                bne DrvEndMark                  ;endmark has been sent and can return to main loop
                jmp DrvMain

DrvNoMoreBytes: sec
                rts
DrvGetSaveByte:
DrvSaveCountLo: lda #$00
                tax
DrvSaveCountHi: ora #$00
                beq DrvNoMoreBytes
                dec DrvSaveCountLo+1
                txa
                bne DrvGetByte
                dec DrvSaveCountHi+1

DrvGetByte:     cli                             ;Timing not critical; allow interrupts (motor will stop)
                ldx #$08                        ;Bit counter
DrvGetBitLoop:  lda #$00
DrvSerialAcc7:  sta $1800                       ;Set CLK & DATA high for next bit
DrvSerialAcc8:  lda $1800
                bmi DrvQuit                     ;Quit if ATN is low
                and #$05                        ;Wait for CLK or DATA going low
                beq DrvSerialAcc8
                sei                             ;Disable interrupts after 1st bit to make sure "no data" signal will be on time
                lsr                             ;Read the data bit
                lda #$02
                bcc DrvGetZero
                lda #$08
DrvGetZero:     ror drvReceiveBuf               ;Store the data bit
DrvSerialAcc9:  sta $1800                       ;And acknowledge by pulling the other line low
DrvSerialAcc10: lda $1800                       ;Wait for either line going high
                and #$05
                cmp #$05
                beq DrvSerialAcc10              ;C=0 after exiting this loop
                dex
                bne DrvGetBitLoop
DrvNoData:      lda #$02                        ;DATA low - no sector data to be transmitted yet
DrvSerialAcc11: sta $1800                       ;or C64 cannot yet transmit next byte
                lda drvReceiveBuf
                rts

DrvQuit:        pla
                pla
DrvExitJump:    lda #$1a                        ;Restore data direction when exiting
                sta $1802
                jmp InitializeDrive             ;1541 = exit through Initialize, others = exit through RTS

DrvReadSector:
DrvReadTrk:     stx.w $0008
DrvReadSct:     sta.w $0009
                lda #$80
DrvDoJob:       sta DrvRetry+1
                jsr DrvLed
DrvRetry:       lda #$80
                ldx #$01
DrvExecJsr:     jsr Drv1541Exec                 ;Exec buffer 1 job
                cmp #$02                        ;Error?
                bcs DrvRetry                    ;Retry infinitely until success
DrvSuccess:     sei                             ;Make sure interrupts now disabled
DrvLed:         lda #$08
DrvLedAcc0:     eor $1c00
DrvLedAcc1:     sta $1c00
                rts

Drv1541Exec:    sta $01                         ;Set command for execution
                cli                             ;Allow interrupts to execute command
Drv1541ExecWait:
                lda $01                         ;Wait until command finishes
                bmi Drv1541ExecWait
                pha
                ldx #$01
DrvCheckID:     lda id,x                        ;Check for disk ID change
                cmp iddrv0,x                    ;(1541 only)
                beq DrvIDOK
                sta iddrv0,x
                lda #$00                        ;If changed, force recache of dir
                sta DrvCacheStatus+1
DrvIDOK:        dex
                bpl DrvCheckID
                pla
                rts

DrvFdExec:      jsr $ff54                       ;FD2000 fix By Ninja
                lda $03
                rts

DrvFindFail:    lda #$00                        ;If ID changed or file not found, force recache of dir
                sta DrvCacheStatus+1
DrvFindSuccess: rts

DrvFindHasEntry:jsr DrvReadSector               ;Read file's initial sector
                lda DrvCacheStatus+1
                bne DrvFindSuccess              ;If diskside was changed in the meanwhile, recache dir & retry
DrvFindFile:
DrvCacheStatus: lda #$00                        ;Reset cache if diskside changed / file not found
                bne DrvDirTrk
                tax
DrvClearFiles:  sta drvFileTrk,x                ;Mark all files as nonexistent first
                inx
                bne DrvClearFiles
DrvDirTrk:      ldx drv1541DirTrk               ;Start over from first directory block
DrvDirSct:      lda drv1541DirSct
DrvDirLoop:     stx DrvNextDirTrk+1
                sta DrvNextDirSct+1
DrvFileNumber:  ldy #$00
                lda drvFileSct,y
                ldx drvFileTrk,y                ;Check if already has entry for file
                bne DrvFindHasEntry
DrvNextDirSct:  lda #$00
DrvNextDirTrk:  ldx #$00                        ;If not, read next directory block, until no more
                beq DrvFindFail
                inc DrvCacheStatus+1            ;At least 1 dir block read, do not reset until failed
                jsr DrvReadSector               ;Read sector
                ldy #$02
DrvFileLoop:    lda drvBuf,y                    ;File type must be PRG
                and #$83
                cmp #$82
                bne DrvSkipFile
                lda drvBuf+3,y                  ;Convert filename
                cmp #$47                        ;Skip if not hexadecimal
                bcs DrvSkipFile
                jsr DrvDecodeLetter             ;into an index for the cache
                asl
                asl
                asl
                asl
                sta DrvIndexOr+1
                lda drvBuf+4,y
                jsr DrvDecodeLetter
DrvIndexOr:     ora #$00
                tax
                lda drvBuf+1,y
                sta drvFileTrk,x
                lda drvBuf+2,y
                sta drvFileSct,x
DrvSkipFile:    tya
                clc
                adc #$20
                tay
                bcc DrvFileLoop
                lda drvBuf+1                    ;Go to next directory block, until no
                ldx drvBuf                      ;more directory blocks
                bcs DrvDirLoop

DrvDecodeLetter:sec
                sbc #$30
                cmp #$10
                bcc DrvDecodeLetterDone
                sbc #$07
DrvDecodeLetterDone:
                rts

drvSendTbl:     dc.b $0f,$07,$0d,$05
                dc.b $0b,$03,$09,$01
                dc.b $0e,$06,$0c,$04
                dc.b $0a,$02,$08,$00

drv1541DirSct  = drvSendTbl+7                   ;Byte $01
drv1581DirSct  = drvSendTbl+5                   ;Byte $03

drv1541DirTrk:  dc.b 18
drvReceiveBuf:  dc.b 0

                if drvReceiveBuf >= $0700
                    err
                endif

                if (DrvMain != $0500)
                    err
                endif

                rend

ilDriveCodeEnd:

        ; Drive detection + drivecode upload commands

ilMWString:     dc.b MW_LENGTH,>drvStart, <drvStart,"W-M"
ilMEString:     dc.b >DrvDetect,<DrvDetect, "E-M"
ilNumPackets:   dc.b (ilDriveCodeEnd-ilDriveCode+MW_LENGTH-1)/MW_LENGTH
ilUICmd:        dc.b "UI"

        ; Device ID buffer for detecting SD2IEC

ilIDBuffer:     ds.b 32,0

        ; Loader validity checks
        
                if loaderCodeEnd - loaderCodeStart > $ff
                err
                endif

                if SlowLoadEnd > FastLoadEnd
                err
                endif

                if ELoadEnd > FastLoadEnd
                err
                endif

                if ELoadHelperEnd > $0300
                err
                endif

                if FL_MaxSprY != EL_MaxSprY
                err
                endif

                if FL_MinSprY != EL_MinSprY
                err
                endif