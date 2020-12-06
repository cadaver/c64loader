;-------------------------------------------------------------------------------
;Fastloader test with sprites & unpacked file
;-------------------------------------------------------------------------------

                processor 6502
                org $0801

                include ..\config.s

                dc.b $0b,$08            ;Address of next BASIC instruction
                dc.w 10                 ;Line number
                dc.b $9e                ;SYS-token
                dc.b $32,$31,$37,$36    ;2176 in ASCII
                dc.b $00,$00,$00        ;BASIC program end

                org $0840
                ds.b 63,$ff             ;Sprite data

                org $0880

Start:          clc                     ;Init loader with fastload allowed,
                jsr InitLoader          ;Kernal will be switched off
                lda loaderMode
                beq SkipSprites         ;If using Kernal + serial, cannot show sprites
                ldx #$07
SpriteLoop:     txa
                sta $d027,x
                asl
                tay
                asl
                asl
                adc #96
                sta $d000,y
                sta $d001,y
                lda #$21
                sta $07f8,x
                dex
                bpl SpriteLoop
                lda #$ff
                sta $d015               ;Sprites on
                lda $d001
                ldx $d00f
                jsr SetSpriteRange      ;Set sprite min/max Y coord range for fastloader

SkipSprites:    lda #$00
                jsr LoadUnpacked        ;Load file 00 as unpacked data and with startaddress
                bcs LoadError           ;Error if carry set
                lda #$02                ;Show the picture we just loaded
                sta $dd00
                lda #59
                sta $d011
                lda #$18
                sta $d016
                lda #$80
                sta $d018
                lda #$00
                sta $d020
                sta $d015               ;Sprites off now
                lda #$01
                sta $d021
                ldx #$00
CopyColors:     lda $6400,x
                sta $d800,x
                lda $6500,x
                sta $d900,x
                lda $6600,x
                sta $da00,x
                lda $6700,x
                sta $db00,x
                inx
                bne CopyColors
                jmp WaitExit
LoadError:      lda #$02
                sta $d020
WaitExit:       lda $dc00
                and $dc01
                and #$10
                bne WaitExit
                inc $01                 ;Kernal back on to reset
                jmp 64738

                include ..\loader.s
                include ..\loadunpacked.s
                include ..\loaderinit.s
