;-------------------------------------------------------------------------------
;Fastloader test with unpacked file
;-------------------------------------------------------------------------------

                processor 6502
                org $0801

                include ..\config.s

                dc.b $0b,$08            ;Address of next BASIC instruction
                dc.w 10                 ;Line number
                dc.b $9e                ;SYS-token
                dc.b $32,$30,$36,$31    ;2061 in ASCII
                dc.b $00,$00,$00        ;BASIC program end

Start:          clc                     ;Init loader with fastload allowed,
                jsr InitLoader          ;Kernal will be switched off
                lda #$00
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
