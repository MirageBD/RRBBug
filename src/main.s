screen					= $06000						; currently $3000+ big
screenorg				= $0e000						; currently $0300 big
palette					= $00400
charmem					= $10000

zpscrdst				= $f0
zpcoldst				= $f4
zpscrsrc				= $f8

zp0 = $80
zp1 = $84

maxsprites				= 59							; 58 is right, 59 is wrong (with screenheight of 25)
spritesize				= 4								; (2*(gotox+char)) ; actual chars, not NCM chars

rrbscreenwidth			= (2*(20+maxsprites*spritesize+1+1)) ; actual chars, not NCM chars
rrbscreenheight 		= 26

screenorgwidth			= ((2*512)/8)
screenorgheight 		= (88/8)

gotox320charmem			= 320*64
regularcharmem			= 256*64

; ----------------------------------------------------------------------------------------------------

.segment "MAIN"

entry_main

		sei

		lda #$35
		sta $01

		lda #%10000000									; Clear bit 7 - HOTREG
		trb $d05d

		lda #$00										; unmap
		tax
		tay
		taz
		map
		eom

		lda #$41										; enable 40MHz
		sta $00

		lda #$47										; enable C65GS/VIC-IV IO registers
		sta $d02f
		lda #$53
		sta $d02f
		eom

		;lda #%10000000									; force PAL mode, because I can't be bothered with fixing it for NTSC
		;trb $d06f										; clear bit 7 for PAL ; trb $d06f 
		;tsb $d06f										; set bit 7 for NTSC  ; tsb $d06f

		lda #$7f										; disable CIA interrupts
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
		sta $d01a

		lda #$00
		sta $d012
		lda #<fastload_irq_handler
		ldx #>fastload_irq_handler
		sta $fffe
		stx $ffff

		lda #$01										; ACK
		sta $d01a

		cli

		jsr fl_init
		jsr fl_waiting
		FLOPPY_IFFL_FAST_LOAD_INIT "RRB.IFFLCRCH"
		FLOPPY_IFFL_FAST_LOAD
		FLOPPY_IFFL_FAST_LOAD
		FLOPPY_IFFL_FAST_LOAD
		jsr fl_exit

		sei

		lda #$35
		sta $01

		lda #$00
		sta $d020
		lda #$0f
		sta $d021

		DMA_RUN_JOB clearcolorramjob
		DMA_RUN_JOB clearscreenjob

														; Enable double line RRB to double the time for RRB operations 
		lda #%00001000									; SET V400, CLEAR H640 FAST ATTR BPM H1280 MONO INT
		sta $d031
		lda #%01000000									; SET DBLRR, CLEAR DBLRR XPOSMSB
		sta $d051
		lda #$00    									; Set CHRYSCL = 0
		sta $d05b

		lda #%01000101 									; Enable 16 bit char numbers (bit 0) CHR16 and FCM for chars > $ff (bit2) FCLRHI, Remember to retain VFAST
		sta $d054

		lda #$50; -32									; set TEXTXPOS to same as SDBDRWDLSB. -2 is 1 pixel, so -32 is 16 pixels
		sta $d04c

		lda #<.loword(screen)							; Set pointer to screen ram
		ldx #>.loword(screen)
		ldy #<.hiword(screen)
		ldz #>.hiword(screen)
		sta $d060
		stx $d061
		sty $d062
		stz $d063 ; careful. overwritten a bit later

		lda #<(20+maxsprites*spritesize+1+1)			; CHRCOUNT - Number of 16-bit characters to display per row
		sta $d05e										; display_row_width in VHDL
		sta $c000
		lda #>(20+maxsprites*spritesize+1+1)
		asl
		asl
		asl
		asl
		sta $c001
		sta $d063										; ..xx.... high bits of CHRCOUNT

		lda #<rrbscreenwidth							; LINESTEPLSB - virtual row width - number of bytes to advance between each text row 
		ldx #>rrbscreenwidth
		sta $d058
		stx $d059

		lda #<COLOR_RAM_OFFSET							; set (offset!) pointer to colour ram
		sta $d064
		lda #>COLOR_RAM_OFFSET
		sta $d065

		lda $d070										; select mapped bank with the upper 2 bits of $d070
		and #%00111111
		sta $d070
		ldx #$00										; set bitmap palette
:		lda palette+$0000,x
		sta $d100,x
		lda palette+$0100,x
		sta $d200,x
		lda palette+$0200,x
		sta $d300,x
		inx
		bne :-
		lda $d070
		and #%11001111									; clear bits 4 and 5 (BTPALSEL) so bitmap uses palette 0
		sta $d070
		
		lda #$00										; turn off audio saturation
		sta $d712

		lda #<104										; pal y border start
		ldx #>104
		sta verticalcenter+0
		stx verticalcenter+1

		bit $d06f
		bpl pal
ntsc	lda #<55
		ldx #>55
		sta verticalcenter+0
		stx verticalcenter+1
pal		lda verticalcenter+0
		sta $d048
		lda #%00001111
		trb $d049
		lda verticalcenter+1
		tsb $d049

		ldx #0
:		lda gotox320charmemdata,x
		sta gotox320charmem,x
		lda regularcharmemdata,x
		sta regularcharmem,x
		inx
		cpx #64
		bne :-

		lda #<.loword(SAFE_COLOR_RAM)					; poke something into colour ram
		ldx #>.loword(SAFE_COLOR_RAM)
		ldy #<.hiword(SAFE_COLOR_RAM)
		ldz #>.hiword(SAFE_COLOR_RAM)
		sta zpcoldst+0
		stx zpcoldst+1
		sty zpcoldst+2
		stz zpcoldst+3

		ldz #0
		lda #%10001000
		sta [zpcoldst],z
		inz
		lda #%00001111
		sta [zpcoldst],z

		lda #$7f										; disable CIA interrupts
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
		sta $d01a

		lda #$fc; 32									; setup IRQ interrupt
		sta $d012
		lda #<irq1
		ldx #>irq1
		sta $fffe
		stx $ffff

		lda #$01										; ACK
		sta $d01a

		cli

loop
		lda $d020
		jmp loop

; ----------------------------------------------------------------------------------------------------

irq1
		pha

		inc framelo
;		lda framelo
;		bne :+
;		inc framehi
;:		lda framehi
;		cmp #1
;		bne :+
;		lda framelo
;		cmp #(320-256)
;		bne :+
;		lda #0
;		sta framelo
;		sta framehi
;:		

		; -----------------------------------------------

		lda #$02
		sta $d020

		jsr rrb_clearbuckets

		lda #spritesize ; actual chars, not NCM chars
		sta rrb_spr_width
		lda #11
		sta rrb_spr_height

		ldx #0

:		phx

		inc $d020

		lda sprdstposxlo,x
		clc
		txa
		asl
		asl
		asl
		adc framelo
		tay
		lda sine,y
		sta rrb_spr_dstx+0
		lda #0
		sta rrb_spr_dstx+1

		lda sprdstposy,x
		;lda #0
		;clc
		;txa
		;asl
		;asl
		;asl
		;adc framelo
		;adc #64
		;tay
		;lda sine,y
		;lsr
		;lsr
		;lsr
		;lsr
		;lsr
		sta rrb_spr_dsty

		txa
		clc
		adc framelo
		lsr
		and #$0f
		tax
		lda sprsrcposx,x
		sta rrb_spr_srcx
		lda sprsrcposy,x
		sta rrb_spr_srcy

		jsr rrb_drawbucketsprite

		plx
		inx
		cpx #maxsprites
		bne :-

		inc $d020

		jsr rrb_finalizebuckets

		lda #$01
		sta $d020

		; -----------------------------------------------

		pla
		asl $d019
		rti


verticalcenter	.word 0
framelo			.byte 0
framehi			.byte 0

sprdstposxlo	.repeat maxsprites, I
					.byte <(0+I*16)
				.endrepeat

sprdstposxhi	.repeat maxsprites, I
					.byte >(0+I*16)
				.endrepeat

sprdstposy		.repeat maxsprites, I
					.byte I/4
				.endrepeat

sprsrcposx		.repeat 2*16, I
					.byte (4*I).MOD 128
				.endrepeat

sprsrcposy		.repeat 2*16, I
					.byte 0
				.endrepeat

gotox320charmemdata
				.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
				.byte $ff,$ff,$ff,$66,$ff,$ff,$ff,$ff
				.byte $ff,$ff,$66,$66,$66,$ff,$ff,$ff
				.byte $ff,$66,$66,$66,$66,$66,$ff,$ff
				.byte $ff,$ff,$ff,$66,$ff,$ff,$ff,$ff
				.byte $ff,$ff,$ff,$66,$ff,$ff,$ff,$ff
				.byte $ff,$ff,$ff,$66,$ff,$ff,$ff,$ff
				.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

regularcharmemdata
				.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
				.byte $ff,$22,$22,$22,$22,$22,$22,$ff
				.byte $ff,$22,$ff,$ff,$ff,$ff,$22,$ff
				.byte $ff,$22,$ff,$ff,$ff,$ff,$22,$ff
				.byte $ff,$22,$ff,$ff,$ff,$ff,$22,$ff
				.byte $ff,$22,$ff,$ff,$ff,$ff,$22,$ff
				.byte $ff,$22,$22,$22,$22,$22,$22,$ff
				.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

;.segment "TABLES"

sine
.byte 255, 254, 254, 254, 254, 254, 253, 253, 252, 251, 251, 250, 249, 248, 247, 246, 245, 244, 242, 241, 240, 238, 236, 235, 233, 231, 230, 228, 226, 224, 222, 219
.byte 217, 215, 213, 210, 208, 206, 203, 201, 198, 195, 193, 190, 187, 185, 182, 179, 176, 173, 170, 167, 164, 161, 158, 155, 152, 149, 146, 143, 140, 137, 134, 131
.byte 128, 124, 121, 118, 115, 112, 109, 106, 103, 100, 097, 094, 091, 088, 085, 082, 079, 076, 073, 070, 068, 065, 062, 060, 057, 054, 052, 049, 047, 045, 042, 040
.byte 038, 036, 033, 031, 029, 027, 025, 024, 022, 020, 019, 017, 015, 014, 013, 011, 010, 009, 008, 007, 006, 005, 004, 004, 003, 002, 002, 001, 001, 001, 001, 001
.byte 001, 001, 001, 001, 001, 001, 002, 002, 003, 004, 004, 005, 006, 007, 008, 009, 010, 011, 013, 014, 015, 017, 019, 020, 022, 024, 025, 027, 029, 031, 033, 036
.byte 038, 040, 042, 045, 047, 049, 052, 054, 057, 060, 062, 065, 068, 070, 073, 076, 079, 082, 085, 088, 091, 094, 097, 100, 103, 106, 109, 112, 115, 118, 121, 124
.byte 127, 131, 134, 137, 140, 143, 146, 149, 152, 155, 158, 161, 164, 167, 170, 173, 176, 179, 182, 185, 187, 190, 193, 195, 198, 201, 203, 206, 208, 210, 213, 215
.byte 217, 219, 222, 224, 226, 228, 230, 231, 233, 235, 236, 238, 240, 241, 242, 244, 245, 246, 247, 248, 249, 250, 251, 251, 252, 253, 253, 254, 254, 254, 254, 254

.byte 254, 254, 254, 254, 254, 254, 253, 253, 252, 251, 251, 250, 249, 248, 247, 246, 245, 244, 242, 241, 240, 238, 236, 235, 233, 231, 230, 228, 226, 224, 222, 219
.byte 217, 215, 213, 210, 208, 206, 203, 201, 198, 195, 193, 190, 187, 185, 182, 179, 176, 173, 170, 167, 164, 161, 158, 155, 152, 149, 146, 143, 140, 137, 134, 131
.byte 128, 124, 121, 118, 115, 112, 109, 106, 103, 100, 097, 094, 091, 088, 085, 082, 079, 076, 073, 070, 068, 065, 062, 060, 057, 054, 052, 049, 047, 045, 042, 040
.byte 038, 036, 033, 031, 029, 027, 025, 024, 022, 020, 019, 017, 015, 014, 013, 011, 010, 009, 008, 007, 006, 005, 004, 004, 003, 002, 002, 001, 001, 001, 001, 001
.byte 001, 001, 001, 001, 001, 001, 002, 002, 003, 004, 004, 005, 006, 007, 008, 009, 010, 011, 013, 014, 015, 017, 019, 020, 022, 024, 025, 027, 029, 031, 033, 036
.byte 038, 040, 042, 045, 047, 049, 052, 054, 057, 060, 062, 065, 068, 070, 073, 076, 079, 082, 085, 088, 091, 094, 097, 100, 103, 106, 109, 112, 115, 118, 121, 124
.byte 127, 131, 134, 137, 140, 143, 146, 149, 152, 155, 158, 161, 164, 167, 170, 173, 176, 179, 182, 185, 187, 190, 193, 195, 198, 201, 203, 206, 208, 210, 213, 215
.byte 217, 219, 222, 224, 226, 228, 230, 231, 233, 235, 236, 238, 240, 241, 242, 244, 245, 246, 247, 248, 249, 250, 251, 251, 252, 253, 253, 254, 254, 254, 254, 254

; ----------------------------------------------------------------------------------------------------

clearcolorramjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, ((SAFE_COLOR_RAM) >> 20)				; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $02									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 11 byte DMA List structure starts here
				.byte %00000111									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word (rrbscreenwidth*rrbscreenheight)/2		; Count LSB + Count MSB

				.byte %00001000									; fill value (use 4 bits per pixel, which makes this an NCM char)
				.byte 0											; this is normally the high byte of the source addres (ignored for fill)
				.byte $00										; source bank (ignored)

				.word ((SAFE_COLOR_RAM) & $ffff)				; Destination Address LSB + Destination Address MSB
				.byte (((SAFE_COLOR_RAM) >> 16) & $0f)			; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

				.byte $00										; No more options
				.byte %00000011									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word (rrbscreenwidth*rrbscreenheight)/2		; Count LSB + Count MSB

				.byte %00001111									; fill value (set to $0f for transparency?)
				.byte 0											; this is normally the high byte of the source addres (ignored for fill)
				.byte $00										; source bank (ignored)

				.word (((SAFE_COLOR_RAM)+1) & $ffff)			; Destination Address LSB + Destination Address MSB
				.byte ((((SAFE_COLOR_RAM)+1) >> 16) & $0f)		; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

; -------------------------------------------------------------------------------------------------

clearscreenjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, ((screen) >> 20)						; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $02									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 11 byte DMA List structure starts here
				.byte %00000111									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word (rrbscreenwidth*rrbscreenheight)/2		; Count LSB + Count MSB

				.byte <(regularcharmem/64)										; fill value (low byte of char address)
				.byte 0											; this is normally the high byte of the source addres (ignored for fill)
				.byte $00										; source bank (ignored)

				.word ((screen) & $ffff)						; Destination Address LSB + Destination Address MSB
				.byte (((screen) >> 16) & $0f)					; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

				.byte $00										; No more options
				.byte %00000011									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word (rrbscreenwidth*rrbscreenheight)/2		; Count LSB + Count MSB

testme
				.byte >(regularcharmem/64)						; fill value (high byte of char address)
				.byte 0											; this is normally the high byte of the source addres (ignored for fill)
				.byte $00										; source bank (ignored)

				.word (((screen)+1) & $ffff)					; Destination Address LSB + Destination Address MSB
				.byte ((((screen)+1) >> 16) & $0f)				; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

; -------------------------------------------------------------------------------------------------
