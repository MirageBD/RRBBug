screen					= $06000						; currently $3000+ big
palette					= $00400

zpscrdst				= $f0
zpcoldst				= $f4

maxsprites				= 59							; 58 is right, 59 is wrong (with screenheight of 25)

rrbscreenwidth			= (2*(20+maxsprites*4+1+1)) ; actual chars, not NCM chars
rrbscreenheight 		= 25

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

		lda #$00
		sta $d020
		lda #$00
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
		stz $d063

		lda #<(20+maxsprites*4+1+1)						; CHRCOUNT - Number of 16-bit characters to display per row
		sta $d05e										; display_row_width in VHDL
		lda #>(20+maxsprites*4+1+1)
		asl
		asl
		asl
		asl
		sta $d063										; ..xx.... high bits of CHRCOUNT

		lda #<rrbscreenwidth							; LINESTEPLSB - virtual row width - number of bytes to advance between each text row 
		ldx #>rrbscreenwidth
		sta $d058
		stx $d059

		lda #<0											; set (offset!) pointer to colour ram
		sta $d064
		lda #>0
		sta $d065

		ldx #0
:		lda gotox320charmemdata,x
		sta gotox320charmem,x
		lda regularcharmemdata,x
		sta regularcharmem,x
		inx
		cpx #64
		bne :-

		lda #<.loword(COLOR_RAM)						; poke something into colour ram
		ldx #>.loword(COLOR_RAM)
		ldy #<.hiword(COLOR_RAM)
		ldz #>.hiword(COLOR_RAM)
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

		jsr rrb_clearbuckets
		jsr rrb_finalizebuckets

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

		inc $d020										; do something to show that IRQ is still running

		ldy #04
		ldx #0
:		dex
		bne :-
		dey
		bne :-

		dec $d020

		pla
		asl $d019
		rti

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


; ----------------------------------------------------------------------------------------------------

clearcolorramjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, ((COLOR_RAM) >> 20)					; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
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

				.word ((COLOR_RAM) & $ffff)						; Destination Address LSB + Destination Address MSB
				.byte (((COLOR_RAM) >> 16) & $0f)				; Destination Address BANK and FLAGS (copy to rbBaseMem)
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

				.word (((COLOR_RAM)+1) & $ffff)					; Destination Address LSB + Destination Address MSB
				.byte ((((COLOR_RAM)+1) >> 16) & $0f)			; Destination Address BANK and FLAGS (copy to rbBaseMem)
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
