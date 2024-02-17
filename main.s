.feature pc_assignment
.feature labels_without_colons
.feature c_comments

; ----------------------------------------------------------------------------------------------------

COLOR_RAM				= $ff80000
screen					= $6000

zpcolsrc				= $f0
zpcoldst				= $f4
zpscrsrc				= $f8
zpscrdst				= $fc

chrcount				= 258							; set to something >256 actual chars, not NCM chars
rows			 		= 25

squarechar				= 256

squarecharmem			= squarechar*64					; $4000
gotox320charmem			= 320*64						; $5000

; ----------------------------------------------------------------------------------------------------

.macro set32bitzp address, zp
	lda #<.loword(address)
	ldx #>.loword(address)
	ldy #<.hiword(address)
	ldz #>.hiword(address)
	sta zp+0
	stx zp+1
	sty zp+2
	stz zp+3
.endmacro

.macro set16bitzp address, zp
	lda #<address
	sta zp+0
	lda #>address
	sta zp+1
.endmacro

.macro add16bitzp zp, add
	clc
	lda zp+0
	adc #<.loword(add)
	sta zp+0
	lda zp+1
	adc #>.loword(add)
	sta zp+1
.endmacro

.macro add32bitzp zp, add
	clc
	lda zp+0
	adc #<.loword(add)
	sta zp+0
	lda zp+1
	adc #>.loword(add)
	sta zp+1
	lda zp+2
	adc #<.hiword(add)
	sta zp+2
	lda zp+3
	adc #>.hiword(add)
	sta zp+3
.endmacro

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

		lda #$0b
		sta $d020
		lda #$00
		sta $d021

														; Enable double line RRB to double the time for RRB operations 
		lda #%00001000									; SET V400, CLEAR H640 FAST ATTR BPM H1280 MONO INT
		sta $d031
		lda #%01000000									; SET DBLRR, CLEAR DBLRR XPOSMSB
		sta $d051
		lda #$00    									; Set CHRYSCL = 0
		sta $d05b

		lda #%01000101 									; Enable 16 bit char numbers (bit 0) CHR16 and FCM for chars > $ff (bit2) FCLRHI, Remember to retain VFAST
		sta $d054

		lda #$50										; set TEXTXPOS to same as SDBDRWDLSB. -2 is 1 pixel, so -32 is 16 pixels
		sta $d04c

		lda #<.loword(screen)							; Set pointer to screen ram
		ldx #>.loword(screen)
		ldy #<.hiword(screen)
		ldz #>.hiword(screen)
		sta $d060
		stx $d061
		sty $d062
		stz $d063

		lda #<chrcount									; CHRCOUNT - Number of 16-bit characters to display per row
		sta $d05e										; display_row_width in VHDL
		lda #>chrcount
		asl
		asl
		asl
		asl
		sta $d063										; ..xx.... high bits of CHRCOUNT

		lda #<(chrcount*2)								; LINESTEPLSB - virtual row width - number of bytes to advance between each text row 
		ldx #>(chrcount*2)
		sta $d058
		stx $d059

		lda #0											; set offset into colour ram
		sta $d064
		sta $d065

		ldx #0											; create 2 different chars to show.
:		lda gotox320charmemdata,x						; 320 is shown as an arrow
		sta gotox320charmem,x
		lda squarecharmemdata,x							; $100 is shown as a square
		sta squarecharmem,x
		inx
		cpx #64
		bne :-

		; ------------------------------------------------------------------------------------------------------------------------
		; fill colour mem and screen mem to show the bug after $0100
		; ------------------------------------------------------------------------------------------------------------------------

		set32bitzp COLOR_RAM, zpcoldst					; set destination to colour ram
		set32bitzp screen, zpscrdst						; set destination to screen ram

		ldx #0											; x keeps track of row numbers

fillrowsloop		

		lda #0											; reset column fill counter
		sta columncountlo
		sta columncounthi

		set16bitzp colourrow, zpcolsrc					; set source to start of colour row data
		set16bitzp screenrow, zpscrsrc					; set source to start of colour row data

fillcolumnsloop

		ldz #0											; copy 2 bytes
		ldy #0
		lda (zpcolsrc),y
		sta [zpcoldst],z
		lda (zpscrsrc),y
		sta [zpscrdst],z
		inz
		iny
		lda (zpcolsrc),y
		sta [zpcoldst],z
		lda (zpscrsrc),y
		sta [zpscrdst],z

		add16bitzp zpcolsrc, 2
		add16bitzp zpscrsrc, 2
		add32bitzp zpcoldst, 2
		add32bitzp zpscrdst, 2

		inc columncountlo								; increase column count
		lda columncountlo
		bne :+
		inc columncounthi

:		lda columncountlo								; have we reached chrcount (x2) bytes copied?
		cmp #<chrcount
		bne :+
		lda columncounthi
		cmp #>chrcount
		bne :+
		jmp exitcolumnsfillloop

:		lbra fillcolumnsloop							; nope, continue copying bytes

exitcolumnsfillloop

		inx												; have we reached the last row to copy?
		cpx #rows
		lbne fillrowsloop								; nope, continue with next row

		; ------------------------------------------------------------------------------------------------------------------------

		; vertically flip the very first character to show that chargen is reading
		; colour/attribute data from $ff80000 instead of $ff80200

		set32bitzp COLOR_RAM, zpcoldst					

		ldz #0
		lda #%10001000
		sta [zpcoldst],z
		inz
		lda #%00001111
		sta [zpcoldst],z

		; ------------------------------------------------------------------------------------------------------------------------

		lda #$7f										; disable CIA interrupts
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
		sta $d01a

		lda #$ff										; setup IRQ interrupt
		sta $d012
		lda #<irq1
		ldx #>irq1
		sta $fffe
		stx $ffff

		lda #$01										; ACK
		sta $d01a

		cli

loop	jmp loop

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

; ----------------------------------------------------------------------------------------------------

gotox320charmemdata
				.byte $00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$77,$00,$00,$00,$00
				.byte $00,$00,$77,$77,$77,$00,$00,$00
				.byte $00,$77,$77,$77,$77,$77,$00,$00
				.byte $00,$00,$00,$77,$00,$00,$00,$00
				.byte $00,$00,$00,$77,$00,$00,$00,$00
				.byte $00,$00,$00,$77,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00

squarecharmemdata
				.byte $00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$22,$22,$22,$22,$22,$22,$00
				.byte $00,$22,$00,$00,$00,$00,$22,$00
				.byte $00,$22,$00,$00,$00,$00,$22,$00
				.byte $00,$22,$00,$00,$00,$00,$22,$00
				.byte $00,$22,$00,$00,$00,$00,$22,$00
				.byte $00,$22,$22,$22,$22,$22,$22,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00

; ----------------------------------------------------------------------------------------------------

colourrow

				.repeat 20
					.byte %00001000
					.byte %00001111
				.endrepeat

				.byte %10010000
				.byte %00001111

				.byte %00001000
				.byte %00001111

				.repeat (chrcount - 20 - 2)			; fill remaining with _gotox_ 320
					.byte %10010000
					.byte %00000000
				.endrepeat

screenrow

				.repeat 20
					.byte <squarechar
					.byte >squarechar
				.endrepeat

				.byte <320
				.byte >320

				.byte <squarechar
				.byte >squarechar

				.repeat (chrcount - 20 - 2)			; fill remaining with gotox _320_
					.byte <320
					.byte >320
				.endrepeat



columncountlo	.byte 0
columncounthi	.byte 0

; ----------------------------------------------------------------------------------------------------
