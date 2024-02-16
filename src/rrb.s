; -------------------------------------------------------------------------------------------------

bucketsize		= (2*(maxsprites*4+1+1)) ; 20; actual chars, not NCM chars
spriteoffset	= (2*(20)) ; actual chars, not NCM chars
screensprites	= (screen+spriteoffset)
coloursprites	= (SAFE_COLOR_RAM+spriteoffset)

rrb_finalizebuckets

		; MOVE THIS SOMEWHERE - SHOULD ALWAYS BE CONSTANT???
		lda #<.hiword(coloursprites)					; set high bits of colour destination pointer
		sta zpcoldst+2
		lda #>.hiword(coloursprites)
		sta zpcoldst+3

		ldx #$00

:		clc
		lda rrb_screenbucketstartlo,x					; get EOL
		adc rrb_bucketpos,x
		sta zpscrdst+0
		lda rrb_screenbucketstarthi,x
		adc #0
		sta zpscrdst+1

		clc
		lda rrb_colourbucketstartlo,x
		adc rrb_bucketpos,x
		sta zpcoldst+0
		lda rrb_colourbucketstarthi,x
		adc #0
		sta zpcoldst+1

		ldz #0
		ldy #0
		lda #%10010000									; finalize with a gotox (can remove transparency bit)
		sta [zpcoldst],z
		inz
		lda #%00001111
		sta [zpcoldst],z
		inz
		lda #<320
		sta (zpscrdst),y
		iny
		lda #>320
		sta (zpscrdst),y
		iny

		lda #%00001000									; draw one final char (clear bit4 = goto, set bit3 = NCM)
		sta [zpcoldst],z
		inz
		lda #%00001111
		sta [zpcoldst],z
		lda #$00
		sta (zpscrdst),y
		iny
		lda #$04
		sta (zpscrdst),y

		inx
		cpx #rrbscreenheight
		lbne :-

		rts

; -------------------------------------------------------------------------------------------------

rrb_clearbuckets

		DMA_RUN_JOB rrb_clearbucketindices

		ldx #0
:
		lda rrb_colourbucketstartlo,x								; set up colour bucket dest
		sta rrbcb_coldst1+0
		lda rrb_colourbucketstarthi,x
		sta rrbcb_coldst1+1

		clc
		lda rrb_colourbucketstartlo,x
		adc #1
		sta rrbcb_coldst2+0
		lda rrb_colourbucketstarthi,x
		adc #0
		sta rrbcb_coldst2+1

		lda rrb_screenbucketstartlo,x								; set up screen bucket dest
		sta rrbcb_scrdst1+0
		lda rrb_screenbucketstarthi,x
		sta rrbcb_scrdst1+1

		clc
		lda rrb_screenbucketstartlo,x
		adc #1
		sta rrbcb_scrdst2+0
		lda rrb_screenbucketstarthi,x
		adc #0
		sta rrbcb_scrdst2+1

		DMA_RUN_JOB rrb_clearbucket

		inx
		cpx #rrbscreenheight
		bne :-
		
		rts

; -------------------------------------------------------------------------------------------------

rrb_clearbucket

				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $81, ((screensprites) >> 20)				; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
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

				.word bucketsize/2								; Count LSB + Count MSB

				.byte <320										; fill value (low byte of char address)
				.byte 0											; this is normally the high byte of the source addres (ignored for fill)
				.byte $00										; source bank (ignored)

rrbcb_scrdst1
				.word ((screensprites) & $ffff)					; Destination Address LSB + Destination Address MSB
				.byte (((screensprites) >> 16) & $0f)			; Destination Address BANK and FLAGS
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

				; -----------------------------------------------------

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

				.word bucketsize/2								; Count LSB + Count MSB

				.byte >320										; fill value (low byte of char address)
				.byte 0											; this is normally the high byte of the source addres (ignored for fill)
				.byte $00										; source bank (ignored)

rrbcb_scrdst2
				.word ((screensprites+1) & $ffff)				; Destination Address LSB + Destination Address MSB
				.byte (((screensprites+1) >> 16) & $0f)			; Destination Address BANK and FLAGS
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

				; -----------------------------------------------------

				.byte $81, ((coloursprites) >> 20)				; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
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

				.word bucketsize/2								; Count LSB + Count MSB

				.byte %10010000									; fill value (gotox)
				.byte 0											; this is normally the high byte of the source addres (ignored for fill)
				.byte $00										; source bank (ignored)

rrbcb_coldst1
				.word ((coloursprites) & $ffff)					; Destination Address LSB + Destination Address MSB
				.byte (((coloursprites) >> 16) & $0f)			; Destination Address BANK and FLAGS
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

				; -----------------------------------------------------

				.byte $00										; No more options

																; 11 byte DMA List structure starts here
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

				.word bucketsize/2								; Count LSB + Count MSB

				.byte %00000000									; fill value (Pixel row mask flags. Not used when bit 3 of previous char is not 0)
				.byte 0											; this is normally the high byte of the source addres (ignored for fill)
				.byte $00										; source bank (ignored)

rrbcb_coldst2
				.word ((coloursprites+1) & $ffff)				; Destination Address LSB + Destination Address MSB
				.byte (((coloursprites+1) >> 16) & $0f)			; Destination Address BANK and FLAGS
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

; -------------------------------------------------------------------------------------------------

rrb_clearbucketindices

				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, ((rrb_bucketpos) >> 20)				; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $01									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 11 byte DMA List structure starts here
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

				.word rrbscreenheight							; Count LSB + Count MSB

				.byte $00										; fill value (low byte of char address)
				.byte 0											; this is normally the high byte of the source addres (ignored for fill)
				.byte $00										; source bank (ignored)

				.word ((rrb_bucketpos) & $ffff)					; Destination Address LSB + Destination Address MSB
				.byte (((rrb_bucketpos) >> 16) & $0f)			; Destination Address BANK and FLAGS
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

; -------------------------------------------------------------------------------------------------

.segment "BUCKETS"

rrb_bucketpos
		.repeat rrbscreenheight
			.byte 0
		.endrepeat

rrb_screenbucketstartlo
		.repeat rrbscreenheight, I
			.byte <(screensprites+I*rrbscreenwidth)
		.endrepeat

rrb_screenbucketstarthi
		.repeat rrbscreenheight, I
			.byte >(screensprites+I*rrbscreenwidth)
		.endrepeat

rrb_colourbucketstartlo
		.repeat rrbscreenheight, I
			.byte <(coloursprites+I*rrbscreenwidth)
		.endrepeat

rrb_colourbucketstarthi
		.repeat rrbscreenheight, I
			.byte >(coloursprites+I*rrbscreenwidth)
		.endrepeat

; -------------------------------------------------------------------------------------------------
