.define COLOR_RAM $ff80000

.macro DMA_RUN_JOB jobPointer
		lda #(jobPointer & $ff0000) >> 16
		sta $d702
		sta $d704
		lda #>jobPointer
		sta $d701
		lda #<jobPointer
		sta $d705
.endmacro
