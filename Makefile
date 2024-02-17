# -----------------------------------------------------------------------------

megabuild		= 1
attachdebugger	= 0

# -----------------------------------------------------------------------------

MAKE			= make
CP				= cp
MV				= mv
RM				= rm -f
CAT				= cat

SRC_DIR			= ./src
UI_SRC_DIR		= ./src/ui
UIELT_SRC_DIR	= ./src/ui/uielements
DRVRS_SRC_DIR	= ./src/drivers
EXE_DIR			= ./exe
BIN_DIR			= ./bin

# mega65 fork of ca65: https://github.com/dillof/cc65
AS				= ca65mega
ASFLAGS			= -g -D megabuild=$(megabuild) --cpu 45GS02 -U --feature force_range -I ./exe
LD				= ld65
CC1541			= cc1541
GCC				= gcc
MEGAADDRESS		= megatool -a
MEGACRUNCH		= megatool -c
EL				= etherload
XMEGA65			= H:\xemu\xmega65.exe
MEGAFTP			= mega65_ftp -e

default: all

# -----------------------------------------------------------------------------

main.o:	main.s Makefile Linkfile
	$(AS) $(ASFLAGS) -o $@ $<

main.prg.addr.mc: main.o Linkfile
	$(LD) -C Linkfile -o main.prg main.o
	$(MEGAADDRESS) main.prg 00000800
	$(MEGACRUNCH) -e 00000800 main.prg.addr

rrbbug.d81: main.prg.addr.mc
	$(RM) $@
	$(CC1541) -n "rrb" -i " 2024" -d 19 -v\
	 \
	 -f "rrbbug" -w main.prg.addr.mc \
	$@

# -----------------------------------------------------------------------------

run: rrbbug.d81

ifeq ($(megabuild), 1)
	$(MEGAFTP) -c "put rrbbug.d81 rrbbug.d81" -c "quit"
	$(EL) -m RRBBUG.D81 -r main.prg.addr.mc
ifeq ($(attachdebugger), 1)
	m65dbg --device /dev/ttyS2
endif
else
	cmd.exe /c $(XMEGA65) -autoload -8 rrbbug.d81
endif

clean:
	$(RM) *.addr
	$(RM) *.addr.mc
	$(RM) *.prg
	$(RM) *.o
	$(RM) *.d81
