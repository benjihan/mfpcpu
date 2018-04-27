# Makefile for mfpcpu
#
# by Ben/OVR
#

override VERSION := 8

VASM = vasmm68k_mot -quiet -devpac -Ftos
VASM_FLAGS = -showcrit

target = mfpcpu$(VERSION).prg

all: $(target)
clean: ; rm -f -- $(target)
PHONY: all clean

$(target): mfpcpu.s 8x8.s aes_fsel.s
	$(VASM) $(VASM_FLAGS) -o $@ $<
