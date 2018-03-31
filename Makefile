# Makefile for mfpcpu
#
# by Ben/OVR
#

override VERSION := 1

VASM = vasmm68k_mot -quiet -devpac -Ftos
VASM_FLAGS = -showcrit

target = mfpcpu$(VERSION).tos

all: $(target)
clean: ; rm -f -- $(target)
PHONY: all clean

$(target): mfpcpu.s
	$(VASM) $(VASM_FLAGS) -o $@ $<
