#! /usr/bin/make -f
#
# @file   Makefile
# @author Ben^OVR
# @brief  Makefile for mfpcpu
#

override VERSION := 10

VASM = vasmm68k_mot -quiet -devpac -Ftos
VASM_FLAGS = -showcrit

HATARI = hatari
HATARI_OPTS  = --machine st --cpulevel 0 --cpuclock 8 --memsize 4
HATARI_OPTS += --monitor rgb --tos-res med
HATARI_OPTS += --confirm-quit no --gemdos-case lower

target = mfpcpu$(VERSION).prg

all: $(target)
clean: ; rm -f -- $(target)
PHONY: all clean
run: $(target);	$(HATARI) $(HATARI_OPTS) $<

$(target): mfpcpu.s 8x8.s aes_fsel.s
	$(VASM) $(VASM_FLAGS) -DVERSION=$(VERSION) -o $@ $<

INSTALLDIR = $(error INSTALLDIR must be defined)

install: $(target)
	test -d "$(INSTALLDIR)" || mkdir -pv -- "$(INSTALLDIR)"
	cp -v -- $< "$(INSTALLDIR)"/$(<F)
