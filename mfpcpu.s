;;; @file    mfpcpu.s
;;; @author  Benjamin Gerard AKA Ben/OVR
;;; @date    2018-04-09
;;; @brief   Estimate MFP-timers:CPU clock (VBL) ratio
;;;
;;; This is free and unencumbered software released into the public domain.
;;; For more information, please refer to <http://unlicense.org>

	ifnd VERSION
VERSION: set 9
	endc

	opt	o+,a+,w-

mfpctH equr d5
mfpctL equr d6
vblcnt equr d7
tdrreg equr a6
tmpreg equr a5

;;; ------------------------------

TD set 0	; TDR
TC set 1	; TCR

;;; ------------------------------

IRQA set 10	; First interrupt vector to save
IRQZ set 256	; Save up to this vector (exclusive)
NIRQ set IRQZ-IRQA

;;; *******************************************************

	section text

;;; *******************************************************

	bra.s	start
	dc.b	"*** "
	dc.b	"A simple program to compute "
	dc.b	"MFP-timers/CPU clock ratio "
	dc.b	"by using VBL synchro. "
	dc.b	"*** By Ben/OVR in 2018 ***"
	dc.b	0

	even
start:
	;; Basic GEMDOS setup
        move.l  4(a7),a0        ; Basepage
        lea     ustack(pc),a7   ; Private user stack
        move.l  $0c(a0),a1      ; TEXT size
        adda.w  #$100,a1        ; Basepage size
        adda.l  $14(a0),a1      ; DATA size
        adda.l  $1c(a0),a1      ; BSS size
        move.l  a1,-(a7)        ; Total size
        move.l  a0,-(a7)        ; Base address
        clr.w   -(a7)           ; Operand: 0=Release block
        move.w  #$4a,-(a7)      ; Mshrink
        trap    #1
        lea     12(a7),a7

	;; Init GEM/AES
	bsr	aes_init	; Setup GEM/AES 
	lea	omask(pc),a0	; "*.rec"
	bsr	aes_mask	; Setup fileselector mask

	;; Test compatible screen mode (PAL/medium)
	
	pea	getrez(pc)
	move.w	#$26,-(a7)	; superexec
	trap	#14
	addq.w	#6,a7
	cmp.w	#$0201,d0
	beq.s	ok_rez
	move.w	#2,ecode

	;; Alert USER
	moveq	#1,d0		; d0: default button index (1-based)
	lea	.alert(pc),a0	; a0: alert dialog creation text
	bsr	aes_alert
	bra	sysexit
.alert:
	dc.b	"[3]"		; STOP !
	dc.b	"[Invalid screen mode"
	dc.b	"| "
	dc.b	"|PAL Medium Rez required"
	dc.b	"| "
	dc.b	"|640x400x4 (50hz)]"
	dc.b	"[exit]",0
	even

getrez:
	move.w	$ffff820a.w,d0
	move.b	$ffff8260.w,d0
	and.w	#$0303,d0
	rts
	
ok_rez:
	move.w	#2,-(a7)
	trap	#14
	addq	#2,a7
	move.l	d0,phybase

	;; Hide mouse pointer
	dc.w	$A000 	; Line-A Init
	dc.w	$A00A	; Line-A Hidemouse

	clr.l	-(a7)
	move	#32,-(a7)
	trap	#1
	addq	#6,a7
	move.l	d0,saveusp
	move.l	a7,savessp

	;; Init records
	move.l	#record,_recW
	clr.l	nbrec

	;; Write help string on both plans
	bsr	cls
	clr.w	cursor
	move.b	#0,color
	pea	pszhlp
	bsr	psz_puts
	move.w	#$0001,cursor

	bsr	clear_acias

	move.w	#$2700,sr

	;; Save exception vectors
	lea	IRQA*4.w,a0
	lea	vectors(pc),a1
	lea	irqs(pc),a2
	move.w	#NIRQ-1,d0
save_vectors:
	move.l	(a0),(a1)+
	move.l	a2,(a0)+
	lea	(irqe-irqs)/NIRQ(a2),a2
	dbf	d0,save_vectors
	clr.w	numvec

	;; Install VBL
	move.l	#vblirq,$70.w

	;; Save MFP
	move.b	$fffffa07.w,savea07
	move.b	$fffffa09.w,savea09
	move.b	$fffffa17.w,savea17

	;; Cut all mfp interrupts
	clr.b	$fffffa07.w	; IERA
	clr.b	$fffffa09.w	; IERB
	bclr	#3,$fffffa17.w	; AEI

	clr.b	$fffffa19.w	; stop timer-A
	move.b	#TD,$fffffa1f.w ; count down
	bset	#5,$fffffa07.w	; IER
	bset	#5,$fffffa13.w	; IMR
	move.l	#timerA,$134.w	; Install timer-A vector

	move.b	#$12,d0		; Disable mouse
	bsr	put_ikbd

	move.b	#$15,d0		; Disable joystick
	bsr	put_ikbd

	moveq	#0,vblcnt
wait1:	stop	#$2300		; Allow VBL
	tst.w	vblcnt
	beq.s	wait1


;;; *******************************************************
;;; Main loop
;;;


loop:	stop	#$2300
	cmp.l	lastvbl,vblcnt
	beq.s	loop
	move.l	vblcnt,lastvbl

	;;;;;;;;;;;;;;;;;;;;;;;;;
	tst.b	lock		; locked ?
	beq.s	.notlock

	move.l	_mfp(pc),d2
	move.l	_vbl(pc),d3
	clr.b	lock

	;;;;;;;;;;;;;;;;;;;;;;;;;
	ifne	1
	move.l	_recW(pc),a0
	move.l	d2,(a0)+
	move.l	d3,(a0)+
	addq.l	#1,nbrec
	cmpa.l	#endrec,a0
	blo.s	.ok
	lea	record(pc),a0
.ok:
	move.l	a0,_recW
	endc

	rol.l	#8,d2
	moveq	#0,d1
	move.b	d2,d1
	clr.b	d2

	move.b	#2,color
	bsr	update
	addq.b	#1,cursory

	bra	.noupdate

	;;;;;;;;;;;;;;;;;;;;;;;;;

.notlock:
	tst.b	paused
	bne.s	.noupdate

	moveq	#0,d1
	move.l	mfpctL,d2
	rol.l	#8,d2
	move.b	d2,d1
	move.b	mfpctH,d2
	move.l	vblcnt,d3

	move.b	#3,color
	bsr	update

.noupdate:
	bsr	get_key
	cmp.b	#$44,d0		; <F10>
	beq	exit
	cmp.b	#$1C,d0		; <RETURN>
	beq	exit_nosave
	cmp.b	#$39,d0		; <SPACE>
	bne	loop
	not.b	paused
	bra	loop

;;; *******************************************************
;;; Exit
;;;

exit_nosave:
	clr.l	nbrec		; Will prevent saving below

exit:
	move	#$2700,sr
	clr.b	$fffffa19.w	; stop timer-A
	bclr	#5,$fffffa07.w	; IER
	bclr	#5,$fffffa13.w	; IMR
	move.b	savea17,$fffffa17.w
	move.b	savea09,$fffffa09.w
	move.b	savea07,$fffffa07.w
	move.w	#$fff,$FFFF8240.w

	;; Restore exception vectors
	lea	IRQA*4.w,a0
	lea	vectors(pc),a1
	move.w	#NIRQ-1,d0
rest_vectors:
	move.l	(a1)+,(a0)+
	dbf	d0,rest_vectors

	stop	#$2300

	bsr	clear_acias

	move.b	#$8,d0		; Enable mouse
	bsr	put_ikbd

	;; Back to usermode
	move.l	savessp(pc),a7
	move.l	saveusp(pc),-(a7)
	move	#32,-(a7)
	trap	#1
	addq	#6,a7

	;; Showmouse (Line-A)
	dc.w	$A009
	
	;;
	move.l	nbrec(pc),d0
	beq	.nosave

	bsr	aes_fsel
	tst.l	d0
	beq.s	.nosave

	clr.w	fhdl
	clr.w	-(a7)		; mode.w
	move.l	d0,-(a7)	; fpath.l
	;pea	oname(pc)	; fname.l
	move.w	#$3c,-(a7)	; Fcreate(fname.l,mode.w)
	trap	#1
	addq.w	#8,a7
	tst.w	d0
	ble.s	.nosave
	move.w	d0,fhdl

	;; Fwrite
	move.l	nbrec(pc),d0
	lsl.l	#3,d0
	move.l	#endrec-record,d1
	cmp.l	d1,d0
	bls.s	.ok
	move.l	d1,d0
.ok:

	pea	record(pc)	; adr.l
	move.l	d0,-(a7)	; cnt.l
	move.w	fhdl(pc),-(a7)	; hdl.w
	move.w	#$40,-(a7)	; Fwrite(hdl.w,cnt.l,adr.l)
	trap	#1
	lea	12(a7),a7

	;; Fclose
	move.w	fhdl(pc),-(a7)	; hdl.w
	move.w	#$3E,-(a7)	; Fclose(hdl.w)
	trap	#1
	addq.w	#4,a7
	clr.w	fhdl

.nosave:
	moveq	#0,d0
	move.w	numvec(pc),d0
	beq	sysexit

	lsl	#2,d0
	lea	pszvec(pc),a0
	bsr	ltox

	pea	pszirq(pc)
	move.w	#9,-(a7)
	trap	#1
	addq	#6,a7

waitesc:
	move	#$7,-(a7)
	trap	#1
	addq	#2,a7
	cmp.b	#27,d0
	bne	waitesc


sysexit:
	;; Pterm(ecode)
	move.w	ecode(pc),-(a7)
	move.w	#$4c,-(a7)
        trap	#1
	illegal


;;; *******************************************************
;;; Convert (mfp,vbl) to CPU freq decimal representation
;;;
;;; CPU= (VBL * $5bb3 << 22) / MFP (Overflow VBL>$b2abcd0
;;;
;;; d1.w mfpH
;;; d2.l mfpL
;;; d3.l vbl
;;; a0.l string

cpufrq:
	;; MUL #$5BB3
	;; ---------------------

	move.w	d3,d0
	mulu	#$5bb3,d0	; d0= d3.lo*F
	swap	d3
	mulu	#$5bb3,d3	; d3= d3.hi*F
	swap	d0
	add.w	d3,d0
	swap	d0
	clr.w	d3
	swap	d3
	moveq	#0,d4
	addx.w	d4,d3		; d3.w:d0=VBL*$5bb3

	;; LSL #22
	;; ---------------------
	;; Inp: d3= ..:AB
	;;	d0= ab:cd
	;; Out: d3= Ba:bc
	;;	d0= d.:..

	moveq	#63,d4		; d4= --:-X
	rol.l	#6,d0		; d0= bc:da
	lsl.w	#6,d3		; d3= .A:B.
	and.w	d0,d4		; d4= ..:.a
	or.w	d4,d3		; d3= .A:Ba
	swap	d3		; d3= Ba:.A
	eor.w	d4,d0		; d0= bc:d.
	swap	d0		; d0= d.:bc
	move.w	d0,d3		; d3= Ba:bc
	clr.w	d0		; d0= d.:..

	;; cmp.q \1:\2,\3:\4
cmpq:	macro
	cmp.l	\1,\3
	bne.s	.ok\@
	cmp.l	\2,\4
.ok\@:
	endm

	;; Add divider/2 for rounding
	move.l	d1,d4
	lsr.l	#1,d4
	move.l	d4,a1
	move.l	d2,d4
	roxr.l	#1,d4
	addx.l	d4,d0
	move.l	a1,d4
	addx.l	d4,d3

	lea	q10(pc),a1
	move.l	d3,a2

.lp10:	move.l	d1,(a1)+
	move.l	d2,(a1)+

	add.l	d2,d2
	addx.l	d1,d1		; x2
	move.l	d1,d3
	move.l	d2,d4
	add.l	d2,d2
	addx.l	d1,d1		; x4
	add.l	d2,d2
	addx.l	d1,d1		; x8
	add.l	d4,d2
	addx.l	d3,d1		; x10

	cmpq	a2,d0,d1,d2
	bls.s	.lp10

	move.l	a2,d3		; d3:d0= vbl*$5bb3<<22
	lea	q10(pc),a2

.digit:
	moveq	#"0",d4
	move.l	-(a1),d2	; d1:d2= mfp*10^n
	move.l	-(a1),d1

	sub.l	d2,d0
	subx.l	d1,d3
	bcs.s	.okdig
	;;
.inc:	addq.b	#1,d4
	sub.l	d2,d0
	subx.l	d1,d3
	bcc.s	.inc
	;;
.okdig: cmp.l	a2,a1
	beq.s	.done
	;;
	add.l	d2,d0
	addx.l	d1,d3
	move.b	d4,(a0)+
	bra.s	.digit

.done:	move.b	d4,(a0)+

	rts

;;; *******************************************************
;;; Update counters
;;;
;;; d1.w mfpH
;;; d2.l mfpL
;;; d3.l vbl

update:
	moveq	#0,d0
	move.w	d1,d0
	lea	mfptxt-4,a0
	bsr	htox

	move.l	d2,d0
	lea	mfptxt,a0
	bsr	ltox

	move.l	d3,d0
	lea	vbltxt,a0
	bsr	ltox

	lea	divtxt(pc),a0
	bsr	cpufrq
	clr.b	(a0)

	clr.b	cursorx		; <CR>
	pea	psztxt
	bsr	psz_puts

	rts


;;; *******************************************************
;;; Write a command to ikbd
;;;
;;;  Inp: d0.b command to write
;;;
put_ikbd:
	btst	#1,$fffffc00.w
	beq.s	put_ikbd
	move.b	d0,$fffffc02.w
	rts

;;; *******************************************************
;;; Reset keyboard
;;;
clear_acias:
	moveq	#$13,d0		; disable transfert
	bsr	put_ikbd

.flushing:
	moveq	#$a1,d0		; 1010 0001
	and.b	$fffffc00.w,d0
	beq.s	.flushed
	move.b	$fffffc02.w,d0
	bra.s	.flushing
.flushed:
	moveq	#$11,d0		; enable transfert
	bsr	put_ikbd
	rts

;;; *******************************************************
;;; Get released key scan
;;;
;;;  Out: d0=char (0:none)
;;;
get_key:
	btst	#1,$fffffc00.w
	beq.s	.nokey
	moveq	#0,d0
	move.b	$fffffc02.w,d0
	bclr	#7,d0
	beq.s	.keypress
.keyrelease:
	cmp.b	curkey,d0
	bne.s	.nokey
	clr.b	curkey
	rts
.keypress:
	move.b	d0,curkey
.nokey:
	moveq	#0,d0
	rts

;;; *******************************************************
;;; Clear screen and reset cursor position
;;;
cls:
	movem.l d0-d1/a0,-(a7)
	move.l	phybase,a0
	moveq	#0,d0
	move.w	d0,cursor
	move.w	#32000/4/8-1,d1
.cls:
	REPT	8
	move.l	d0,(a0)+
	ENDR
	dbf	d1,.cls
	movem.l (a7)+,d0-d1/a0
	rts

;;; *******************************************************
;;; Display a zero-terminated string
;;;
;;;  Inp: 4(a7).l string
;;;  Use: d0/d1/a0/a1/a2

psz_puts:
	move.l	4(a7),a2
.next:
	move.b	(a2)+,d0
	beq.s	.over
	bsr	putc
	bra.s	.next
.over:
	move.l	(a7)+,(a7)
	rts

putc:
	moveq	#0,d1
	tst.b	d0
	spl	d1
	and	d1,d0
	sub.b	#32,d0
	spl	d1
	and	d1,d0
	lsl	#3,d0
	lea	font(pc),a0
	adda	d0,a0

	move.l	phybase,a1

	; Ensure cursor is inside screen
	move.b	cursorx,d0
	move.b	cursory,d1
	cmp.b	#80,d0
	blo.s	.okx
	moveq	#0,d0
	addq.b	#1,d1
.okx:
	cmp.b	#25,d1
	blo.s	.oky
	moveq	#1,d1
.oky:
	move.b	d0,cursorx
	move.b	d1,cursory

	;; cursor -> screen address
	moveq	#0,d0
	move.b	cursorx,d0
	moveq	#1,d1
	and.w	d0,d1
	adda.w	d1,a1
	sub.w	d1,d0	; x2
	add.w	d0,d0	; x4
	adda.w	d0,a1
	move.b	cursory,d1
	lsl.w	#5,d1	; x32
	move.w	d1,d0
	add.w	d0,d0	; x64
	add.w	d0,d0	; x128
	add.w	d0,d1	; x160
	lsl.w	#3,d1	; x1280
	adda.w	d1,a1	; a1: screen address

	;; Update cursor
	addq.b	#1,cursorx

	moveq	#0,d0
	moveq	#0,d1
	move.b	color(pc),d1
	add.w	d1,d1
	add.w	d1,d1
	jmp	.tjmp(pc,d1.w)
.tjmp:	bra.w	.col0
	bra.w	.col1
	bra.w	.col2
	bra.w	.col3

	;; -------------
N SET 0
.col0:
	REPT	8
	move.b	(a0)+,d1
	not.b	d1
	move.b	d1,N+0(a1)
	move.b	d1,N+2(a1)
N SET N+160
	ENDR
	rts

	;; -------------
N SET 0
.col1:
	REPT	8
	move.b	d0,N+0(a1)
	move.b	(a0)+,N+2(a1)
N SET N+160
	ENDR
	rts

	;; -------------
N SET 0
.col2:
	REPT	8
	move.b	(a0)+,N+0(a1)
	move.b	d0,N+2(a1)
N SET N+160
	ENDR
	rts

	;; -------------
N SET 0
.col3:
	REPT	8
	move.b	(a0)+,d1
	move.b	d1,N+0(a1)
	move.b	d1,N+2(a1)
N SET N+160
	ENDR
	rts

;;; *******************************************************
;;; Convert 16/32bit unsigned integer to string
;;;
;;;  Inp: d0=int, a0=buffer (8 bytes)
;;;  Out: a0=end of string
;;;
htox:
	move.l	d1,-(a7)
	move.l	d2,-(a7)
	moveq	#3,d1
.loop:
	rol.w	#4,d0
	moveq	#15,d2
	and	d0,d2
	move.b	thex(pc,d2.w),(a0)+
	dbf	d1,.loop
	move.l	(a7)+,d2
	move.l	(a7)+,d1
	rts

ltox:
	move.l	d1,-(a7)
	move.l	d2,-(a7)
	moveq	#7,d1
.loop:
	rol.l	#4,d0
	moveq	#15,d2
	and	d0,d2
	move.b	thex(pc,d2.w),(a0)+
	dbf	d1,.loop
	move.l	(a7)+,d2
	move.l	(a7)+,d1
	rts

thex:	dc.b	"0123456789ABCDEF"


;;; *******************************************************
;;; VBL irqs
;;;

;;; 1st VBL init registers, install second
;;;
vblirq:
	;; Prepare timer start
	movea.w #$fa19,tdrreg	; TCR for 2nd VBL
	moveq	#TC,mfpctL	; TC  for 2nd VBL
	move.l	#vblirq2,$70.w	; install new VBL
	rte

;;; 1st VBL init counters and records before starting the timer
;;;
vblirq2:
	moveq	#0,vblcnt	; clear VBL counter
	move.b	mfpctL,(tdrreg) ; start timer
	moveq	#0,mfpctL	; clear MFP counter
	movea.w #$fa1f,tdrreg	; timer-A data register
	move.l	#vblirq3,$70.w	; install new VBL
	rte

vblirq3:
	move.l	mfpctL,tmpreg	; save MFP counter
	move.b	(tdrreg),mfpctH ; read timer count
	cmp.w	tmpreg,mfpctL	; timer interrupted ?
	bne.s	.insync

	neg.b	mfpctH		; count-down to count-up
	addq.l	#1,vblcnt	; update VBL counter
	rte

.insync:
	;; At this point we assume the counters are:
	;; vbl: vblcount+1
	;; mfp: tmpreg+1

	addq.l	#1,vblcnt	; update VBL counter
	addq.l	#1,tmpreg	; update MFP counter

	tas	lock		; try to lock
	bne.s	.cantlock	; Ooopsy daisy

	move.l	vblcnt,_vbl
	move.l	tmpreg,_mfp

.cantlock:
	moveq.l #0,mfpctH
	rte

;;; *******************************************************
;;;

timerA:
	addq.l	#1,mfpctL
	rte

;;; *******************************************************
;;;
;;;
declirq macro
	move.w	#\1,numvec
	move.l	#exit_nosave,2(a7)
	rte
	endm

irqs:
I	set	IRQA
	rept	NIRQ
	declirq I
I	set	I+1
	endr
irqe:

;;; *******************************************************

	SECTION DATA

;;; *******************************************************

font:	include "8x8.s"

pszirq: dc.b 27,'E'
	dc.b "Unexpected interruption @$"
pszvec: dc.b "00000000",10,13
	dc.b "Press <ESC> to exit",0
pszhlp: dc.b "V","0"+VERSION," | "
	dc.b "<SPC> pause | <F10> Save&Exit | <RET> Exit",0
psztxt: dc.b "mfp:$0000"
mfptxt: dc.b "00000000 vbl:$"
vbltxt: dc.b "00000000 cpu:"
divtxt: ds.b 32

oname:	dc.b "mfpvblv","0"+VERSION,".rec",0
omask:	dc.b "*.rec",0

	even
	include "aes_fsel.s"

;;; *******************************************************

	SECTION BSS

;;; *******************************************************

	even
cursor:
cursorx:	ds.b 1
cursory:	ds.b 1
color:		ds.b 1

	even
_recW:		ds.l 1
nbrec:		ds.l 1

	even
lock:		ds.w 1
_vbl:		ds.l 1
_mfp:		ds.l 1

	even
q10:		ds.l 2*16

	even
ecode:		ds.w 1
srez:		ds.w 1
lastvbl:	ds.l 1
phybase:	ds.l 1
fhdl:		ds.w 1
numvec:		ds.w 1
paused:		ds.b 1
savea07:	ds.b 1
savea09:	ds.b 1
savea17:	ds.b 1
curkey:		ds.b 1

	even
		ds.l 256
ustack:		ds.l 1

savessp:	ds.l 1
saveusp:	ds.l 1
vectors:	ds.l NIRQ

	even
record:		ds.b 3<<17	; 384Kb should be more than enough
endrec:		ds.l 4		; a bit more
