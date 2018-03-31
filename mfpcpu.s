;;; @file    mfpcpu.s
;;; @author  Ben/OVR
;;; @date    2017-05-03
;;; @brief   CPU/MFP clock ratio
;;; @version 2
;;;
;;; -----------------------------------------------------------------------
;;; 
;;; This is free and unencumbered software released into the public domain.
;;; 
;;; Anyone is free to copy, modify, publish, use, compile, sell, or
;;; distribute this software, either in source code form or as a compiled
;;; binary, for any purpose, commercial or non-commercial, and by any
;;; means.
;;; 
;;; In jurisdictions that recognize copyright laws, the author or authors
;;; of this software dedicate any and all copyright interest in the
;;; software to the public domain. We make this dedication for the benefit
;;; of the public at large and to the detriment of our heirs and
;;; successors. We intend this dedication to be an overt act of
;;; relinquishment in perpetuity of all present and future rights to this
;;; software under copyright law.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;; 
;;; For more information, please refer to <http://unlicense.org/>

mfptmp 		equr	d5
vblreg 		equr	d6
mfpreg 		equr	d7

TF set 8
TD set (1<<TF)&255	; TDR 
TC set 7		; TCR
	
	opt	o+,a+,w-

start:
	move.w	#2,-(a7)
	trap	#14
	addq	#2,a7
	move.l	d0,phybase
	bsr	cls
	
	clr.l	-(a7)
	move	#32,-(a7)
	trap	#1
	addq	#6,a7
	move.l	d0,-(a7)
	
	move	sr,-(a7)
	move.w	#$2700,sr
	
	;; Save VBL and install
	move.l	$70.w,save070
	move.l	#vblirq,$70.w
	
	;; Save MFP
	move.b	$fffffa07.w,savea07
	move.b	$fffffa09.w,savea09
	move.b	$fffffa17.w,savea17
	
	;; Cut all mfp interrupts
	clr.b	$fffffa07.w	; IERA
	clr.b	$fffffa09.w	; IERB
	bclr	#3,$fffffa17.w	;
	
	clr.b	$fffffa19.w	; stop timer-A
	move.b	#TD,$fffffa1f.w	; count down
	bset	#5,$fffffa07.w	; IER
	bset	#5,$fffffa13.w	; IMR
	move.l	$134.w,save134	;
	move.l	#timerA,$134.w	;
	
	move	(a7)+,sr
	
	moveq	#0,vblreg	; clear VBL 
	moveq	#0,mfpreg	; clear MFP 
	
wait1:	stop	#$2300
	tst.l	vblreg
	beq.s	wait1
			
loop:	
	tst.b	paused
	bne.s	.noupdate
	
	bsr	update

.noupdate:
	tst.b	synced		; This critical section
	beq.s	.notsync	; is not completly safe.
	move.l	mfpsyn,d3	; Just get the value as fast 
	move.l	vblsyn,d4	; as possible
	move.l	divsyn,d0	; as possible
	sf	synced

	lea	divtxt,a0
	bsr	atox

	move.l	d3,d0
	lea	mfptxt,a0
	bsr	atox

	move.l	d4,d0
	lea	vbltxt,a0
	bsr	atox
	
	addq.l	#2,phybase
	move.b	#41,cursorx
	pea	psztxt
	bsr	psz_puts
	subq.l	#2,phybase
	clr.b	cursorx
	addq.b	#1,cursory

.notsync:
	bsr	get_key
	cmp.b	#$39,d0
	beq	exit
	cmp.b	#$00,d0
	seq	d0
	eor.b	d0,paused
	bra	loop
	
exit:
	move	sr,-(a7)
	;; ----------------	
	move	#$2700,sr
	clr.b	$fffffa19.w	; stop timer-A
	bclr	#5,$fffffa07.w	; IER
	bclr	#5,$fffffa13.w	; IMR
	move.l	save070,$70.w	; VBL vector
	move.l	save134,$134.w	
	move.b	savea17,$fffffa17.w
	move.b	savea09,$fffffa09
	move.b	savea07,$fffffa07
	;; ----------------	
	move	sr,-(a7)

	;; Back to usermode
	move	#32,-(a7)
	trap	#1
	addq	#6,a7
	;; Exit
	clr.w	-(a7)
	trap	#1
	illegal

;;; *******************************************************
;;; Read stdin if a char is available
;;;
;;;  Out: d0=char (0:none)
;;;
get_key:
	moveq	#0,d0
;	btst	#0,$fffffc00.w
;	beq.s	.nokey
	move.b	$fffffc02.w,d0
	bmi.s	.nokey
.keypress:
	cmp.b	curkey,d0
	beq.s	.nokey
	move.b	d0,curkey
	rts
.nokey:
	moveq	#0,d0
	rts

;;; *******************************************************
;;; Clear screen and reset cursor position
;;;
cls:
	movem.l	d0-d1/a0,-(a7)
	move.l	phybase,a0
	moveq	#0,d0
	move.w	d0,cursor
	move.w	#32000/32-1,d1
.cls:	
	REPT	8
	move.l	d0,(a0)+
	ENDR
	dbf	d1,.cls
	
	movem.l	(a7)+,d0-d1/a0
	rts

;;; *******************************************************
;;; Display a zero-terminated string
;;;
;;;  Inp: 4(a7).l string 
;;;
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
	addq	#1,d1
.okx:	
	cmp.b	#25,d1
	blo.s	.oky
	moveq	#0,d1
.oky:
	move.b	d0,cursorx
	move.b	d1,cursory	

	;; cursor screen address	
	moveq	#0,d0
	move.b	cursorx,d0
	moveq	#1,d1
	and.w	d0,d1
	adda	d1,a1
	sub	d1,d0	; x2
	add	d0,d0	; x4
	adda	d0,a1
	move.b	cursory,d1
	lsl	#5,d1	; x32
	move.w	d1,d0
	add	d0,d0	; x64
	add	d0,d0	; x128
	add	d0,d1	; x160
	lsl	#3,d1	; x1280
	adda	d1,a1
N SET 0
	REPT	8
	move.b	(a0)+,N(a1)
N SET N+160
	ENDR
		
	;; Update cursor
	addq.b	#1,cursorx

	rts

;;; *******************************************************
;;; Convert 32bit unsigned integer to string
;;;
;;;  Inp: d0=int, a0=buffer (8 bytes) 
;;;  Out: a0=end of string 
;;;
atox:
	move.l	d1,-(a7)
	move.l	d2,-(a7)
	moveq	#7,d1
.loop:
	rol.l	#4,d0
	moveq	#15,d2
	and	d0,d2
	move.b	.thex(pc,d2.w),(a0)+
	dbf	d1,.loop
	move.l	(a7)+,d2
	move.l	(a7)+,d1
	rts
	
.thex:	dc.b	"0123456789ABCDEF"


;;; *******************************************************
;;; Refresh screen
;;;
refresh:
	rts

;;; *******************************************************
;;; Update screen
;;;
update:
	move.l	mfpreg,d0
	lea	mfptxt,a0
	bsr	atox

	move.l	vblreg,d0
	lea	vbltxt,a0
	bsr	atox
	
	clr.b	cursorx
	pea	psztxt
	bsr	psz_puts
	
	rts

;;; *******************************************************
;;; VBL irq
;;;
vblirq:
	moveq	#0,mfpreg	; reset MFP counter (d7)
	move.b	#TC,$fffffa19.w	; start timer-A
	moveq	#-1,vblreg	; reset VBL counter (d6)
	move.l	#vblirq2,$70.w	; setup next VBL
vblirq2:
	move.l	mfpreg,mfptmp	; store temporary MFP counter
	addq.l	#1,vblreg	; increments VBL counter
	cmp.l	mfpreg,mfptmp
	beq.s	.ignore
	move.l	mfpreg,mfptmp	; fast save the MFP counter
	tas.b	synced		; critical section
	bne.s	.lost		; previous value not retrieved
	move.l	mfptmp,mfpsyn
	move.l	vblreg,vblsyn
	lsl.l	#TF,mfptmp
	divu	vblreg,mfptmp
	move.l	mfptmp,divsyn
	
.ignore:	
	rte
.lost:
	addq.l	#1,synlost
	eor	#$300,$ffff8240.w
	rte

;;; *******************************************************
;;; Timer-A irq
;;;
timerA:
	addq.l	#1,mfpreg	; increments MFP counter
	;not.w	$ffff8240.w
	rte


;;; *******************************************************
;;; *******************************************************

	SECTION	DATA

;;; *******************************************************
;;; *******************************************************

font:	include "8x8.s"

psztxt:	dc.b "mfp:$"
mfptxt:	dc.b "00000000 vbl:$"
vbltxt:	dc.b "00000000 /$"
divtxt:	dc.b "00000000",0

	even
cursor:
cursorx:	dc.b	0
cursory:	dc.b	0

	even
vblsyn:		dc.l 0
mfpsyn:		dc.l 0
divsyn:		dc.l 0
synlost:	dc.l 0
synced:		dc.w 0

	even
phybase:	dc.l	0
save070:	dc.l	0
save134:	dc.l	0
mfpcount:	dc.l	0
paused:		dc.b	0
savea07:	dc.b	0
savea09:	dc.b	0
savea17:	dc.b	0
curkey:		dc.b	0
