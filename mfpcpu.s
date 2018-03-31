;;; @file    mfpcpu.s
;;; @author  Ben/OVR
;;; @date    2016-09-29
;;; @brief   CPU/MFP clock ratio
;;; @version 1
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
	
	opt	o+,a+,w-

vblcount: equ $462

start:
	clr.l	-(a7)
	move	#32,-(a7)
	trap	#1
	addq	#6,a7
	move.l	d0,-(a7)
	
	move	sr,-(a7)
	move.w	#$2700,sr
	
	
	clr.b	$fffffa19.w	; stop timer-A
	clr.b	$fffffa1f.w	; 256 count down
	bset	#5,$fffffa07.w	; IER
	bset	#5,$fffffa13.w	; IMR
	move.l	$134.w,save134	;
	move.l	#timerA,$134.w	;
	
	move	(a7)+,sr
	clr.l	vblcount	; clear vbl count
	clr.l	mfpcount	; clear mfp count
	move.b	#7,$fffffa19.w	; 2457600/200/256 = 48.0hz


	bsr	reset_term
	bsr	refresh
			
loop:	
	tst.b	paused
	bne.s	.noupdate
	bsr	update
.noupdate:
	bsr	get_key
	cmp.b	#27,d0
	beq	exit
	cmp.b	#32,d0
	seq	d0
	eor.b	d0,paused
	bra	loop
	
exit:
	move	sr,-(a7)
	move	#$2700,sr
	clr.b	$fffffa19.w	; stop timer-A
	bclr	#5,$fffffa07.w	; IER
	bclr	#5,$fffffa13.w	; IMR
	move.l	save134,$134.w
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
	;; Check for user keyboard interrupt
	move	#$B,-(a7)
	trap	#1
	addq	#2,a7
	tst	d0
	beq.s	.nokey
	move	#$7,-(a7)
	trap	#1
	addq	#2,a7
.nokey:
	rts

;;; *******************************************************
;;; Display a zero-terminated string
;;;
;;;  Inp: 4(a7).l string 
;;;
psz_puts:
	move.l	d0,-(a7)
	move.l	8(a7),-(a7)
	move	#9,-(a7)
	trap	#1
	addq	#6,a7
	move.l	(a7)+,d0
	move.l	(a7)+,(a7)
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
;;; Reset terminal
;;;
reset_term:
	pea	psz_resetterm
	bsr	psz_puts
	rts	

;;; *******************************************************
;;; Refresh screen
;;;
refresh:
	pea	psz_refresh
	bsr	psz_puts
	rts

;;; *******************************************************
;;; Update screen
;;;
update:
	move.l	vblcount,d0
	move.l	mfpcount,d1
	
	lea	xvbl,a0
	bsr	atox
	exg.l	d0,d1
	lea	xmfp,a0
	bsr	atox

	pea	psz_update
	bsr	psz_puts
	rts
	

;;; *******************************************************
;;; Timer A irq
;;;
timerA:
	addq.l	#1,mfpcount
	move.b	#~$20,$fffffa0f.w	; ISR
	rte


;;; *******************************************************
;;; *******************************************************

	SECTION	DATA

;;; *******************************************************
;;; *******************************************************

save134:	dc.l	0
mfpcount:	dc.l	0
paused:		dc.b	0

psz_refresh:
	dc.b	27,"E"
	dc.b	"<ESC> to exit",10,13
	dc.b	"<SPC> to pause",10,13
	dc.b	10,13

psz_update:
	dc.b	13
	dc.b	"vbl: $"
xvbl:	dc.b	"xxxxxxxx"
	dc.b	" "
	dc.b	"mfp: $"
xmfp:	dc.b	"xxxxxxxx"
	dc.b	0

psz_resetterm:
	dc.b	27,"c0",27,"b3"	; black on white
	dc.b	27,"q"		; normal video
	dc.b	27,"f"		; show cursor
	dc.b	27,"v"		; Line wrap
	dc.b	27,"E"		; clear screen
	dc.b	27,"j"		; save position
	dc.b	0
