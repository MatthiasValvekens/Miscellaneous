.nolist
#include "ti83plus.inc"
.list
.org $9D93
.db $BB,$6D

; (length) number
; length: sgn length (0..127)
	ld de,appbackupscreen
	ld hl,digit_test
	call make_digit_iterator
	ex de,hl
testloop:
	call callhl
	jr nz,testloop
	ret
callhl:
	jp (hl)
digit_test:
.db $02,$03,$05
make_digit_iterator:
	; make SMC iterator of (hl) at (de)
	; destroys bc
	push hl
	push de
	push hl
	push de
	;copy code to approprate location in RAM
	ld hl,digit_iterator_start
	ld bc,digit_iterator_end - digit_iterator_start
	ldir
	pop de
	pop hl
	ld a,(hl)
	and $7F
	inc de
	ld (de),a ;set length of number in SMC
	
	;set lengthbyte pointer in SMC
	push hl
	ld h,d
	ld l,e
	push de
	ld de,digit_iterator_lengthbyte_ptr-digit_iterator_start
	add hl,de ;+1 already accomplished by inc de earlier
	pop de
	ld (hl),e
	inc hl
	ld (hl),d
	pop hl
	ex de,hl
	push de
	ld de,digit_iterator_digit - digit_iterator_start
	add hl,de
	pop de
	;pointer to digit pointer now in hl. de contains addr of info byte
	inc de ;point to first digit
	ld (hl),e
	inc hl
	ld (hl),d
	ld d,h
	ld e,l
	dec de
	push de
	ld de,4
	add hl,de ;HARDCODED POINTER INCREMENT, BEWARE
	pop de
	;set digit pointer pointer
	ld (hl),e
	inc hl
	ld (hl),d
	pop de
	pop hl
	ret
	
digit_iterator_start:
	; will be loaded into an arbitrary location in memory
	; relevant pointers will be modified on load
	ld a,0 ;length
	or a
	ret z
	dec a
digit_iterator_lengthbyte_ptr:
	ld (0),a   ;modify self to decrement length byte 
	;(can't use relative addressing because we don't know where the code will be run)
	inc a ;res z? (todo: figure out the proper way to do this)
	push hl
digit_iterator_digit:
	ld hl,0
	ld a,(hl)
	inc hl ;this never sets z(assuming the digit is not stored at the top of RAM)
digit_iterator_DIGIT_POINTER:
	ld (0),hl
	pop hl
	ret
digit_iterator_end:

iterbuf1 equ tempswaparea
iterbuf2 equ tempswaparea+30
iter1:
	push hl
	ld hl,iterbuf1
	call callhl
	pop hl
	ret
iter2:
	push hl
	ld hl,iterbuf2
	call callhl
	pop hl
	ret
arb_raw_add:
	;adds ignoring sign
	;(hl) arg1
	;(de) arg2
	;(bc) dest, can be the same as either arg
	push bc
	push de	
	ld de,iterbuf1
	call make_digit_iterator
	pop hl
	ld de,iterbuf2
	call make_digit_iterator
	pop bc
	ld h,0 ;reset hl and d
	ld l,h
	ld d,h
	push bc
arb_raw_addloop:
	inc bc
	;carry in hl
	call iter1
	ld e,a
	add hl,de
	call iter2
	ld e,a
	add hl,de
	push hl
	ld a,l
	ld h,b
	ld l,c
	ld (hl),a
	pop hl
	ld l,h ;shift carry
	ld h,0
	;todo loop end condition
	jr arb_raw_addloop
	ld h,b
	ld l,c
	dec hl ;necessary?
	dec hl
	pop de ;pop old bc into de
	or a ;rcf (this shouldn't be necessary, but you never know)
	sbc hl,de
	ld a,l
	ld (de),a ;set length byte (sign ignored)
	ret
	
arb_sub:
	ret
	