.org $9d93
.db $bb,$6d
	ld hl,$0506
	ld a,2
	call linear_congruence_solve
	ld hl,-1
	ld a,5
	call hlmoda
	ret

hlmoda:
	;compute hl mod a -> hl
	;destroys none
	;FIXME: negative numbers?
	push af
	push de
	ld d,a
	call divhld
	ld h,0
	ld l,a
	pop de
	pop af
	ret
ensure_mod:
	;reduces hl and de to their mod a
	call hlmoda
	ex de,hl
	call hlmoda
	ex de,hl
	ret
safe_modadd:
	;add hl+de mod a, compute modulo of args before evaluating
	call ensure_mod
modadd:
	;add hl+de mod a -> hl
	;destroys none
	add hl, de
	call hlmoda
	ret

egcd:
	; compute the gcd of h and l
	; destroys all non-shadow
	; gcd in h
	; bezout coefficients in b and d
	ld bc,$100
	ld de,1
egcd_loop:
	;r-pair: hl
	;s-pair: de
	;t-pair: bc
	ld a,l
	or a ;check remainder zero
	ret z
	push de
	push hl	
	ld d,l
	ld l,h
	ld h,0 ;no 16-bit shift instructions
	call divhld ;old_r/r-> h/l
	ex de,hl
	pop hl ;quot in de, remainder in a
	ld h,l
	ld l,a ;(old_r,r):=(r, old_r - quot*r)
	ex de,hl ;quot in hl, new r-pair in de
	ld a,l ;quotient is guaranteed to be 8-bit, discard remainder
	pop hl ;pop (old_s, s) from stack into hl
	push de ;save new r-pair on stack
	;compute new s-pair
	push bc
	push hl
	ld h,l 
	ld e,a
	call htimese ;quotient*s
	ex de,hl ;product in de
	pop hl
	push af
	ld a,h
	sub e ;subtract quot*s from old_s
	ld h,l ;save new s-pair to hl
	ld l,a
	pop af ;recover quotient
	pop bc ;recover old t-pair
	push hl ;save new s-pair on stack
	;compute new t-pair
	push bc
	ld h,c
	ld e,a
	call htimese
	ex de,hl ;product in de
	pop hl ;old t-pair in hl
	ld a,h
	sub e
	ld b,l ;save new t-pair to bc
	ld c,a
	pop de ;pop new s-pair into de
	pop hl ;pop new r-pair into hl
	jr egcd_loop
linear_congruence_solve:
	;solve the congruence h x = a mod l
	;result in hl
	push af
	call egcd
	pop af
	ld e,b ;h's bezout coefficient -> e
	ld d,h ; gcd -> d
	ld h,a
	push de
	call htimese
	pop de
	ld h,0
	call divhld
	ret
mod_mul:
	;compute h * e mod a -> hl
	push bc
	call htimese
	call hlmoda
	pop bc
	ret

htimese:
;destroys de and bc, result in HL
	ld d,0
	ld l,d
	ld b,8
hte_loop:
	add hl,hl
	jr nc,hte_skip
	add hl,de
hte_skip:
	djnz hte_loop
	ret
divhld:
;quotient in hl, remainder in a
	ld e,b ;save b
	xor a
	ld b,16
dhd_loop:
	add hl,hl
	rla
	jr c,dhd_overflow
	cp d
	jr c,dhd_skip
dhd_overflow:
	sub d
	inc l
dhd_skip:
	djnz dhd_loop
	ld b,e
	ret