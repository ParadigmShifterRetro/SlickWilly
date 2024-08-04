; sjasmplus.exe --sym=out.sym --syntax=f --raw=out.bin swedit.asm

; SLICK WILLY EDITOR by Paradigm Shifter

; Original game by Matthew Smith (C) Bug Byte 1983, (C) 1983 Software Projects
; Engine Rewrite (C) 2010-2024 ParadigmShifter (aka Paradigm Shifter on WoS): https://spectrumcomputing.co.uk/forums/memberlist.php?mode=viewprofile&u=2484
; Updated animation/sprites: (C) 2024 gabriele amore (Spectrum Computing forum) https://spectrumcomputing.co.uk/forums/memberlist.php?mode=viewprofile&u=959
; AY music (C) 2024 Lee Bee (Spectrum Computing forum) https://spectrumcomputing.co.uk/forums/memberlist.php?mode=viewprofile&u=838
; Final Barrier/Title Screen image (C) 2024 Grongy (Spectrum Computing forum) https://spectrumcomputing.co.uk/forums/memberlist.php?mode=viewprofile&u=1817 
;	with tweaks by Lee Bee
; Eugene Sprite by HEXdidnt https://spectrumcomputing.co.uk/forums/memberlist.php?mode=viewprofile&u=2340

; Programming tech assist/brainstorming: Ketmar (Spectrum Computing forum) https://spectrumcomputing.co.uk/forums/memberlist.php?mode=viewprofile&u=1037 (no I'm not going to rewrite this in Forth) 
; Programming tech assist/brainstorming: AndyC (Spectrum Computing forum) https://spectrumcomputing.co.uk/forums/memberlist.php?mode=viewprofile&u=67
; Proportional font tech assist/brainstorming: 
;	uglifruit (Spectrum Computing forum) https://spectrumcomputing.co.uk/forums/memberlist.php?mode=viewprofile&u=615
;	Ast A. Moore (Spectrum Computing forum) https://spectrumcomputing.co.uk/forums/memberlist.php?mode=viewprofile&u=99
;	sn3j (Spectrum Computing forum) https://spectrumcomputing.co.uk/forums/memberlist.php?mode=viewprofile&u=2192

; PNG to SCR for Final Barrier image done by XoRRoX (Spectrum Computing forum) https://spectrumcomputing.co.uk/forums/memberlist.php?mode=viewprofile&u=496 

; Test/Proof of Concept: Morkin (Spectrum Computing forum) https://spectrumcomputing.co.uk/forums/memberlist.php?mode=viewprofile&u=68

; With thanks to Manic Miner disassembly by Richard Dymond, disassembled with Skoolkit by Richard Dymond, with contributions from Philip M Anderson
; Skoolkit: https://skoolkit.ca/docs/skoolkit/whatis.html
; SkoolKid on Spectrum Computing Forum https://spectrumcomputing.co.uk/forums/memberlist.php?mode=viewprofile&u=149

; Tools used during development:

; SjASMPlus Z80 Cross-Assembler v1.20.3 (https://github.com/z00m128/sjasmplus)
;	Based on code of SjASM by Sjoerd Mastijn (http://www.xl2s.tk)
;	Copyright 2004-2023 by Aprisobal and all other participants

; bin2asm.exe by Rob Pearmain available here: https://worldofspectrum.net/utilities/

; ZXNext image conversion tools available here: https://zx.remysharp.com/tools/

; ZXSpin emulator
; Fuse emulator

ORGADDR	EQU #6000
ENTRYPOINT	EQU	#8000

	ORG ORGADDR

	DISPLAY "ORIGIN ", /A, ORGADDR
	DISPLAY "ENTRY POINT ", /A, ENTRYPOINT
	DISPLAY "RESERVED MEMORY FOR LEVEL DATA ", /A, ENTRYPOINT - ORGADDR

SCRBASE		EQU	#4000
ATTRIBS		EQU #5800
FONTBASE	EQU	proportional_font-256

TRUE	EQU 1
FALSE	EQU 0

SILLYSPEED EQU FALSE;TRUE
DBG_OCCUPIED EQU FALSE

SPLASH	EQU TRUE

ROLLINGDEMO	EQU  FALSE

DOUBLEUPTUNE	EQU FALSE ; TRUE sounds bad lol

PLAYTUNE	EQU FALSE;TRUE;
	IF DOUBLEUPTUNE
TUNETICKINITIAL	EQU 2;5
	ELSE
TUNETICKINITIAL	EQU 4;5
	ENDIF
TUNENUMLOOPS	EQU 3; 3 is standard, 2 sounds ok though?
TUNE3LOOPSTHEN2	EQU 0;1

PACKED		EQU FALSE

FPS25		EQU	FALSE

GAMEPLAYINGSATE		EQU 0
GAMEOVERSTATE		EQU 1
GAVEOVERINPROGRESS	EQU 2

	IF	!FPS25
TIMING		EQU FALSE;TRUE
	ELSE
TIMING		EQU FALSE
	ENDIF

BEAMFLICKERTEST	EQU FALSE

AIRTICKS	EQU 16

GUARDIANSERASEATTRIBS	EQU	FALSE

FIXFINALIMAGE	EQU FALSE

ALWAYSSAVESP	EQU	TRUE

SIMPLEDESCENDERS EQU 0;1
ASCENDERSDESCENDERS	EQU 1
MAYBE8TALL	EQU 1

CHAR_HEIGHT EQU 10

BUFFWIDTH	EQU 40
MAXKEYS		EQU 15 ; must be power of 2 - 1

HORZ_REPEAT_BIT	EQU 4
VERT_REPEAT_BIT	EQU 5
HORZ_REPEAT	EQU (1<<HORZ_REPEAT_BIT) ; horizontal repeat brush flag
VERT_REPEAT	EQU (1<<VERT_REPEAT_BIT) ; vertical repeat brush flag

MAXCONVEYORS	EQU 8

TEMPNAMEBUFF	EQU collision_map+450

	IF SIMPLEDESCENDERS
SIMPLEDESCENDERBIT	EQU 7
SIMPLEDESCENDER		EQU (1<<SIMPLEDESCENDERBIT)
	ELSE
SIMPLEDESCENDER	EQU 0
	ENDIF

	IF ASCENDERSDESCENDERS
DESCENDERBIT	EQU 7
DESCENDER		EQU (1<<DESCENDERBIT)
ASCENDERBIT		EQU 6
ASCENDER		EQU (1<<ASCENDERBIT)
	ELSE
	ENDIF 

	IF MAYBE8TALL
IS8TALLBIT	EQU 5
IS9TALLBIT	EQU 4
IS8TALL		EQU (1<<IS8TALLBIT)
IS9TALL		EQU (1<<IS9TALLBIT)
	ELSE
IS8TALL		EQU 0
	ENDIF

EVENT_SOLAR		EQU #FE
EVENT_GFXCOPY	EQU #FD
EVENT_GAMEOVER	EQU #FC

TALLCHARBUFF	EQU collision_map + 512 - 9 ; last 9 bytes of collision map used as tall character buffer

PUTCHAR	EQU sputprcharlinear;put8x8prchar;put8x8char;

	MACRO ADDTOERASECELLSLIST
	push hl
	ld bc, ATTRIBS-collision_map
	add hl, bc
	ATTRIBSTOSCR h
	ex de, hl
	ld (hl), a
	inc l ; does not cross 256 byte boundary
	ld (hl), e
	inc l ; does not cross 256 byte boundary
	ex de, hl
	pop hl
	ENDM

	MACRO ADDTOREDRAWCELLSLIST
	exx
	rlca ; *2
	ld l, a
	ld c, (hl)
	inc l
	ld b, (hl)
	ex de, hl
	ld (hl), b
	inc l
	ld (hl), c
	inc l
	exx
	push hl
	ld bc, ATTRIBS-collision_map
	add hl, bc
	ATTRIBSTOSCR h
	exx
	ld (hl), a
	inc l
	exx
	ld a, l
	exx
	ld (hl), a
	inc l
	ex de, hl
	exx
	pop hl
	ENDM

	; B row
	; C column. Both of those in attrib cells
	MACRO ROWCOLTOSCRADDR
	ld a, b
	add b ; A = B*2
	ld h, tbl_rows/256 ; high byte of screen address lookup table. Aligned 256 so low byte will be just row*2
	ld l, a ; index into table 
	ld a, (hl) ; low byte of screen address
	inc l ; point HL to high byte of screen address
	ld h, (hl) ; read high byte of screen address
	add c ; add on column to low byte of screen address
	ld l, a ; and write it back. HL now holds correct screen address
	; so we now know the address...
	ENDM

	MACRO AROWX2TOSCRROWADDR
	ld h, tbl_rows/256 ; high byte of screen address lookup table. Aligned 256 so low byte will be just row*2
	ld l, a ; index into table 
	ld a, (hl) ; low byte of screen address
	inc l ; point HL to high byte of screen address
	ld h, (hl) ; read high byte of screen address
	ld l, a
	ENDM

	MACRO SCRTOATTRIBS reg
	ld a, reg

	or #87
	rra
	rra
	srl a
	ENDM

	MACRO ATTRIBSTOSCR reg
    ld a, reg
    add a
    add a
    add a
    and reg
    ENDM

	; A = collmap tile
	MACRO COLLMAPTOTILE
	ex de, hl
	rlca ; *2
	ld h, level_tiles/256
	ld l, a
	ld a, (hl)
	inc l
	ld h, (hl)
	ld l, a
	ex de, hl
	ENDM

	; A = collmap tile
	MACRO COLLMAPTOTILEATTRIB
	ex de, hl
	ld h, tile_attribs/256
	ld l, a
	ld a, (hl)
	ex de, hl
	ENDM

	MACRO SETBORDER bdr
	ld a, bdr
	out (#FE), a
	ENDM

	MACRO DWXYTOSCRADDR _x_, _y_
	dw SCRBASE + (((_y_&#7)|((_y_&#C0)>>3))<<8)|((_x_&#1F)|((_y_&#38)<<2))
	ENDM

	MACRO XYTOSCRADDRHL _x_, _y_
	ld hl, SCRBASE + (((_y_&#7)|((_y_&#C0)>>3))<<8)|((_x_&#1F)|((_y_&#38)<<2))
	ENDM

	MACRO XYTOSCRADDROFFSET _regpair_, _x_, _y_, _offset_
	ld _regpair_, SCRBASE + _offset_ + (((_y_&#7)|((_y_&#C0)>>3))<<8)|((_x_&#1F)|((_y_&#38)<<2))
	ENDM

	MACRO XYTOSCRADDRHLDE _x_, _y_
	XYTOSCRADDRHL _x_, _y_
	XYTOSCRADDROFFSET de, _x_, _y_, 1
	ENDM


	MACRO MEMSET _startaddr_, _length_, _value_
	ASSERT _length_ > 1
	ld hl, _startaddr_
	ld de, _startaddr_+1
	ld bc, _length_-1
	ld (hl), _value_
	ldir
	ENDM

	MACRO MEMCLEAR _startaddr_, _length_
	MEMSET _startaddr_, _length_, 0
	ENDM

	MACRO IPB ink, paper, bright
	db ink|(paper<<3)|(bright<<6)
	ENDM

	MACRO IPB_LAST ink, paper, bright
	db ink|(paper<<3)|(bright<<6)|128
	ENDM

	MACRO XY _x_, _y_
	dw (_y_)*32+_x_
	ENDM

	MACRO DRAWSPRITE2LINES
	pop de								; 10T	
	ld (hl), e							; 7T
	inc l								; 4T
	ld (hl), d							; 7T
	inc h								; 4T
	pop de								; 10T
	ld (hl), d							; 7T
	dec l								; 4T
	ld (hl), e							; 7T
	inc h								; 4T
	ENDM

FRAMESHIFT			EQU 13
IS4FRAMEBIT			EQU 12
ISHALFSPEEDBIT		EQU 11
HALFSPEEDFLAGBIT	EQU 0
IS4FRAMEFLAGBIT		EQU 1
HGFRAMEMASK			EQU #E0

	MACRO HG8FRAME_XY_FLAGS _x_, _y_, _frame_, _halfspeed_
	dw _y_*32+_x_|(_frame_<<FRAMESHIFT)|(_halfspeed_<<ISHALFSPEEDBIT)
	ENDM

	MACRO HG4FRAME_XY_FLAGS _x_, _y_, _frame_, _halfspeed_
	dw _y_*32+_x_|(_frame_<<FRAMESHIFT)|(1<<IS4FRAMEBIT)|(_halfspeed_<<ISHALFSPEEDBIT)
	ENDM

HGUARDIANPAGE	EQU horizontal_guardians_page

	MACRO HGUARDIANTYPE _gfx_
	db (_gfx_ - HGUARDIANPAGE) / 128
	ENDM

VGUARDIANPAGE	EQU vertical_guardians_page

	MACRO VGUARDIANTYPE _gfx_
	db (_gfx_ - VGUARDIANPAGE) / 128
	ENDM

	MACRO	VGUARDIAN	_x_, _y_, _frame_
	db	_y_
	db	_x_|(_frame_<<6)
	ENDM

VGVELSHIFT	EQU 2
VGVELMASK	EQU #FC
VGFLAGSMASK	EQU ~VGVELMASK

VGEUGENE	EQU 1
VGKONG		EQU 2
VGSKYLAB	EQU 3
VGSKYLABCRASHING	EQU 4
VGDONTERASEFLAG	EQU 128

	MACRO	VGUARDIANVELFLAGS _vel_, _flags_
	db (_vel_<<VGVELSHIFT)|_flags_
	ENDM

KEYPAGE EQU gfx_keypage

	MACRO keytype _gfx_
	db (_gfx_ - KEYPAGE) / 8
	ENDM

EXITPAGE EQU gfx_exit0

HAS_HGUARDIAN_BIT	EQU 6
HAS_VGUARDIAN_BIT	EQU 7

	MACRO EXITANDGUARDIANS _exitgfx_, _hashorzguardians_, _hasvertguardians_
	db ((_exitgfx_-EXITPAGE)/32)|(_hashorzguardians_<<HAS_HGUARDIAN_BIT)|(_hasvertguardians_<<HAS_VGUARDIAN_BIT)
	ENDM

TILEPAGE EQU gfx_8x8page

	MACRO celltype _gfx_
	db (_gfx_ - TILEPAGE) / 8
	ENDM

	MACRO TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	IF !PACKED
	db	_platforms_idx_
	db	_spikies_idx_
	db	_crumblies_idx_
	db	_conveyorl_idx_
	db	_conveyorr_idx_
	db	_bg_idx_
	ENDIF
	ENDM

	MACRO TILE _x_, _y_
	IF !PACKED
	db _x_
	db _y_
	ENDIF
	ENDM

	MACRO TILELAST _x_, _y_
	IF !PACKED
	db _x_
	db _y_|128
	ENDIF
	ENDM

	MACRO TILERPT _x_, _y_, _count_
	IF !PACKED
	db _x_
	db _y_
	db _count_
	ENDIF
	ENDM

	MACRO TILERPT_LAST _x_, _y_, _count_
	IF !PACKED
	db _x_
	db _y_|128
	db _count_
	ENDIF
	ENDM

	MACRO TILEHVRPT _x_, _y_, _width_, _height_
	IF !PACKED
	db _x_
	db _y_
	db _width_
	db _height_
	ENDIF
	ENDM

	MACRO TILEHVRPT_LAST _x_, _y_, _width_, _height_
	IF !PACKED
	db _x_
	db _y_|128
	db _width_
	db _height_
	ENDIF
	ENDM

bytesfree BLOCK ENTRYPOINT-$

main:
	ld sp, 0

	;call streamtest
	SETBORDER 0 

	IF SPLASH
	; A = 0 here
	ld hl, ATTRIBS
	ld de, ATTRIBS+1
	ld (hl), a
	ld bc, 8*32-1
	ldir

	ld hl, collision_map
	;ld de, ATTRIBS+8*32
	ld bc, 7*32
	ldir

	ld hl, ATTRIBS+15*32
	inc de
	ld (hl), a
	ld bc, 9*32-1
	ldir

.morehalt
	halt
	djnz .morehalt
	ENDIF

	call cls

game_init:
	xor a
	ld (tune_offset), a
	inc a
	ld (tune_play), a
	ld a, TUNETICKINITIAL
	ld (tune_tick), a

	call init_attribs

	call hud_init

	call parse_unpacked_level

mainloop:
	IF TIMING
	SETBORDER 7
	ENDIF

	halt
	IF FPS25
	halt
	ENDIF
	ld (mainlooprestoresp+1), sp ; stash stack pointer

	ld a, (game_state)
	cp 1
	jr nc, .doneerasewilly

	IF TIMING
	SETBORDER 1
	ENDIF

	ld hl, tile_attribs
	ld c, (hl) ; background colour

	ld hl, erasecellslist
	ld b, 0
.nextcellerase
	ld a, (hl)
	or a
	jr z, .donecellerase
	inc l
	ld e, (hl)
	ld d, a
	inc l

	ex de, hl

	REPT 8, idx
	ld (hl), b
	IF idx < 7
	inc h
	ENDIF
	ENDR

	; attribs
	SCRTOATTRIBS h
	ld h, a ; HL should be attribs address now

	ld (hl), c

	ex de, hl

	jr .nextcellerase

.donecellerase
	ld hl, redrawcellslist
.nextcellredraw
	ld a, (hl)
	or a
	jr z, .doneerasewilly
	inc l
	ld c, (hl)
	ld b, a
	inc l
	ld d, (hl)
	inc l
	ld e, (hl)
	inc l

	REPT 8, idx
	ld a, (bc)
	ld (de), a
	IF idx < 7
	inc c
	inc d
	ENDIF
	ENDR
	jr .nextcellredraw

.doneerasewilly

	ld hl, tile_attribs
	ld c, (hl) ; background colour

	; erase
	; ARE THESE THE WRONG ENDIANNESS OR NOT??
	; They are supposed to be HIGH BYTE LOW BYTE so high byte is guaranteed to be non-zero?
	; looks like they are other way around :(
	ld hl, erase8x16list
	ld b, 0
.nexterase
	ld a, (hl)
	or a
	jr z, .doneerase8x16
	inc l
	ld d, (hl)
	ld e, a
	ex de, hl

	REPT 8
	ld (hl), b
	inc h
	ENDR

	; move to next cell down
	ld a, l             ;calculate screen address to start of next 8 lines
	add #20		        ;add 32 to screen pointer
	ld l, a
	jr c, .addr_ok      ;jump if addition resulted in overflow (this means we are at next 64 line segment)
	ld a, h
	sub 8	            ;subtract 8*256
	ld h, a
.addr_ok

	REPT 7
	ld (hl), b
	inc h
	ENDR
	ld (hl), b

	ex de, hl
	inc l
	jr .nexterase


.doneerase8x16

	IF GUARDIANSERASEATTRIBS
	ld hl, tile_attribs
	ld c, (hl) ; background colour
	ENDIF

	ld hl, erase16xNlist

.nexterase16xN
	ld a, (hl)
	or a
	jr z, .doneerase
.doerase16xN
	inc l
	ld d, (hl)
	ld e, a
	inc l
	ld b, (hl)
	inc l
	ex de, hl

	;dec h - we did this already when we built the erase list
	xor a
.erase16xNloop
	inc h
	ld (hl), a
	inc l
	ld (hl), a
	dec l
	djnz .erase16xNloop

	IF GUARDIANSERASEATTRIBS
	; attribs
	SCRTOATTRIBS h
	ld h, a ; HL should be attribs address now 
	ld (hl), c
	inc l
	ld (hl), c
	ENDIF

	ex de, hl
	jr .nexterase16xN

.doneerase

	; DRAWING WITH STACK STARTS HERE
	di

	IF TIMING
	SETBORDER 2
	ENDIF
	; draw guardians
	ld hl, guardian0

drawnextguardian:
	ld a, (hl)
	or a
	jr z, donedrawguardians
	inc l
	ld e, (hl)
	ld d, a
	inc l
	ld c, (hl)
	ld b, l
	ld a, GUARDIAN_DATA.flags - GUARDIAN_DATA.frame
	add l
	ld l, a
	bit IS4FRAMEFLAGBIT, (hl)
	jr z, .not4frame
	res 7, c
.not4frame
	ld l, b
	ld a, c

	add e
	ld e, a
	ex de, hl
	ld sp, hl
	ex de, hl
	inc l
	ld e, (hl)
	inc l
	ld d, (hl)
	inc l
	ld c, (hl) ; attrib

	ld a, d
	and 7
	jp z, draw_horz_guardian

	; lookup how many rows we want to draw in top and bottom cell and stash in the alt register set (in BC probs)
	; can use B'C' before I set them
	; 1 reg needs to point to the table, so probs D'E'. D' is constant in the loop
	; HL could point to one of the addresses and JP (HL)
	; JP (IX) is faster than a normal jump too (8T) so can use that as well to jump to the other routine.
	; Just set IX and H'L' outside the loop

	jp draw_vert_guardian

donedrawguardians:

	IF TIMING
	SETBORDER 4
	ENDIF
	
	; draw the conveyors if it's an odd frame
	ld a, (frame_counter)
	and 3
	jr nz, .doneconveyordraw

	ld hl, conveyorlist
	exx
	;ld b, 0 ; always < 255 count for ldir
	ld b, a ; A is 0 since we didn't take the jump above

	exx
.conveyorloop
	ld a, (hl)
	or a
	jr z, .doneconveyordraw
	inc l
	exx
	ld e, a
	exx
	ld a, (hl)
	inc l
	exx
	ld d, a
	exx
	ld a, (hl)
	rrca
	rrca
	ld (hl), a
	ex af, af'
	inc l
	ld a, (hl)
	inc l
	exx
	ld c, a
	ld h, d
	ld l, e
	ex af, af'
	ld (hl), a
	dec c
	jr z, .conveyorsingle
	inc e
	ldir ; draw the rest

	ex af, af'
	ld a, h
	xor 2
	ld h, a
	ld a, d
	xor 2
	ld d, a
	exx
	ld a, (hl)
	rlca
	rlca
	ld (hl), a
	ex af, af'
	dec l
	ld a, (hl)
	exx
	ld c, a
	ex af, af'
	ld (hl), a
	dec c
	dec e
	dec e
	lddr

	exx
	inc l
	inc l

	jr .conveyorloop

.conveyorsingle
	exx
	ld a, (hl)
	rlca
	rlca
	ld (hl), a
	exx
	ex af, af'
	ld a, h
	xor 2
	ld h, a
	ex af, af'
	ld (hl), a

	exx
	inc l

	jr .conveyorloop

.doneconveyordraw

	IF TIMING
	SETBORDER 1
	ENDIF

	ld a, (game_state)
	cp GAMEOVERSTATE
	jr nz, .notgameoverframe1
	inc a
	ld (game_state), a
	jr .drawwillythisframe
.notgameoverframe1
	cp GAVEOVERINPROGRESS
	jp nc, dogameoverattribs
.drawwillythisframe

drawwilly:
	jp draw_willy_2rows

donedrawwilly:

	IF TIMING
	SETBORDER 6
	ENDIF

	ld a, (frame_counter)
	rrca
	rrca
	and 3
	ex af, af'
	ld a, (keydata)
	add 3
	ld ixl, a

	; draw the keys
	ld hl, key_positions

drawkeysloop:
	ld a, (hl)
	or a
	jr z, donedrawingkeys
	inc l
	ld d, a
	ld e, (hl)
	inc l
	ex de, hl

keygfx:
	ld sp, gfx_key0
	REPT 4, idx
	pop bc
	ld (hl), c
	inc h
	ld (hl), b
	IF idx < 3
	inc h
	ENDIF
	ENDR

	SCRTOATTRIBS h
	ld h, a
	ex af, af'
	ld b, ixl
	ld c, a
	add b
	ld (hl), a
	ld a, c
	inc a
	and 3
	ex af, af'
	ex de, hl
	jr drawkeysloop

donedrawingkeys

	IF TIMING
	SETBORDER 3
	ENDIF
	; draw the exit
exitrow0:
	ld hl, 0
exitgfx0:
	ld sp, gfx_exit0

	REPT 4, idx
	pop de								; 10T	
	ld (hl), e							; 7T
	inc l								; 4T
	ld (hl), d							; 7T
	pop de								; 10T
	inc h								; 4T
	ld (hl), d							; 7T
	dec l								; 4T
	ld (hl), e							; 7T

	IF idx < 3
	inc h								; 4T
	ENDIF
	ENDR
	 
exitrow1:
	ld hl, 0

	REPT 4, idx
	pop de								; 10T	
	ld (hl), e							; 7T
	inc l								; 4T
	ld (hl), d							; 7T
	pop de								; 10T
	inc h								; 4T
	ld (hl), d							; 7T
	dec l								; 4T
	ld (hl), e							; 7T

	IF idx < 3
	inc h								; 4T
	ENDIF
	ENDR

attribexit:
	ld a, 0
attribrow0:
	ld hl, 0
	ld (hl), a
	inc l
	ld (hl), a
attribrow1:
	ld hl, 0
	ld (hl), a
	inc l
	ld (hl), a

	ld a, (has_solar)
	or a
	jp z, dontdosolar
	IF TIMING
	SETBORDER 1
	ENDIF

	; solar power
	ld h, ATTRIBS/256 ; we'll erase and draw top half first
solar_erase_upper:
	ld sp, solar_power_erase0
solar_bkg_colour0:
	ld a, 0 ; level background colour SMC
.eraseuppergetmoredata
	pop de
	inc e ; since terminator is 255
	jr z, .doupperbeam
	dec e ; since terminator was 255
	ld l, e
	ld (hl), a
	ld l, d
	inc d ; since terminator is 255
	jr z, .doupperbeam
	dec d ; since terminator was 255
	ld (hl), a
	jp .eraseuppergetmoredata
.doupperbeam
solar_draw_upper:
	ld sp, solar_power_draw0
beam_colour0:
	ld a, 0 ; beam colour SMC
.beamuppergetmoredata
	pop de
	inc e ; since terminator is 255
	jr z, .dolowererase
	dec e ; since terminator was 255
	ld l, e
	ld (hl), a
	ld l, d
	inc d ; since terminator is 255
	jr z, .dolowererase
	dec d ; since terminator was 255
	ld (hl), a
	jp .beamuppergetmoredata

.dolowererase
	IF TIMING
	SETBORDER 2
	ENDIF
solar_erase_lower:
	ld sp, solar_power_erase1
solar_bkg_colour1:
	ld a, 0 ; level background colour SMC
	inc h ; middle third attribs
.eraselowergetmoredata
	pop de
	inc e ; since terminator is 255
	jr z, .dolowerbeam
	dec e ; since terminator was 255
	ld l, e
	ld (hl), a
	ld l, d
	inc d ; since terminator is 255
	jr z, .dolowerbeam
	dec d ; since terminator was 255
	ld (hl), a
	jp .eraselowergetmoredata
.dolowerbeam
solar_draw_lower:
	ld sp, solar_power_draw1
beam_colour1:
	ld a, 0 ; beam colour SMC
.beamlowergetmoredata
	pop de
	inc e ; since terminator is 255
	jr z, solardone
	dec e ; since terminator was 255
	ld l, e
	ld (hl), a
	ld l, d
	inc d ; since terminator is 255
	jr z, solardone
	dec d ; since terminator was 255
	ld (hl), a
	jp .beamlowergetmoredata

solardone:
	ld a, (solar_erase_upper+1)
	xor ((solar_power_erase0&255)^(solar_power_draw0&255))
	ld (solar_erase_upper+1), a

	ld a, (solar_erase_lower+1)
	xor ((solar_power_erase1&255)^(solar_power_draw1&255))
	ld (solar_erase_lower+1), a

	ld a, (solar_draw_upper+1)
	xor ((solar_power_erase0&255)^(solar_power_draw0&255))
	ld (solar_draw_upper+1), a

	ld a, (solar_draw_lower+1)
	xor ((solar_power_erase1&255)^(solar_power_draw1&255))
	ld (solar_draw_lower+1), a

dontdosolar:

mainlooprestoresp:
	ld sp, 0 ; SMC
	ei

	ld a, (game_state)
	or a
	call z, hud_update

	ld hl, erase8x16listptr
	ld (hl), erase8x16list&255

	ld hl, erase16xNlistptr
	ld (hl), erase16xNlist&255

	IF !SILLYSPEED
	ld a, (frame_counter)
	and 1
	call z, update_guardians
	ELSE ; silly speed
	call update_guardians
	ENDIF

	ld a, (has_solar)
	or a
	call nz, do_solar_down_left

	IF TIMING
	SETBORDER 3
	ENDIF

	call update_willy

	ld hl, frame_counter
	inc (hl)

	IF ROLLINGDEMO
	ld a, (hl)
	or a
	jp nz, .dontchangelevel
	ld hl, (lvl_next)
	ld (current_level), hl
	jr .newlevel
.dontchangelevel
	ENDIF

	IF DBG_OCCUPIED
	call dbg_show_occupied_cells
	ENDIF

	IF PLAYTUNE
	ld a, (tune_play)
	or a
	jr z, .skipnote

	; play tune
	ld a, (tune_tick)
	dec a
	ld (tune_tick), a
	jr nz, .skipnote
	ld a, (tune_offset)
	inc a
	ld (tune_offset), a
	ld a, TUNETICKINITIAL

.playnote
	ld (tune_tick), a
	ld a, (tune_offset)
	IF !DOUBLEUPTUNE
	and #7e
	ELSE
	and #FE
	ENDIF
	rrca
	ld e, a
	ld d, 0
	ld hl, tune_data
	add hl, de
	IF TIMING
	xor a
	ELSE
	ld a, (bdr_colour)
	ENDIF
	ld e, (hl)

	IF TUNE3LOOPSTHEN2
	ld b, a ; B=0
	ld a, (tune_numloops)
	ld c, a
	xor 1
	ld (tune_numloops), a
	xor a
	ELSE
	ld bc, TUNENUMLOOPS
	ENDIF
.tuneloop
	out (#fe), a
	dec e
	jr nz, .noflip
	ld e, (hl)
	xor #18
.noflip
	djnz .tuneloop
	dec c
	jr nz, .tuneloop
.skipnote
	ENDIF

	IF !ROLLINGDEMO
	; see if we are pressing S or D
	ld a, #fd
	in a, (#fe)

	; if we press d skip to next level
	bit 2, a
	jr nz, .carryon

	call next_level
	xor a ; this should cause next test for pressing S to go off, so level will be redrawn

.carryon
	; reset level if we press S
	bit 1, a
	jp z, .newlevel
	ENDIF
	jp mainloop

.newlevel
	ld hl, (current_level)
	call parse_unpacked_level
	jp mainloop

dogameoverattribs:
	ld sp, ATTRIBS+510
	ld b, 0
	ld a, (guardian0+GUARDIAN_DATA.vgy)
	cp 96
	jr z, .doneboot
	;ld hl, ATTRIBS
	;ld a, (hl)
	; pop the data instead, saves a load of HL etc.
	pop af
	add 24
	and %01011111

	ld d, a
	ld e, a

.morerows
	push de
	djnz .morerows

	ld a, (guardian0+GUARDIAN_DATA.vgy)
	rlca
	cpl
	ld e, a
	xor a
	ld c, #FE
	ld hl, #2818
.wobblespeaker
	;out (#FE), a ; this is faster than OUT (C), A lol
	out (c), a ; but this has less noise I think?
	xor l
	ld b, e
.delayloop
	djnz .delayloop
	dec h
	jp nz, .wobblespeaker
	jp solardone

.doneboot
	ld de, #4747
	pop af ; SP is off by 2 here, so pop it

.morerowsdoneboot
	push de
	djnz .morerowsdoneboot

	ld hl, (mainlooprestoresp+1)
	ld sp, hl

	MEMCLEAR collision_map, 512

	ld hl, collision_map+BUFFWIDTH
	ld (textoutaddr), hl
	ld a, 7
	ld (textxoffset), a
	ld hl, strGame
	call put_strz
	ld bc, 6*8*256+9*8
	ld a, 5
	call put_string_at_width

	MEMCLEAR collision_map, 512

	ld hl, collision_map+BUFFWIDTH
	ld (textoutaddr), hl
	xor a
	ld (textxoffset), a
	ld hl, strOver
	call put_strz
	ld bc, 6*8*256+18*8
	ld a, 4
	call put_string_at_width

	ld a, #47
	ei
	ld b, 50
.repeat
	push bc
	halt
	halt
	ld hl, ATTRIBS+6*32+9
	ld de, ATTRIBS+6*32+9+1
	ld bc, 4
	ld (hl), a
	ldir
	xor 4
	ld hl, ATTRIBS+6*32+18
	ld de, ATTRIBS+6*32+18+1
	ld bc, 3
	ld (hl), a
	ldir
	xor 4
	add 5
	and 7
	or #40
	pop bc
	djnz .repeat

	ld hl, Central_Cavern
	;ld hl, Game_Over
	ld (current_level), hl
	jp game_init


; next_level
next_level:

.waitfordebounce
	ld a, #fd
	in a, (#fe)

	; if we press d skip to next level
	bit 2, a
	jr z, .waitfordebounce

	ld hl, (lvl_next)
	ld (current_level), hl
	ret

clear_level_name_text_area:
	REPT 12, idx
	XYTOSCRADDRHLDE 0, 16*8+idx+1
	ld bc, 31
	ld (hl), b
	ldir
	ENDR
	ret

hud_init:
	MEMCLEAR collision_map, 512

	ld hl, collision_map+BUFFWIDTH
	ld (textoutaddr), hl
	ld a, 1
	ld (textxoffset), a
	ld hl, strAir
	call put_strz
	ld bc, 18*8*256
	call put_string_at

	ld hl, collision_map
	ld de, collision_map+1
	push hl
	push de
	ld (hl), l ; collision_map is align 256
	ld bc, BUFFWIDTH*CHAR_HEIGHT-1 ; CHAR_HEIGHT is max height of a character
	ldir

	ld hl, collision_map+BUFFWIDTH
	ld (textoutaddr), hl
	ld a, 1
	ld (textxoffset), a
	ld hl, strHighScore
	call put_strz
	ld bc, 20*8*256
	call put_string_at

	pop de ; collision_map+1
	pop hl ; collision_map
	ld (hl), l ; collision_map is align 256
	ld bc, BUFFWIDTH*CHAR_HEIGHT-1 ; CHAR_HEIGHT is max height of a character
	ldir

	ld hl, collision_map+BUFFWIDTH
	ld (textoutaddr), hl
	xor a
	ld (textxoffset), a
	ld (willy_dance_idx), a
	ld (scoreishighscore), a
	ld hl, strScore
	call put_strz
	ld bc, 20*8*256+170
	call put_string_at

	ld a, #FF

	XYTOSCRADDRHLDE 0, 19*8
	ld (hl), a
	ld bc, 31
	ldir

	XYTOSCRADDRHLDE 0, 16*8
	ld (hl), a
	ld bc, 31
	ldir

	XYTOSCRADDRHLDE 0, 17*8+7
	ld (hl), a
	ld bc, 31
	ldir

	XYTOSCRADDRHLDE 0, 17*8+5
	ld (hl), a
	ld bc, 31
	ldir

	XYTOSCRADDRHL 0, 17*8+6
	ld (hl), #E0
	XYTOSCRADDRHL 31, 17*8+6
	ld (hl), #07

	IF 0
	XYTOSCRADDRHLDE 4, 18*8+2
	ld (hl), a
	ld bc, 27
	ldir

	XYTOSCRADDRHLDE 4, 18*8+3
	ld (hl), a
	ld bc, 27
	ldir

	XYTOSCRADDRHLDE 4, 18*8+4
	ld (hl), a
	ld bc, 27
	ldir

	XYTOSCRADDRHLDE 4, 18*8+5
	ld (hl), a
	ld bc, 27
	ldir

	XYTOSCRADDRHLDE 4, 18*8+6
	ld (hl), a
	ld bc, 27
	ldir
	ENDIF

	ld a, #18 ; part of a dot or colon
	XYTOSCRADDRHL 30, 22*8+6
	ld (hl), a
	inc h
	ld (hl), a

	XYTOSCRADDRHL 27, 22*8+3
	ld (hl), a
	inc h
	ld (hl), a
	inc h
	inc h
	ld (hl), a
	inc h
	ld (hl), a

	XYTOSCRADDRHL 24, 22*8+3
	ld (hl), a
	inc h
	ld (hl), a
	inc h
	inc h
	ld (hl), a
	inc h
	ld (hl), a

	ld b, 8;16 ; max num lives
	XYTOSCRADDRHL 0, 192-16
	ld de, gfx_gnewwilly0 
.livesloop
	push hl
	push de
	call blitsprite16x16
	pop de
	pop hl
	inc l
	inc l
	djnz .livesloop

	ld a, 32
	ld (willy_dance_frame), a

	ld hl, score
	ld de, score+1
	ld bc, 7
	ld (hl), 0; '0'
	ldir

	call draw_time

	ld hl, highscore+7
	ld c, 8
	jp draw_score

hud_update:
	IF TIMING
	SETBORDER 2
	ENDIF

	ld a, (willy_dance_idx)
	XYTOSCRADDRHL 0, 192-16
	add l
	ld l, a
	ld d, gfx_gnewwilly0/256
	ld a, (willy_dance_frame)
	ld e, a
	call blitsprite16x16

	ld a, (willy_dance_idx)
	add 2
	;cp 32
	cp 16
	jr nz, .doneupdatedanceframe

	ld a, (willy_dance_frame)
	add 32
	ld (willy_dance_frame), a
	xor a

.doneupdatedanceframe
	ld (willy_dance_idx), a

	call air_down

	ld c, 25
	ld hl, score+7

; HL: points to end of score data (i.e. the units part)
; C: column offset to adjust by
draw_score:
	ld b, 6

.scoreloop
	push hl
	push bc

	ld a, (hl)

	rlca
	rlca
	rlca
	ld de, FONTBASE+'0'*8+1
	add e
	ld e, a

	XYTOSCRADDRHL 0, 192-32+1
	pop bc
	ld a, b
	add c
	or l
	ld l, a

	REPT 7, idx
	ld a, (de)
	inc e
	ld (hl), a
	IF idx < 6
	inc h
	ENDIF
	ENDR

	pop hl
	dec hl
	djnz .scoreloop

draw_time:
	ld b, 9;6
	ld hl, time+6

.timeloop
	push hl
	push bc

	ld a, (hl)

	rlca
	rlca
	rlca
	ld de, FONTBASE+'0'*8+1
	add e
	ld e, a

	XYTOSCRADDRHL 0, 192-16+1
	pop bc
	ld a, b
	add 22
	or l
	ld l, a

	ld a, b
	cp 9
	jr z, .decb
	cp 6
	jr z, .decb
	cp 3
	jr z, .decb
.donedecb

	REPT 7, idx
	ld a, (de)
	inc e
	ld (hl), a
	IF idx < 6
	inc h
	ENDIF
	ENDR

	pop hl
	dec hl
	djnz .timeloop


	ld a, 7
	call add_score

	ld hl, time+7
	call add_time

	ld a, (scoreishighscore)
	or a
	ret z

	ld b, 0
	ld a, 6
	REPT 7, idx
	XYTOSCRADDRHL 26, 192-32+1+idx
	XYTOSCRADDROFFSET de, 9, 192-32+1+idx, 0
	ld c, a
	ldir
	ENDR

	ret
.decb
	dec b
	jr .donedecb

air_down:
	ld a, (air_column)
	ld b, a
	cp 3
	jr nc, .haveair
	;ld hl, willy_dead
	;inc (hl)
	ret

.haveair
	ld a, (air_tick)
	dec a
	ld (air_tick), a
	ret nz
	ld a, AIRTICKS
	ld (air_tick), a

	XYTOSCRADDRHL 0, 18*8+2
	ld a, l
	add b
	ld l, a

	ld a, (hl)
	sla a

	ld (hl), a
	inc h
	ld (hl), a
	inc h
	ld (hl), a
	inc h
	ld (hl), a
	inc h
	ld (hl), a
	or a
	ret nz
	ld a, b
	dec a
	ld (air_column), a
	ret

; A: place value of score to add
; 7 = add 1
; 6 = add 10
; 5 = add 100
; etc.
add_score:
	ld hl, score
	ld d, 0
	ld e, a
	add hl, de

	ld b, a
	inc b

.nextdigit
	inc (hl)

	ld a, (hl)
	cp 10
	jr c, .checkforhiscore
.overflow
	ld (hl), 0
	dec hl
	dec b
	ret z
	jr .nextdigit
.checkforhiscore
	ld a, (scoreishighscore)
	or a
	ret nz

	ld hl, score
	ld de, highscore
	ld b, 8
.hiscoreloop
	ld a, (de)
	cp (hl)
	jr z, .checknext
	ret nc
	ld hl, scoreishighscore
	inc (hl)
	ret

.checknext
	inc de
	inc hl
	djnz .hiscoreloop
	ret

; HL: pointer last digit of timer string
add_time:
	ld a, (hl)
	inc a
	ld b, 7

.nextdigit
	inc a
	ld (hl), a

	ld c, a
	ld a, b
	cp 4
	jr z, .checkfor6
	cp 2
	jr z, .checkfor6
	ld a, c
	cp 10

	ret c

.overflow
	ld (hl), 0
	dec hl
	dec b
	ret z
	ld a, (hl)
	jr .nextdigit

.checkfor6
	ld a, c
	cp 6
	ret c
	jr .overflow

; HL: pointer to dc format string
; DE: buffer to tokenise into
tokenise_string:
	ld b, 0 ; set to 1 at end of string
	ld a, (hl)
	inc hl
	cp 128
	jr c, .notlastchar
	and 127
	inc b
.notlastchar
	cp 32
	jr c, .tokenised
	jr nz, .notspace
	xor a ; TOKEN_SPACE
	jr .tokenised
.notspace
	sub 'A'-1
	and 31
.tokenised
	ld (de), a
	inc de
	djnz tokenise_string ; cheeky use of DJNZ here ;)
	ld a, TOKEN_ENDOFSTRING
	ld (de), a
	inc de
	ret

	DISPLAY "tokenise_string size: ", /A, $-tokenise_string

; A: character to print
; writes to linear storage array where each row is offset by BUFFWIDTH
sputprcharlinear:
	ld de, (textoutaddr)
	push de

	cp ' ' ; is it a space?
	jr z, .dospace

	rlca ; char*2
	ld bc, FONTBASE
	ld h, c ; 0
	ld l, a
	add hl, hl ; char*4
	add hl, hl ; char*8
	add hl, bc

	IF ASCENDERSDESCENDERS
	pop de
	ld a, (hl)
	ld c, a
	ex af, af' ; also stash width in A'
	ld a, c
	bit ASCENDERBIT, a
	jr z, .doneascender
	ld a, -BUFFWIDTH
	add e
	ld e, a
	jr c, .doneascender
	dec d
.doneascender
	bit DESCENDERBIT, c
	jr z, .doneaaddradjust

	ld a, BUFFWIDTH*2
	add e
	ld e, a
	jr nc, .doneaaddradjust
	inc d
.doneaaddradjust

	push de
	ld a, c
	ENDIF

	IF MAYBE8TALL
	and IS8TALL|IS9TALL
	jr z, .nottall

	push hl

	inc l ; point to first byte of glyph data
	ld de, TALLCHARBUFF
	ld bc, 4 ; going to copy 4 bytes
	push bc ; saves 1 byte push/pop rather than ld bc, 4 again
	ldir
	dec l
	and IS9TALL
	jr z, .copy2ndhalf
	ldi ; copy row 4
	dec l
.copy2ndhalf
	;ld bc, 4
	pop bc
	ldir

	pop hl

	ld hl, TALLCHARBUFF ; put this instead of glyph content

	ex af, af' ; get width back
	ld b, 8 ; this is the loop counter we will use
	bit IS9TALLBIT, a
	jr z, .doneloopctr
	inc b
.doneloopctr
	jr .gotglyphaddress ; so jump to after glyph address isfound

.nottall
	ENDIF

	ld a, (hl)
	ld b, 7 ; loop counter
	inc l ; point at first byte of glyphdata
.gotglyphaddress
	and 15 ; mask out flags from width byte
	ld c, a ; width in c

	call .dobuffoffsetadvance

	ld a, d ; original text offset
	or a
	pop de
	jr nz, .doshiftedtext

	ld c, BUFFWIDTH
.loop
	ld a, (hl)
	ld (de), a
	inc l
	ld a, e
	add c ; BUFFWIDTH
	ld e, a
	jr nc, .nextline
	inc d
.nextline
	djnz .loop

	ret

.dospace
	ld a, (FONTBASE+256) ; get width for space
	ld c, a
	pop de

.dobuffoffsetadvance
	ld a, (textxoffset)
	ld d, a ; orginal textoffset
	add c ; width
	inc a ; letter separator
	exx ; better than pushing popping hl
	ld hl, (textoutaddr)
.checknextcell
	cp 8
	jr c, .donereducemod8
	sub 8
	inc hl
	jr .checknextcell

.donereducemod8
	ld (textxoffset), a
	ld (textoutaddr), hl
	exx
	ret

.doshiftedtext
	; this could be made smaller if we ex de, hl here
	ld c, a

.nextrow
	push bc

	ld b, c
	ld c, 0
	ld a, (hl)
.shiftagain
	srl a
	rr c
	djnz .shiftagain
	ld b, a
	ld a, (de)
	or b
	ld (de), a
	inc de
	ld a, c
	ld (de), a
	;dec e
	ld a, e
	add BUFFWIDTH-1
	ld e, a
	jr nc, .nextshiftline
	inc d
.nextshiftline
	inc l

	pop bc

	djnz .nextrow
	ret

	DISPLAY "sputprcharlinear size: ", /A, $-sputprcharlinear

; HL: pointer to zero-terminated string
put_strz:
	ld a, (hl)
	or a
	ret z
	inc hl
	push hl
	call PUTCHAR
	pop hl
	jr put_strz

; B: row (0-191)
; C: column (0-255)
put_string_at:

	ld a, c
	and ~7
	rrca
	rrca
	rrca
	ld c, a
	ld a, 32
	sub c
	ld (.adjustforwidth+1), a

	ld a, b
	and ~7 ; mask out row offset

	rrca
	rrca ; divide by 4

	ex de, hl
	ld h, tbl_rows/256 ; high byte of screen address lookup table. Aligned 256 so low byte will be just row*2
	ld l, a ; index into table 
	ld a, (hl) ; low byte of screen address
	inc l ; point HL to high byte of screen address
	ld h, (hl) ; read high byte of screen address
	add c ; add on column to low byte of screen address
	ld l, a ; and write it back. HL now holds correct screen address

	ld a, b
	and 7
	ld b, a
	add h
	ld h, a

	ex de, hl
	ld hl, collision_map

	ld a, CHAR_HEIGHT

.nextline
	ex af, af'
	push de
	push hl
.adjustforwidth
	ld bc, 0 ; gets self modified to account for width
	ldir
	pop hl
	ld de, BUFFWIDTH
	add hl, de
	pop de

	inc d
	ld a, d
	and 7
	jr nz, .ok
	ld a, e
	add 32
	ld e, a
	jr c, .ok
	ld a, d
	sub 8
	ld d, a
.ok
	ex af, af'
	dec a
	jr nz, .nextline

	ret

; B: row (0-191)
; C: column (0-255)
; A: width in characters
put_string_at_width:

	ld (.adjustforwidth+1), a
	ld a, c
	and ~7
	rrca
	rrca
	rrca
	ld c, a

	ld a, b
	and ~7 ; mask out row offset

	rrca
	rrca ; divide by 4

	ex de, hl
	ld h, tbl_rows/256 ; high byte of screen address lookup table. Aligned 256 so low byte will be just row*2
	ld l, a ; index into table 
	ld a, (hl) ; low byte of screen address
	inc l ; point HL to high byte of screen address
	ld h, (hl) ; read high byte of screen address
	add c ; add on column to low byte of screen address
	ld l, a ; and write it back. HL now holds correct screen address

	ld a, b
	and 7
	ld b, a
	add h
	ld h, a

	ex de, hl
	ld hl, collision_map

	ld a, CHAR_HEIGHT

.nextline
	ex af, af'
	push de
	push hl
.adjustforwidth
	ld bc, 0 ; gets self modified to account for width
	ldir
	pop hl
	ld de, BUFFWIDTH
	add hl, de
	pop de

	inc d
	ld a, d
	and 7
	jr nz, .ok
	ld a, e
	add 32
	ld e, a
	jr c, .ok
	ld a, d
	sub 8
	ld d, a
.ok
	ex af, af'
	dec a
	jr nz, .nextline

	ret

init_attribs:
	xor a
	call cls

	IF 1
	MEMSET ATTRIBS+16*32, 32, 6*8
	;MEMSET ATTRIBS+16*32, 32, 64+7*8

	MEMSET ATTRIBS+17*32, 32, 64+6*8
	ELSE
	MEMSET ATTRIBS+16*32, 32, 64+6*8
	;MEMSET ATTRIBS+16*32, 32, 64+7*8

	MEMSET ATTRIBS+17*32, 32, 6*8
	ENDIF

	MEMSET ATTRIBS+18*32, 10, 64+2*8+7
	MEMSET ATTRIBS+18*32+10, 22, 64+4*8+7
	MEMSET ATTRIBS+19*32, 10, 64+2
	MEMSET ATTRIBS+19*32+10, 22, 64+4
	MEMSET ATTRIBS+20*32, 64, 64+6
	MEMSET ATTRIBS+22*32, 64, 64+5
	ret

	; A -> attrib to set when clearing screen
cls:
	; set attribs
	ld hl, #5800+767
	ld de, #5800+766
	ld bc, 768
	ld (hl), a
	lddr

	; clear pixels
	ld (hl), b ; since B=0 here
	ld bc, 6144-1
	lddr
	ret

; Bitstream data format
; WORD: Byte count
; BYTE: End address bitcount of packed bits
; BYTE[]: data stream

; Stream handle
; WORD: Current position
; BYTE: Current offset in byte

	IF 0
streamtest:
	ld hl, packed_data_test

	ld c, %11111
	call open_extract_n_bits

	REPT 8
	ld c, %11111
	call extract_n_bits
	ENDR

	ld hl, stream_place_test
	xor a
	ld (currentstream_bit), a
	REPT 9, idx
	ld b, 5
	ld a, idx+1
	call emplace_n_bits
	ENDR
	ret

; HL: start address of bitstream data (data format at start)
new_bitstream:
	push hl
	xor a
	ld (hl), a
	inc hl
	ld (hl), a
	inc hl
	ld (hl), a
	pop hl
	ret

; extract bits from a stream
; C: mask of bits to extract
open_extract_n_bits:
	ld e, (hl) ; get current byte
	inc hl
	ld d, (hl) ; get next byte
	ex af, af'
	xor a
	ld (currentstream_bit), a
	ex af, af'

	ld b, 8 ; speed up the loop

extract_n_bits:
	ld a, e
	and c
	ex af, af' ; bits we want are in A' now
	ld a, (currentstream_bit)

.morebits
	srl c ; shift mask down
	jr nc, .done ; if carry we need to shift, no carry we are done
	srl d
	rr e
	inc a
	cp b
	jr nz, .morebits
	sub b
	inc hl
	ld d, (hl) ; get next byte
	jr .morebits
.done

	ld (currentstream_bit), a
	ex af, af'
	ret

	DISPLAY "Bit extraction code size: ",/A, $-open_extract_n_bits

; A: value
; B: bitwidth
; HL: current stream pointer
emplace_n_bits:
	ld e, a 
	ld a, (currentstream_bit)
	or a
	jr z, .noshiftrequired
	ld d, 0
	ld c, b
	ld b, a
.shiftloop
	sla e
	rl d
	djnz .shiftloop
	add c
	cp 8
	jr c, .dontbumpptr
	ex af, af'
	ld a, (hl)
	or e
	ld (hl), a
	ex af, af'
	sub 8
	ld e, d
	inc hl

.dontbumpptr
	ld (currentstream_bit), a
	ld a, (hl)
	or e
	ld (hl), a

	ret

.noshiftrequired
	ld a, b
	ld (currentstream_bit), a
	ld (hl), e
	ret

	DISPLAY "Bit packer code size: ",/A, $-emplace_n_bits
	ENDIF

parse_unpacked_level:
	xor a
	ld (has_solar), a
	ld (game_state), a
	ld (frame_counter), a

	ld h, a
	ld l, a
	ld (gfx_copyevent), hl

	ld hl, erasecellslist
	ld (hl), a

	ld hl, redrawcellslist
	ld (hl), a

	ld hl, collision_map
	ld de, collision_map+1
	ld (hl), a
	ld bc, BUFFWIDTH*CHAR_HEIGHT-1 ; CHAR_HEIGHT is max height of a character
	ldir

	ld hl, guardian_occupycell_bitmap
	ld de, guardian_occupycell_bitmap+1
	ld (hl), a
	ld bc, 63
	ldir

	ld a, 31
	ld (air_column), a
	ld a, AIRTICKS
	ld (air_tick), a

	ld hl, collision_map+BUFFWIDTH
	ld (textoutaddr), hl

	call clear_level_name_text_area

	ld hl, erase8x16listptr
	ld (hl), erase8x16list&255
	ld hl, erase16xNlistptr
	ld (hl), erase16xNlist&255

	ld hl, (current_level)
	inc hl ; skip over count of bytes for now
	ld a, (hl) ; num keys+key paper/bright
	ld (keydata), a

	inc hl

	ld de, willy_colpos

	ldi

	ld a, (hl)
	inc hl

	ld c, 1
	bit 1, a
	jr z, .donewillyfacing
	dec c
	dec c
.donewillyfacing
	and 1 ; put in range 0-511 so collision map offset
	ld (de), a

	ld a, (willy_colpos)
	and 31
	rlca
	rlca
	rlca ; multiply by 8 to get willy_xpos
	ld (willy_xpos), a

	ld a, c
	ld (willy_facing), a

	exx
	call update_willy_pos
	exx

	ld de, exit_xpos
	ld bc, 3
	ldir

	ld bc, (exit_xpos)
	ld a, (exit_attrib)
	ld (attribexit+1), a

	ex de, hl
	ROWCOLTOSCRADDR

	ld (exitrow0+1), hl
	SCRTOATTRIBS h
	ld h, a
	ld (attribrow0+1), hl
	inc b
	ROWCOLTOSCRADDR
	ld (exitrow1+1), hl
	SCRTOATTRIBS h
	ld h, a
	ld (attribrow1+1), hl
	ex de, hl

	ld a, (hl); border colour

	out (#FE), a
	ld (bdr_colour), a

	inc hl
	ld a, (hl); paper colour
	and 7
	rlca
	rlca
	rlca
	or 7 ; white ink

	push hl
	ld hl, tile_attribs
	ld (hl), a
	and ~(7|64) ; just get paper colour and bright
	ld b, a
	and ~64 ; get rid of bright bit
	rrca
	rrca
	rrca
	or b ; same ink and paper
	call clear_playarea
	pop hl

	inc hl
	call put_level_name_unpacked

	; clear collision map
	push hl
	xor a
	ld hl, collision_map
	ld de, collision_map+1
	ld (hl), a
	ld bc, 511 ;
	ldir

	; clear guardian data
	ld hl, guardian0
	ld de, guardian0+1
	ld (hl), a
	ld bc, GUARDIAN_DATA*8-1
	ldir

	; clear erase lists
	; if these were next to each other could do it all in one go
	ld hl, erase8x16list
	ld de, erase8x16list+1
	ld (hl), a
	ld bc, 16

	ld hl, erase16xNlist
	ld de, erase16xNlist+1
	ld (hl), a
	ld bc, ERASE16xNLISTSIZE-1
	ldir

	pop hl

	ld a, (hl) ; exit gfx index
	and #3F ; mask out the guardian parameters

	push hl
	ld l, a
	ld h, 0

	ld de, EXITPAGE 

	REPT 5
	add hl, hl ; multiply by 32
	ENDR
	add hl, de
	ld (exitgfx0+1), hl
	pop hl

	call parse_guardian_data

	xor a
	ld de, key_positions
	ld (de), a

	inc hl
	ld a, (keydata)
	and MAXKEYS
	jr z, .nokeys
	ld c, a
	ld a, (keydata)
	and ~MAXKEYS
	rrca
	ld (keydata), a
	ld a, (hl)
	inc hl
	push hl
	ld l, a
	ld h, 0
	add hl, hl ; *2
	add hl, hl ; *4
	add hl, hl ; *8
	ld de, KEYPAGE
	add hl, de
	ld de, keygfx+1
	ex de, hl
	ld (hl), e
	inc hl
	ld (hl), d
	pop hl
	ld a, c ; remember num keys
	rl c ; multiply by 2
	ld b, 0
	ld de, key_positions
	ldir

	exx
	ld b, a
	ld hl, key_positions
.nextkey
	ld e, (hl)
	inc l
	ld d, (hl)
	push hl
	ld hl, ATTRIBS
	add hl, de
	ATTRIBSTOSCR h
	ld h, a
	ex de, hl
	pop hl
	ld (hl), e
	dec l
	ld (hl), d
	inc l
	inc l
	djnz .nextkey
	ld (hl), 0
	exx

.nokeys
	call parse_level_tiles

	; draw walls on each side - Manic Miner only
	ld bc, 0
	ld a, 16
	ld e, 1 ; wall tile
	push hl
	call vert_repeat_brush_nocheckfor0
	ld a, 16
	ld bc, 31
	call vert_repeat_brush_nocheckfor0
	pop hl

.nextbrush
	ld a, (hl)
	ld e, a ; brush index in e
	cp #FF
	jp z, .donebrushes
	cp EVENT_SOLAR
	jr nz, .notsolar
	call parse_solar_event
	jr .nextbrush
.notsolar
	cp EVENT_GFXCOPY
	jr nz, .notgfxcopy
	inc hl
	ex de, hl
	ld hl, gfx_copyevent
	ld (hl), e
	inc hl
	ld (hl), d
	ex de, hl
	ld a, 10 ; size of event data
	add l
	ld l, a
	jr nc, .okgfxcopy
	inc h
.okgfxcopy
	jr .nextbrush
.notgfxcopy
	cp EVENT_GAMEOVER
	jr nz, .notgameover
	xor a
	ld (tune_play), a
	ld a, GAMEOVERSTATE
	ld (game_state), a
	; change guardian0 gfx into the boot
	ld a, gfx_boot/256
	ld (guardian0+GUARDIAN_DATA.gfx), a
	ld a, gfx_boot&255
	ld (guardian0+GUARDIAN_DATA.gfx+1), a
	; set it to not erase
	ld de, guardian0+GUARDIAN_DATA.flags
	ld a, (de)
	or VGDONTERASEFLAG
	ld (de), a

	ld a, (willy_xpos)
	add 3
	ld (willy_xpos), a
	exx
	call update_willy_pos
	exx
	inc hl
	jr .nextbrush
.notgameover
	and ~15 ; mask out flags
	; Get the brush again
	ld a, (hl)
	jr nz, .repeat
	inc hl

.nextstroke
	ld c, (hl)
	inc hl
	ld a, (hl)
	and 31
	ld b, 0
	rrca
	rrca
	rrca ; *32, and carry bit
	bit 0, a
	jr z, .nohighbit
	and #FE
	inc b
.nohighbit
	or c
	ld c, a

	push hl
	ld hl, collision_map
	add hl, bc
	ld (hl), e
	pop hl

	ld a, (hl)
	inc hl
	and #80
	jr nz, .nextbrush
	jr .nextstroke

.repeat
	ld d, a
	and 15 ; mask out flags
	ld e, a ; brush index in e
	inc hl

.nextrptstroke
	ld c, (hl)
	inc hl
	ld a, (hl)
	ld ixl, a ; stash last flag
	and 31
	ld b, 0
	rrca
	rrca
	rrca ; *32, and carry bit
	bit 0, a
	jr z, .nohighbitrept
	and #FE
	inc b
.nohighbitrept
	or c
	ld c, a

	ld a, d
	and HORZ_REPEAT|VERT_REPEAT
	cp HORZ_REPEAT|VERT_REPEAT
	jr z, .hvrepeat

	inc hl
	ld a, (hl)

	bit HORZ_REPEAT_BIT, d
	push hl
	jr z, .vertrepeatbit
	call horz_repeat_brush
	jr .donerepeat
.vertrepeatbit
	call vert_repeat_brush
.donerepeat

	pop hl
	ld a, ixl
	and #80
	inc hl
	jr z, .nextrptstroke
	jp .nextbrush

.donebrushes
	inc hl
	ld a, (hl)
	inc a
	ex de, hl
	ld hl, lvl_next
	jr z, .lastlevel
.setlevelnext
	ld (hl), e
	inc hl
	ld (hl), d

	; clear any collision at the exit
	ld a, (exit_ypos)
	rlca ; *2
	rlca ; *4
	rlca ; *8
	rlca ; *16
	ld l, a
	ld h, 0
	add hl, hl
	ld a, (exit_xpos)
	add l
	ld l, a
	xor a
	ld de, collision_map
	add hl, de
	ld (hl), a
	inc l
	ld (hl), a
	ld a, 32
	add l
	ld l, a
	jr nc, .dontinch
	inc h
.dontinch
	xor a
	ld (hl), a
	dec l
	ld (hl), a

	jp draw_level
	ret

.lastlevel
	;ld de, Central_Cavern
	ld de, Game_Over
	jr .setlevelnext

.hvrepeat
	inc hl
	ld a, (hl)
	ld (.innerloopctr), a
	inc hl
	ld a, (hl)
	ld (.outerloopctr), a
	push hl
.nexthrepeat
	ld a, (.innerloopctr)
	push bc
	call horz_repeat_brush
	pop bc
	ld a, c
	add 32
	ld c, a
	ld a, b
	adc 0
	ld b, a
	ld a, (.outerloopctr)
	dec a
	ld (.outerloopctr), a
	jr nz, .nexthrepeat
	jr .donerepeat
.outerloopctr db 0
.innerloopctr db 0

horz_repeat_brush:
	or a
	jr nz, .reptloop
	ld a, 32
.reptloop
	push bc
	ld hl, collision_map
	add hl, bc
	ld (hl), e
	pop bc
	inc c
	dec a
	jr nz, .reptloop
	ret

vert_repeat_brush:
	or a
	jr nz, .donecheck
	ld a, 16
.donecheck
vert_repeat_brush_nocheckfor0:
	push bc
	ld hl, collision_map
	add hl, bc
	ld (hl), e
	pop bc
	ex af, af'
	ld a, 32
	add c
	ld c, a
	ld a, b
	adc 0
	ld b, a
	ex af, af'
	dec a
	jr nz, vert_repeat_brush_nocheckfor0
	ret

	DISPLAY "parse_unpacked_level size: ",/A, $-parse_unpacked_level

parse_level_tiles:
	ld de, tile_dist_table
	ld bc, 6
	ldir

	ld de, level_tiles+2 ; tile gfx pointer for tile1
	ld bc, tile_attribs+1 ; tile attrib for tile1
.nexttileentry
	ld a, (hl) ; It's the IPB colour, and the flash bit is set if it is the last tile for the level
	and #7F ; mask out high bit
	ld (bc), a ; place it in tile_attribs array
	inc c ; ok since these tables don't cross 256 byte boundary
	inc hl
	ld a, (hl) ; tile index in page / 8

	rlca
	rlca
	rla
	ld (de), a
	ld a, TILEPAGE/256 ; high byte of tile entry. May not need this, lookup is faster if only needs to read 1 byte...
	adc 0 ; up to 64 tiles/page then now cos of RLA above
	inc e ; ok since these tables don't cross 256 byte boundary
	ld (de), a
	inc e
	dec hl
	ld a, (hl) ; It's the IPB colour, and the flash bit is set if it is the last tile for the level
	bit 7, a
	inc hl ; next byte in level data
	inc hl ; next byte in level data
	jr z, .nexttileentry ; there's more to come
	ret

	DISPLAY "parse_level_tiles size: ",/A, $-parse_level_tiles

parse_solar_event:
	ex de, hl
	ld hl, has_solar
	inc (hl)
	ex de, hl
	inc hl ; event code
	ld a, (hl) ; starting location
	push hl
	call init_solar
	pop hl
	inc hl ; background colour
	ld a, (hl)
	ld (solar_bkg_colour0+1), a
	ld (solar_bkg_colour1+1), a
	inc hl ; beam colour
	ld a, (hl)
	ld (beam_colour0+1), a
	ld (beam_colour1+1), a
	inc hl ; next brush
	ret

parse_gfxcopy:
	push hl
	ld a, (gfx_copyevent+1) ; high byte will be non-zero if we have something to do
	or a
	jr z, .doret

	ld hl, (gfx_copyevent)
	ld a, (hl)
	exx
	ld h, a
	ld l, 0
	exx
	inc hl
	ld a, (hl)
	exx
	ld e, a
	exx
	inc hl
	ld a, (hl)
	exx
	ld d, a
	exx
	inc hl
	ld a, (hl)
	exx
	ld c, a
	exx
	inc hl
	ld a, (hl)
	exx
	ld b, a
	ldir
	exx
	inc hl
	ld (gfx_copyevent), hl

.doret
	pop hl
	ret

parse_guardian_data:
	ld ix, guardian0
	xor a
	ld (num_guardians), a
	ld a, (hl) ; exit and guardian flags
	ld b, a
	bit HAS_VGUARDIAN_BIT, a
	jr z, .novguardians

	ld d, VGUARDIANPAGE/256
	call get_guardian_type

.morevguardiandata

	call get_guardian_attribs

	; (hl) = initial ypos
	ld a, (hl)

	ld (ix+GUARDIAN_DATA.vgy), a

	; I shouldn't use exx here if I want to reserve that for bitstream data hmm
	exx
	ld e, a
	and ~7
	rrca
	rrca

	AROWX2TOSCRROWADDR

	ld a, e
	and 7
	add h
	ld h, a
	exx

	inc hl ; initial xpos and frame
	ld a, (hl) ; add on xpos
	and #3F ; mask out frame

	exx
	add l

	ld (ix+GUARDIAN_DATA.scraddr), a
	ld a, h
	ld (ix+GUARDIAN_DATA.scraddr+1), a
	exx

	ld a, (hl)
	and #C0 ; frame
	rrca ; frame*32
	ld (ix+GUARDIAN_DATA.frame), a

	inc hl ; miny
	ld a, (hl)
	ld (ix+GUARDIAN_DATA.minaddr), a

	inc hl ; maxy
	ld a, (hl)
	ld (ix+GUARDIAN_DATA.maxaddr), a

	inc hl ; speed/flags
	ld a, (hl)
	and VGVELMASK ; mask out flags
	REPT VGVELSHIFT
	sra a
	ENDR
	ld (ix+GUARDIAN_DATA.yvel), a

	ld a, (hl)
	and VGFLAGSMASK
	ld (ix+GUARDIAN_DATA.flags), a

	call bump_guardian_ptr
	jp z, .morevguardiandata

.novguardians
	bit HAS_HGUARDIAN_BIT, b
	;jr z, .nohguardians
	ret z

	ld d, HGUARDIANPAGE/256
	call get_guardian_type

.morehguardiandata

	call get_guardian_attribs

	ld e, (hl)
	inc hl ; xy and flags
	ld a, (hl)
	ld d, 0 ; flags
	bit (ISHALFSPEEDBIT-8), a
	jr z, .nothalfspeed
	set HALFSPEEDFLAGBIT, d
.nothalfspeed
	bit (IS4FRAMEBIT-8), a
	jr z, .not4frame
	set IS4FRAMEFLAGBIT, d
.not4frame
	ld (ix+GUARDIAN_DATA.flags), d
	and 7 ; mask out flags
	ld d, a
	push hl
	ld hl, ATTRIBS
	add hl, de


	ATTRIBSTOSCR h
	ld e, l
	ld d, a ; bag scraddr
	ld (ix+GUARDIAN_DATA.scraddr+1), a
	ld a, l
	ld (ix+GUARDIAN_DATA.scraddr), a

	pop hl

	ld a, (hl)
	and HGFRAMEMASK
	ld (ix+GUARDIAN_DATA.frame), a

	inc hl ; path min
	ld a, e
	and ~31
	ld e, a
	ld a, (hl)
	or e

	ld (ix+GUARDIAN_DATA.minaddr), a

	inc hl ; path max
	ld a, e
	and ~31
	ld e, a
	ld a, (hl)
	or e

	ld (ix+GUARDIAN_DATA.maxaddr), a

	call bump_guardian_ptr
	jp z, .morehguardiandata

.nohguardians

	ret

; D: guardian base page
get_guardian_type:
	inc hl ; guardian type

	ld a, (hl)
	and #7F ; mask out is last flag
	rra ; shift down one

	ld e, 0
	rr e ; carry in high bit
	;add HGUARDIANPAGE/256
	add d
	ld d, a
	ret

; DE - guardian gfx (from get_guardian type call)
get_guardian_attribs:
	ld (ix+GUARDIAN_DATA.gfx), d ; high byte of guardian pointer
	ld (ix+1+GUARDIAN_DATA.gfx), e ; low byte of guardian pointer

	ex de, hl
	ld hl, num_guardians
	inc (hl)
	ex de, hl

	inc hl ; attrib
	ld a, (hl)
	ld c, a ; last one in high bit
	and #7F ; mask out flag
	ld (ix+GUARDIAN_DATA.attrib), a

	inc hl
	ret

; set up pointer for reading next guardian data
bump_guardian_ptr:
	ld d, (ix+GUARDIAN_DATA.gfx) ; low byte of guardian pointer
	ld e, (ix+1+GUARDIAN_DATA.gfx) ; highbyte of guardian pointer

	push de
	ld de, GUARDIAN_DATA
	add ix, de
	pop de

	bit 7, c
	ret

	DISPLAY "parse_guardian_data size: ",/A, $-parse_guardian_data

draw_level:

	ld hl, collision_map
	ld bc, 0 ; position in col map

.nextcollcell
	ld a, (hl)
	or a
	jr z, .donedraw

	push de
	push bc
	push hl
	COLLMAPTOTILE
	call sprite8x8a
	pop hl
	pop bc
	pop de

.donedraw
	inc hl
	inc c
	ld a, c
	cp 32
	jr nz, .ok
	ld c, 0
	inc b
.ok
	bit 4, b
	jr z, .nextcollcell

	call parse_gfxcopy

	ld hl, ATTRIBS
	exx
	ld hl, collision_map
	ld bc, 0 ; position in col map

	halt ; wait for vblank

.nextattribcell
	ld a, (hl)
	COLLMAPTOTILEATTRIB
	exx
	ld (hl), a
	inc hl
	exx
	inc hl
	inc bc
	ld a, b
	cp #2
	jr nz, .nextattribcell

	call parse_gfxcopy

	IF FIXFINALIMAGE
	REPT 3, idx
	ld hl, ATTRIBS+(5+idx)*32+22
	ld de, ATTRIBS+(5+idx)*32+23
	ld (hl), 7
	ld bc, 8
	ldir
	ENDR

	REPT 3, idx
	ld hl, ATTRIBS+(5+idx)*32+19
	ld de, ATTRIBS+(5+idx)*32+20
	ld a, 7
	ld (hl), a
	ld (de), a
	ENDR

	xor a
	REPT 24, idx
	XYTOSCRADDRHLDE 22, 40+idx
	ld (hl), a
	ld bc, 8
	ldir
	ENDR

	REPT 24, idx
	XYTOSCRADDRHL 19, (40+idx)
	ld (hl), a
	inc hl
	ld (hl), a
	ENDR

.endless
	jr .endless
	ENDIF

	; look for conveyors
	ld hl, conveyorlist ; set this up now. We null terminate it later
	exx

	ld hl, tile_dist_table+TILEDISTTBL.lconveyoridx
	ld a, (hl)
	or a
	jr z, .noleftconvs
	ld d, a
	inc l
	jr .donesetup

.noleftconvs
	inc l
	ld a, (hl)
	or a
	jr z, .doneconvs ; no conveyors on this level
	ld d, a
.donesetup
	inc l
	ld e, (hl)

	ld hl, collision_map
	ld b, c ; position in col map. C was 0 at end of loop so this sets BC=0

.conveyorloop

	ld a, (hl)
	cp d
	jr c, .notconveyor
	cp e
	jr nc, .notconveyor

	push de
	push hl

	ld de, ATTRIBS-collision_map
	add hl, de

	ld d, a ; stash A = tileID

	ld a, (hl)
	;or #80 ; test - make it flash
	;ld (hl), a

	ATTRIBSTOSCR h
	ld h, a

	ld a, (tile_dist_table+TILEDISTTBL.lconveyoridx)
	or a ; any left conveyors?

	jr z, .isrightconveyor

	ld a, (tile_dist_table+TILEDISTTBL.rconveyoridx)
	or a ; any right conveyors?

	jr z, .isleftconveyor

	cp d
	jr nc, .isrightconveyor

.isleftconveyor
	ld a, (hl)
	inc h
	inc h
	jr .gotconveyordir

.isrightconveyor
	ld a, (hl)

.gotconveyordir
	ex af, af'
	ld a, (hl)
	ex af, af'

	push hl
	exx
	pop de
	ld (hl), e
	inc l
	ld (hl), d
	inc l
	ld (hl), a
	inc l
	;ld (hl), 1 ; set this later when we know the length of the conveyor
	inc l
	ex af, af'
	ld (hl), a

	dec l ; want HL' to point to count...
	exx

	pop hl

	ld e, 1 ; current conveyor count

.conveyorcountlength
	inc hl
	inc bc
	bit 1, b
	jr nz, .conveyorlasttileincmap
	ld a, (hl)
	cp d ; same as last tile?
	jr nz, .setconveyorcount
	inc e
	jr .conveyorcountlength

.setconveyorcount
	; set count of conveyor length here
	ld a, e
	exx
	ld (hl), a
	inc l
	inc l ; point to next entry
	exx

	pop de
	jr .conveyorloop

.conveyorlasttileincmap
	ld a, e
	exx
	ld (hl), a
	inc l
	inc l ; point to next entry
	exx

	pop de
	jr .doneconvs

.notconveyor
	inc hl
	inc bc
	bit 1, b
	jr z, .conveyorloop

.doneconvs
	exx
	ld (hl), c ; C is 0 here. Null terminate list

	ld a, #FF
	; draw air
	XYTOSCRADDRHLDE 4, 18*8+2
	ld (hl), a
	ld bc, 27
	ldir

	XYTOSCRADDRHLDE 4, 18*8+3
	ld (hl), a
	ld bc, 27
	ldir

	XYTOSCRADDRHLDE 4, 18*8+4
	ld (hl), a
	ld bc, 27
	ldir

	XYTOSCRADDRHLDE 4, 18*8+5
	ld (hl), a
	ld bc, 27
	ldir

	XYTOSCRADDRHLDE 4, 18*8+6
	ld (hl), a
	ld bc, 27
	ldir

	ret

	DISPLAY "draw_level size: ",/A, $-draw_level

update_guardians:
	IF TIMING
	SETBORDER 5
	ENDIF

	ld ix, guardian0

	ld a, (frame_counter)
	IF SILLYSPEED
	and 1
	ELSE
	and 3
	ENDIF
	ld b, a

	ld a, (ix+GUARDIAN_DATA.gfx)
	or a
	ret z ; no guardians on this level

	ld de, (erase16xNlistptr)

	cp vertical_guardians_page/256
	jr nc, .vertguardian

.dohorzguardians

	ld hl, (erase8x16listptr)
	ld c, 32

.updatenext

	ld a, (ix+GUARDIAN_DATA.flags)

	bit HALFSPEEDFLAGBIT, a
	jr z, .nothalfspeed
	ld a, b
	or a
	jr nz, .donemove
.nothalfspeed

	ld a, (ix+GUARDIAN_DATA.frame)
	cp 128
	jr nc, .movingleft
	cp 96 ; about to change cell?
	jr c, .oktoincreaseanimright
	; are we at end of path?
	ld a, (ix+GUARDIAN_DATA.maxaddr)
	cp (ix+GUARDIAN_DATA.scraddr)
	jr nz, .notatendofpathright
	; don't need to erase since we just turn around in same position
	ld (ix+GUARDIAN_DATA.frame), 7*32
	jr .donemove
.notatendofpathright
	ld a, (ix+GUARDIAN_DATA.scraddr)
	ld (hl), a
	inc l
	ld a, (ix+GUARDIAN_DATA.scraddr+1)
	ld (hl), a
	inc l

	inc (ix+GUARDIAN_DATA.scraddr)
	ld (ix+GUARDIAN_DATA.frame), 0
	jr .donemove
.oktoincreaseanimright
	;add 32
	add c
	ld (ix+GUARDIAN_DATA.frame), a
	jr .donemove

.movingleft
	cp 128 ; about to change cell?
	jr nz, .oktoincreaseanimleft
	; are we at end of path?
	ld a, (ix+GUARDIAN_DATA.minaddr)
	cp (ix+GUARDIAN_DATA.scraddr)
	jr nz, .notatendofpathleft
	ld (ix+GUARDIAN_DATA.frame), 0
	jr .donemove
.notatendofpathleft

	ld a, (ix+GUARDIAN_DATA.scraddr)
	inc a
	ld (hl), a
	inc l
	ld a, (ix+GUARDIAN_DATA.scraddr+1)
	ld (hl), a
	inc l

	dec (ix+GUARDIAN_DATA.scraddr)
	ld (ix+GUARDIAN_DATA.frame), 7*32
	jr .donemove
.oktoincreaseanimleft
	;sub 32
	sub c
	ld (ix+GUARDIAN_DATA.frame), a
	;jr .donemove
.donemove
	ld a, GUARDIAN_DATA
	add ixl
	ld ixl, a

	ld a, (ix+GUARDIAN_DATA.gfx)
	or a
	jp z, .doret ; done
	jp .updatenext

.vertguardian
	bit 1, b
	jr nz, .vgnoanimupdate

	ld a, (ix+GUARDIAN_DATA.flags)
	and ~VGDONTERASEFLAG
	;or a
	jr z, .noflags
	dec a ; is it EUGENE?
	jr z, .vgnoanimupdate ; don't animate Eugene
	dec a ; is it KONG?
	jr nz, .notkong
	ld a, (frame_counter)
	and 31
	jr nz, .vgnoanimupdate ; Kong animates slow speed
	jr .noflags
.notkong
	cp 2
	jr nz, .vgnoanimupdate
	ld a, (ix+GUARDIAN_DATA.frame)
	add 32
	ld (ix+GUARDIAN_DATA.frame), a
	jr nz, .vgnoanimupdate
	ld a, VGSKYLAB
	ld (ix+GUARDIAN_DATA.flags), a
	ld a, (ix+GUARDIAN_DATA.minaddr)
	ld (ix+GUARDIAN_DATA.vgy), a

	exx
	ld hl, (erase8x16listptr)

	ld a, (ix+GUARDIAN_DATA.scraddr)
	ld e, a
	ld (hl), a
	inc l
	ld a, (ix+GUARDIAN_DATA.scraddr+1)
	ld d, a
	ld (hl), a
	inc l
	inc e
	ld (hl), e
	inc l
	ld (hl), d
	inc l
	ld (erase8x16listptr), hl

	ld a, (ix+GUARDIAN_DATA.vgy)

	ld c, a
	and ~7
	rrca
	rrca
	AROWX2TOSCRROWADDR
	ld a, (ix+GUARDIAN_DATA.scraddr)
	add 8
	and 31
	add l

	ld (ix+GUARDIAN_DATA.scraddr), a

	ld a, c
	and 7
	add h

	ld (ix+GUARDIAN_DATA.scraddr+1), a
	exx


	jp .doneupdateeraselist

.noflags
	ld a, (ix+GUARDIAN_DATA.frame)
	add 32
	and #7F
	ld (ix+GUARDIAN_DATA.frame), a
.vgnoanimupdate

	ld a, (ix+GUARDIAN_DATA.vgy)
	ld l, a ; remember old vgy
	ld c, (ix+GUARDIAN_DATA.yvel)
	ld h, c ; remember yvel
	add c

	cp (ix+GUARDIAN_DATA.minaddr)
	jp c, .vgchangedir
	cp (ix+GUARDIAN_DATA.maxaddr)
	jp nc, .vgchangedir

.setvgpos
	ld (ix+GUARDIAN_DATA.vgy), a

	push hl
	ld c, a
	and ~7
	rrca
	rrca
	AROWX2TOSCRROWADDR
	ld a, (ix+GUARDIAN_DATA.scraddr)
	and 31
	add l

	ld (ix+GUARDIAN_DATA.scraddr), a

	ld a, c
	and 7
	add h

	ld (ix+GUARDIAN_DATA.scraddr+1), a

	pop hl

	bit 7, h
	jr z, .dontadjustformovingup

	ld a, l
	add 16
	add h
	ld l, a

.dontadjustformovingup

	ld a, (ix+GUARDIAN_DATA.flags)
	bit 7, a
	jr nz, .doneupdateeraselist

	ld a, l
	ld c, a

	and ~7
	rrca
	rrca
	AROWX2TOSCRROWADDR
	ld a, (ix+GUARDIAN_DATA.scraddr)
	and 31
	add l
	ld l, a

	ld a, c
	and 7
	ld c, a
	add h
	ld h, a

	ld a, (ix+GUARDIAN_DATA.yvel)
	or a
	jr z, .doneupdateeraselist
	jp p, .dontflipvel
	neg
.dontflipvel

	push de
	ld e, a
	ld a, 8
	sub c
	cp e
	jr nc, .only1cell
	ld d, a
	ld a, e
	sub d
	ld (.span2), a
	ld a, d
	ld (.span1), a
	pop de

	ex de, hl
	ld (hl), e
	inc l
	dec d ; we store the high byte - 1 since in the loop we preinc it
	ld (hl), d
	inc l
	ld a, (.span1)
	ld (hl), a
	inc l

	ex de, hl

	ld a, h
	and ~7
	ld h, a
	; move to next cell down. Why are the flags and the sub reversed here??!
	; it works this way around anyway
	ld a, l			; 4T calculate screen address to start of next 8 lines
	add #20			; 7T add 32 to screen pointer
	ld l, a			; 4T
	jr nc, .eraseaddr_ok
	ld a, h			; 4T (nc)
	add 8			; 7T (nc)
	ld h, a			; 4T = 37T if jump not taken
.eraseaddr_ok			; 27T if jump was taken

	ex de, hl
	ld (hl), e
	dec d ; we store the high byte - 1 since in the loop we preinc it
	inc l
	ld (hl), d
	inc l
	ld a, (.span2)
	ld (hl), a
	inc l
	ex de, hl

	jr .doneupdateeraselist
.span1
	db 0
.span2
	db 0
.only1cell
	ld a, e ; erase this many in 1st cell
	pop de

	ex de, hl
	ld (hl), e
	inc l
	dec d ; we store the high byte - 1 since in the loop we preinc it
	ld (hl), d
	inc l
	ld (hl), a
	inc l
	ex de, hl

.doneupdateeraselist
.dontupdatevgpos
	ld a, GUARDIAN_DATA
	add ixl
	ld ixl, a

	ld a, (ix+GUARDIAN_DATA.gfx)
	or a
	jr z, .seteraselistptrdoret ; done

	cp vertical_guardians_page/256
	jp c, .dohorzguardians

	jp .vertguardian

.vgchangedir
	ld a, (ix+GUARDIAN_DATA.flags)
	or a
	jr z, .notspecialvg
	cp VGSKYLAB
	jr nz, .dontupdatevgpos
	inc (ix+GUARDIAN_DATA.flags) ; set to crashing (4)
	ld a, (ix+GUARDIAN_DATA.maxaddr)
	ld (ix+GUARDIAN_DATA.vgy), a

	jp .setvgpos

.notspecialvg
	ld a, c
	neg
	ld (ix+GUARDIAN_DATA.yvel), a
	ld a, (ix+GUARDIAN_DATA.vgy)
	jr .dontupdatevgpos

.seteraselistptrdoret
	ld hl, (erase8x16listptr)
.doret
	; null terminate lists
	xor a
	ld (hl), a
	ld (erase8x16listptr), hl
	ld (de), a
	ld (erase16xNlistptr), de

; clear and rebuild guardian cell occupation bitmap
	ld hl, guardian_occupycell_bitmap
	ld de, guardian_occupycell_bitmap+1
	ld (hl), a
	ld bc, 63

	IF TIMING
	SETBORDER 4
	ENDIF

	ldir

	ld a, (num_guardians)
	or a
	ret z ; no guardians, don't bother doing this

	ld h, guardian_occupycell_bitmap/256
	ld d, shift_tbl/256
	exx
	ld b, a

	ld hl, guardian0+GUARDIAN_DATA.scraddr

.occupycellsloop
	ld e, (hl)
	inc l
	ld d, (hl)
	ld a, GUARDIAN_DATA.vgy - (GUARDIAN_DATA.scraddr+1)
	add l
	ld l, a
	ld c, (hl) ; stash y position if vertical guardian

	SCRTOATTRIBS d
	ld d, a
	ld a, e
	and 7
	ex af, af'
	rr d
	rr e
	ld a, e
	and ~3
	rrca
	rrca

	exx
	ld l, a
	ex af, af'
	ld e, a
	ld a, (hl)
	ex de, hl
	ld c, (hl) ; shift table entry
	or c
	ex de, hl
	dec c
	jr nz, .ok1
	inc l
	set 7, (hl)
	dec l
.ok1
	inc c
	ld (hl), a
	ld a, l
	add 4
	ld l, a
	ld a, (hl)
	or c
	dec c
	jr nz, .ok2
	inc l
	set 7, (hl)
	dec l
.ok2
	inc c
	ld (hl), a
	exx
	ld a, c
	and 7
	jr z, .not3tall
	exx
	ld a, l
	add 4
	ld l, a
	ld a, (hl)
	or c
	dec c
	jr nz, .ok3
	inc l
	set 7, (hl)
	dec l
.ok3
	ld (hl), a
	exx

.not3tall


	dec b
	ret z
	ld a, GUARDIAN_DATA + GUARDIAN_DATA.scraddr - GUARDIAN_DATA.vgy
	add l
	ld l, a
	jr .occupycellsloop

	IF DBG_OCCUPIED
dbg_show_occupied_cells:
	ld ix, ATTRIBS
	ld hl, guardian_occupycell_bitmap
	ld b, 64
	ld de, 8
.loop
	ld c, (hl)
	bit 7, c
	jr z, .unoccupied0
	ld a, (ix+0)
	xor #8
	ld (ix+0), a
.unoccupied0
	bit 6, c
	jr z, .unoccupied1
	ld a, (ix+1)
	xor #8
	ld (ix+1), a
.unoccupied1
	bit 5, c
	jr z, .unoccupied2
	ld a, (ix+2)
	xor #8
	ld (ix+2), a
.unoccupied2
	bit 4, c
	jr z, .unoccupied3
	ld a, (ix+3)
	xor #8
	ld (ix+3), a
.unoccupied3
	bit 3, c
	jr z, .unoccupied4
	ld a, (ix+4)
	xor #8
	ld (ix+4), a
.unoccupied4
	bit 2, c
	jr z, .unoccupied5
	ld a, (ix+5)
	xor #8
	ld (ix+5), a
.unoccupied5
	bit 1, c
	jr z, .unoccupied6
	ld a, (ix+6)
	xor #8
	ld (ix+6), a
.unoccupied6
	bit 0, c
	jr z, .unoccupied7
	ld a, (ix+7)
	xor #8
	ld (ix+7), a
.unoccupied7
	add ix, de
	inc l
	djnz .loop
	ret
	ENDIF

put_level_name_unpacked:
	; get the name of the level and tokenise it
	ld de, tokenise_buff
	call tokenise_string

	ld de, TEMPNAMEBUFF

	push hl
	ld hl, tokenise_buff
	call expand_measurepr_tokenised_string
	push hl
	srl c
	ld a, 128
	sub c
	push af ; stash width
	and 7
	ld (textxoffset), a
	ld hl, TEMPNAMEBUFF
	call put_strz
	pop af
	pop hl
	push hl
	IF 0
	ld b, 131
	ELSE
	ld b, 130
	ENDIF
	ld c, a
	call put_string_at
	pop hl

	pop hl
	ret

	DISPLAY "put_level_name_unpacked size: ",/A, $-put_level_name_unpacked

update_willy:
	ld h, level_tiles/256
	ld de, redrawcellslist
	exx

	ld de, erasecellslist
	ld hl, (willy_colpos)
	ld bc, collision_map
	add hl, bc

	ld a, (hl)
	or a
	jr nz, .tlnotblank
	
	ADDTOERASECELLSLIST
	jp .topright

.tlnotblank

	ADDTOREDRAWCELLSLIST

.topright

	inc l

	ld a, (hl)
	or a
	jr nz, .trnotblank
	
	ADDTOERASECELLSLIST
	jp .midleft

.trnotblank
	ADDTOREDRAWCELLSLIST

.midleft

	ld a, 31
	add l
	ld l, a
	jr nc, .addrokmid
	inc h
.addrokmid

	ld a, (hl)
	or a
	jr nz, .midlnotblank
	
	ADDTOERASECELLSLIST
	jp .midright

.midlnotblank
	ADDTOREDRAWCELLSLIST

.midright
	inc l

	ld a, (hl)
	or a
	jr nz, .midrnotblank
	
	ADDTOERASECELLSLIST
	jp .done2rows

.midrnotblank
	ADDTOREDRAWCELLSLIST

.done2rows
	xor a
	ld (de), a

	exx
	ld (de), a

	call read_keyboard
	ld hl, willy_xpos
	ld a, (hl)
	add c
	cp 8
	jr c, .doneupatepos
	cp 240
	jr nc, .doneupatepos

	ld (hl), a
.doneupatepos

update_willy_pos:
	ld d, gfx_gnewwilly0/256

	ld a, (willy_facing)
	dec a
	jr z, .setanimframe
	inc d

.setanimframe
	ld a, (willy_xpos)
	and 7
	rrca
	rrca
	rrca ; *32
	ld e, a

	ld (draw_willy_2rows+1), de

	ld hl, (willy_colpos)
	ld a, l
	and ~31
	ld l, a

	ld a, (willy_xpos)
	and ~7
	rrca
	rrca
	rrca
	or l
	ld l, a

	ld (willy_colpos), hl

	ld de, ATTRIBS
	add hl, de
	ATTRIBSTOSCR h
	ld h, a

	ld (willyrow0addr+1), hl

	ld hl, (willy_colpos)
	ld de, ATTRIBS+32
	add hl, de
	ATTRIBSTOSCR h
	ld h, a

	ld (willyrow1addr+1), hl

	ret

; at exit, C contains 1 if we pressed right and -1 if we pressed left
read_keyboard:
	; Read these ports to scan keyboard
	; bit N (0-based) is clear if the key is being pressed
	; #FE - SHIFT, Z, X, C, & V
	; #FD - A, S, D, F, & G
	; #FB - Q, W, E, R, & T
	; #F7 - 1, 2, 3, 4, & 5
	; #EF - 0, 9, 8, 7, & 6
	; #DF - P, O, I, U, & Y
	; #BF - ENTER, L, K, J, & H
	; #7F - SPACE, FULL-STOP, M, N, & B
	; ld a, port
	; in a, (#FE)
	; to do the read of the port

	ld bc, 0

	; are we pressing W?
	ld a, #FB
	in a, (#FE)
	bit 1, a
	jr nz, .notpressingW
	inc c
.notpressingW
	; are we pressing Q?
	bit 0, a
	jr nz, .notpressingQ
	dec c
.notpressingQ
	ld a, c
	or a
	ret z ; not moving
	ld hl, willy_facing
	ld (hl), c
	ret


; A: starting cell for solar
init_solar:
	MEMCLEAR solar_power_erase0, SOLAR_DATA_LEN

	ld (solar_startcell), a
	and ~7
	rrca
	rrca
	rrca
	ld (solar_occupyindex), a

	ld c, #80
	ld a, (solar_startcell)
	and 7
	jr z, .dontadjustmask
	ld b, a
.shiftloop
	rrc c
	djnz .shiftloop
.dontadjustmask
	ld a, c

	ld (solar_startmask), a
	xor a
	ld (solar_direction), a

	dec a ; A <- #FF
	ld hl, solar_power_erase0
	ld (hl), a ; terminate
	ld hl, solar_power_erase1
	ld (hl), a ; terminate

	ld hl, solar_power_draw0
	ld (hl), a ; terminate
	ld hl, solar_power_draw1
	ld (hl), a ; terminate

	ret

do_solar_down_left:
	IF TIMING
	SETBORDER 6
	ENDIF
	
	ld h, collision_map/256
	exx

	ld d, guardian_occupycell_bitmap/256
	ld hl, solar_occupyindex
	ld e, (hl)
	ld hl, solar_startmask
	ld b, (hl)
	ld hl, solar_direction
	ld c, (hl)
	ld hl, (solar_draw_upper+1)

	ld a, (solar_startcell)

.loopupper
	exx
	ld b, a
	ld l, a
	ld a, (hl)
	or a
	ld a, b
	exx
	jr nz, .doupperret
	
	ld (hl), a
	ex af, af'
	inc l

.movingdown
	ld a, (de)
	and b
	jr z, .keepgoing
	inc c
.keepgoing
	bit 0, c
	jr nz, .moveleft
	ld a, e
	add 4
	ld e, a
	ex af, af'
	add 32
	jr nc, .loopupper
	jr .dolower

.moveleft
	rlc b
	jr nc, .doneeadjust
	dec e
.doneeadjust
	ex af, af'
	dec a
	exx
	ld b, a
	and 31
	ld a, b
	exx
	jr nz, .loopupper
.doupperret
	ld (hl), 255
	ld hl, (solar_draw_lower+1)

	jr .doret

.dolower
	ld (hl), 255

	ld hl, (solar_draw_lower+1)
	exx
	inc h
	exx

.looplower
	exx
	ld b, a
	ld l, a
	ld a, (hl)
	or a
	ld a, b
	exx
	jr nz, .doret
	
	ld (hl), a
	ex af, af'
	inc l

.movingdownlower
	ld a, (de)
	and b
	jr z, .keepgoinglower
	inc c
.keepgoinglower

	bit 0, c
	jr nz, .moveleftlower
	ld a, e
	add 4
	ld e, a
	ex af, af'
	add 32
	jr nc, .looplower
	jr .doret

.moveleftlower
	rlc b
	jr nc, .doneeadjustlower
	dec e
.doneeadjustlower
	ex af, af'
	dec a
	exx
	ld b, a
	and 31
	ld a, b
	exx
	jr nz, .looplower

.doret
	ld (hl), 255

	ret

; B: row (in character cells, so [0-23])
; C: column
; DE: 8 rows 8x1 sprite data (8 byte aligned)
sprite8x8a:
	ld a, b
	add b ; A = B*2
	ld h, tbl_rows/256 ; high byte of screen address lookup table. Aligned 256 so low byte will be just row*2
	ld l, a ; index into table 
	ld a, (hl) ; low byte of screen address
	inc l ; point HL to high byte of screen address
	ld h, (hl) ; read high byte of screen address
	add c ; add on column to low byte of screen address
	ld l, a ; and write it back. HL now holds correct screen address
	; so we now know the address...
; HL: screen address
; DE: 8 rows 8x1 sprite data (8 byte aligned)
sprite8x8aKnowAddr:
	REPT 7
		ld a, (de)
		inc e ; ok because gfx data is 8 byte aligned

		ld (hl), a
		inc h ; next row of pixels down
	ENDR

	; last row, don't need to increment gfx data pointer or screen row
	ld a, (de)
	ld (hl), a
	ret

	; A -> attrib to set when clearing screen
clear_playarea:
	; set attribs
	ld hl, #5800
	ld de, #5800+1
	ld bc, 511 ; only set attribs of top 2/3 of screen
	ld (hl), a
	ldir

	; clear pixels
	ld hl, #4000
	ld de, #4001
	ld (hl), b ; since B=0 here
	ld bc, 4096-1
	ldir
	ret

; B: row (in character cells, so [0-23])
; C: column
erase8x8a:
	ld a, b
	add b ; A = B*2
	ld h, tbl_rows/256 ; high byte of screen address lookup table. Aligned 256 so low byte will be just row*2
	ld l, a ; index into table 
	ld a, (hl) ; low byte of screen address
	inc l ; point HL to high byte of screen address
	ld h, (hl) ; read high byte of screen address
	add c ; add on column to low byte of screen address
	ld l, a ; and write it back. HL now holds correct screen address
	; so we now know the address...
; HL: screen address
erase8x8aKnowAddr:
	xor a

	REPT 7
		ld (hl), a
		inc h ; next row of pixels down
	ENDR

	ld (hl), a
	ret

draw_willy_2rows:
	ld sp, 0 ; SMC

willyrow0addr:
	ld hl, 0 ; SMC

	REPT 4, idx
	pop de								; 10T	
	ld a, (hl)							; 7T	17T
	or e								; 4T	21T
	ld (hl), a							; 7T	28T
	inc l								; 4T	32T
	ld a, (hl)							; 7T	39T
	or d								; 4T	43T
	ld (hl), a							; 7T	50T
	inc h								; 4T	54T
	pop de								; 10T	64T
	ld a, (hl)							; 7T	71T
	or d								; 4T	75T
	ld (hl), a							; 7T	82T
	dec l								; 4T	86T
	ld a, (hl)							; 7T	93T
	or e								; 4T	97T
	ld (hl), a							; 7T	104T
	IF idx < 3
	inc h								; 4T	108T
	ENDIF
	ENDR								; 14T + 108T*4 = 446T

willyrow1addr:
	ld hl, 0 ; SMC

	REPT 4, idx
	pop de								; 10T	
	ld a, (hl)							; 7T	17T
	or e								; 4T	21T
	ld (hl), a							; 7T	28T
	inc l								; 4T	32T
	ld a, (hl)							; 7T	39T
	or d								; 4T	43T
	ld (hl), a							; 7T	50T
	inc h								; 4T	54T
	pop de								; 10T	64T
	ld a, (hl)							; 7T	71T
	or d								; 4T	75T
	ld (hl), a							; 7T	82T
	dec l								; 4T	86T
	ld a, (hl)							; 7T	93T
	or e								; 4T	97T
	ld (hl), a							; 7T	104T
	IF idx < 3
	inc h								; 4T	108T
	ENDIF
	ENDR								; 14T + 108T*4 = 446T

	jp donedrawwilly

draw_horz_guardian:

	ld b, l
	ld a, GUARDIAN_DATA -  GUARDIAN_DATA.attrib; offset to next guardian data
	add l
	ld b, a ; should point at next guardian

	ex de, hl

	REPT 4
	pop de								; 10T	
	ld (hl), e							; 7T
	inc l								; 4T
	ld (hl), d							; 7T
	pop de								; 10T
	inc h								; 4T
	ld (hl), d							; 7T
	dec l								; 4T
	ld (hl), e							; 7T
	inc h								; 4T
	ENDR								; 

	; move to next cell down
	ld a, l			; 4T calculate screen address to start of next 8 lines
	add #20			; 7T add 32 to screen pointer
	ld l, a			; 4T
	jr c, .addr_ok	; 7T/12T jump if addition resulted in overflow (this means we are at next 64 line segment)
	ld a, h			; 4T (nc)
	sub 8			; 7T (nc) subtract 8*256
	ld h, a			; 4T = 37T if jump not taken
.addr_ok			; 27T if jump was taken

	REPT 4, idx
	pop de								; 10T	
	ld (hl), e							; 7T
	inc l								; 4T
	ld (hl), d							; 7T
	pop de								; 10T
	inc h								; 4T
	ld (hl), d							; 7T
	dec l								; 4T
	ld (hl), e							; 7T
	IF idx < 3
	inc h								; 4T
	ENDIF
	ENDR								; 

	SCRTOATTRIBS h
	ld h, a ; HL should be attribs address now 
	ld (hl), c
	inc l
	ld (hl), c
	ld a, l
	sub 32
	ld l, a
	jr nc, .dontadjusth
	dec h
.dontadjusth
	ld (hl), c
	dec l
	ld (hl), c

	ld h, guardian0/256
	ld l, b

	jp drawnextguardian

draw_vert_guardian:
	; do this in the guardian draw loop and jump to right place?
	bit 0, a
	jr nz, .drawoddrow

	; do these calcs in the update to save more time here?
	rrca

	; A has row offset/2 (1-3)
	ld b, a
	ex af, af'
	ld a, 4
	sub b
	ld b, a

	ld a, GUARDIAN_DATA -  GUARDIAN_DATA.attrib; offset to next guardian data
	add l
	ld (.restorehl+1), a

	ex de, hl

.firstrowsloop
	DRAWSPRITE2LINES
	djnz .firstrowsloop

	ld a, l
	add 32
	ld l, a
	jr c, .ok1
	ld a, h
	sub 8
	ld h, a
.ok1

	REPT 4
	DRAWSPRITE2LINES
	ENDR

	ld a, l
	add 32
	ld l, a
	jr c, .ok2
	ld a, h
	sub 8
	ld h, a
.ok2

	ex af, af'
	ld b, a
.lastrowsloop
	DRAWSPRITE2LINES
	djnz .lastrowsloop

	jp .doattribs

.drawoddrow

	dec a ; make it even
	rrca

	; A has row offset/2 (1-3)
	ld b, a

	ex af, af'
	ld a, 4
	sub b
	ld b, a
	
	ld a, GUARDIAN_DATA -  GUARDIAN_DATA.attrib; offset to next guardian data
	add l
	ld (.restorehl+1), a ; SMC

	ex de, hl

	dec b
	jr z, .nofirstrowloop

	; unroll and jump to right place? To avoid djnz overhead
.firstoddrowsloop
	DRAWSPRITE2LINES
	djnz .firstoddrowsloop

.nofirstrowloop

	; draw last line
	pop de								; 10T	
	ld (hl), e							; 7T
	inc l								; 4T
	ld (hl), d							; 7T
	inc h								; 4T

	ld a, l
	add 32
	ld l, a
	jr c, .okodd1
	ld a, h
	sub 8
	ld h, a
.okodd1

	REPT 4
	pop de								; 10T
	ld (hl), d							; 7T
	dec l								; 4T
	ld (hl), e							; 7T
	inc h								; 4T
	pop de								; 10T
	ld (hl), e							; 7T
	inc l								; 4T
	ld (hl), d							; 7T
	inc h								; 4T
	ENDR

	ld a, l
	add 32
	ld l, a
	jr c, .okodd2
	ld a, h
	sub 8
	ld h, a
.okodd2

	ex af, af'
	or a
	jr z, .onlydrawlast
	ld b, a

.lastoddrowsloop
	pop de								; 10T
	ld (hl), d							; 7T
	dec l								; 4T
	ld (hl), e							; 7T
	inc h								; 4T
	pop de								; 10T
	ld (hl), e							; 7T
	inc l								; 4T
	ld (hl), d							; 7T
	inc h								; 4T
	djnz .lastoddrowsloop

.onlydrawlast
	pop de								; 10T
	ld (hl), d							; 7T
	dec l								; 4T
	ld (hl), e							; 7T

.doattribs
	SCRTOATTRIBS h
	ld h, a ; HL should be attribs address now 

	ld (hl), c
	inc l
	ld (hl), c
	ld a, l
	sub 33
	ld l, a
	jr nc, .draw2attribs
	dec h
.draw2attribs
	ld (hl), c
	inc l
	ld (hl), c
	ld a, l
	sub 32
	ld l, a
	jr nc, .dontadjusth
	dec h
.dontadjusth
	ld (hl), c
	dec l
	ld (hl), c

.restorehl
	ld hl, guardian0 ; SMC
	jp drawnextguardian

	DISPLAY "draw_vert_guardian size: ", /A, $-draw_vert_guardian

; HL: screen address
blitsprite16x16:
	ld (.restoresp+1), sp
	di

	ex de, hl							; 4T	
	ld sp, hl							; 6T	10T
	ex de, hl							; 4T	14T

	REPT 4
	pop de								; 10T	
	ld (hl), e
	inc l								; 4T	32T
	ld (hl), d
	inc h								; 4T	54T
	pop de								; 10T	64T
	ld (hl), d
	dec l								; 4T	32T
	ld (hl), e
	inc h								; 4T	54T
	ENDR								; 14T + 108T*4 = 446T

	; we could split this into 2 routines depending on whether we straddle a screen third boundary or not
	; which we could work out before calling
	; then we could remove the jr c
	ld a, #20							; 7T		453T
	add l								; 4T		457T
	ld l, a								; 4T		461T
	jr c, .ok							; 12T/7T	468T/480T
	ld a, h								; 4T		472T
	sub #8								; 7T		479T
	ld h, a								; 4T		483T
.ok

	; 483T if cross boundary, 480T otherwise

	REPT 4, idx
	pop de								; 10T	
	ld (hl), e
	inc l								; 4T	32T
	ld (hl), d
	inc h								; 4T	54T
	pop de								; 10T	64T
	ld (hl), d
	dec l								; 4T	32T
	ld (hl), e
	IF idx < 3
	inc h								; 4T
	ENDIF
	ENDR								;

.restoresp
	ld sp, 0
	ei
	ret

;currentstream_handle dw packed_data_test
;currentstream_last dw packed_data_test+5
;currentstream_bit db 0
;currentstream_lastbit db 5
;packed_data_test db %010'00001, %0'00011'00, %0101'0010, %11'00110'0, %01000'001, %01001 ; 1, 2, 3, 4, 5, 6, 7, 8, 9 packed into 5 bits each

;stream_place_test BLOCK 256

TOKEN_SPACE			EQU 0
TOKEN_LEADINGTHE	EQU 0
TOKEN_QUESTIONMARK	EQU	27
TOKEN_APOSTROPHE	EQU 28
TOKEN_OFTHE			EQU 29
TOKEN_MEETSTHE		EQU 30
TOKEN_ENDOFSTRING	EQU 31

	ALIGN 32
strOfThe
	dc " of the "
strMeetsThe
	dc " meets the "
strThe
	dc "The "

capsadjust db 0

	ALIGN 256

_00	EQU 0
_RB	EQU 128|64|(2<<3)|0
_BM	EQU 128|64|(0<<3)|3
_GB	EQU 128|64|(4<<3)|0
_YB	EQU 128|64|(6<<3)|0
_RC	EQU 128|64|(2<<3)|5
_BC	EQU 128|64|(0<<3)|5
_BY	EQU 128|64|(0<<3)|6
_BR	EQU 128|64|(0<<3)|2
_MB	EQU 128|64|(3<<3)|0
_CY	EQU 128|64|(5<<3)|6
_CB	EQU 128|64|(5<<3)|0
_BG	EQU 128|64|(0<<3)|4
_MG	EQU 128|64|(3<<3)|4
_YM	EQU 128|64|(6<<3)|3
_GR	EQU 128|64|(4<<3)|2

collision_map:
	; splash screen attributes
	db _00, _00, _00, _00, _RB, _RB, _RB, _RB, _00, _00, _BM, _BM, _BM, _00, _GB, _GB, _GB, _00, _00, _BY, _00, _00, _00, _00, _00, _00, _00, _00, _00, _00, _00, _00
	db _00, _00, _00, _RB, _00, _00, _00, _00, _00, _YB, _00, _BM, _00, _00, _BR, _GB, _00, _00, _00, _BY, _00, _00, _00, _00, _MB, _00, _00, _00, _MB, _00, _00, _00
	db _00, _00, _00, _00, _RC, _RB, _RB, _00, _BC, _YB, _00, _BM, _00, _00, _BR, _GB, _00, _00, _00, _CY, _CB, _CB, _00, _BG, _MB, _00, _00, _MG, _00, _00, _00, _00
	db _00, _00, _00, _00, _BC, _00, _00, _RB, _BC, _YB, _00, _BM, _00, _00, _BR, _GB, _00, _00, _CB, _BY, _00, _00, _CB, _00, _MG, _MB, _MG, _00, _00, _00, _00, _00
	db _00, _00, _00, _RB, _RC, _RB, _RC, _00, _BC, _YB, _BM, _BM, _BM, _00, _GR, _GB, _GB, _00, _CB, _BY, _BY, _BY, _BY, _00, _MB, _BG, _00, _MB, _00, _00, _00, _00
	db _00, _00, _00, _00, _BC, _00, _BC, _00, _BC, _YB, _YB, _YB, _YB, _00, _BR, _BR, _BR, _BR, _CB, _00, _00, _00, _CB, _00, _MB, _BG, _00, _00, _MB, _00, _00, _00
	db _00, _00, _00, _00, _00, _BC, _00, _BC, _00, _00, _00, _00, _00, _00, _00, _00, _00, _00, _00, _CB, _CB, _CB, _00, _00, _00, _BG, _00, _00, _00, _00, _00, _00

	DISPLAY "BLOCK ", /A, 512 - ($-collision_map)
	BLOCK 512 - ($-collision_map)

	ALIGN 256
proportional_font	;db #00, #00, #00, #00, #00, #00, #00, #00 ; space
	db 4;3;5 ; width

	IF MAYBE8TALL
	dw proportional_font-256+'j'*8+8;7 ; address of last row+1 of character in font
	dg ##......
	dw proportional_font-256+127*8+8;7 ; (C) address of last row+1
	dg ..####..
	db 0 ; 1 byte left over
	ELSE
	BLOCK 7 ; this is used for special case chars table, not for the image of the space char
	ENDIF

	;db #18, #18, #18, #18, #18, #00, #18, #00 ; !
	db 2 ; width
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	dg ........
	dg ##......

	;db #6C, #6C, #6C, #00, #00, #00, #00, #00 ; "
	db 5|ASCENDER ; width
	dg ##.##...
	dg ##.##...
	dg ##.##...
	dg ........
	dg ........
	dg ........
	dg ........

	;db #6C, #6C, #FE, #6C, #FE, #6C, #6C, #00 ; #
	db 7 ; width
	dg .##.##..
	dg .##.##..
	dg #######.
	dg .##.##..
	dg #######.
	dg .##.##..
	dg .##.##..

	;db #18, #7E, #D8, #7E, #1B, #7E, #18, #00 ; $
	db 8 ; width
	dg ...##...
	dg .######.
	dg ##.##...
	dg .######.
	dg ...##.##
	dg .######.
	dg ...##...

	;db #00, #66, #6C, #18, #36, #66, #00, #00 ; %
	db 6 ; width
	dg ........
	dg ##..##..
	dg ##.##...
	dg ..##....
	dg .##.##..
	dg ##..##..
	dg ........

	;db #38, #6C, #38, #76, #DC, #CC, #76, #00 ; &
	db 7 ; width
	dg ..###...
	dg .##.##..
	dg ..###...
	dg .###.##.
	dg ##.###..
	dg ##..##..
	dg .###.##.

	;db #18, #18, #18, #00, #00, #00, #00, #00 ; '
	IF 0
	db 2|ASCENDER ; width
	dg ##......
	dg ##......
	dg ##......
	dg ........
	dg ........
	dg ........
	dg ........
	ELSE
		IF 1
		db 3|ASCENDER ; width
		dg .##.....
		dg .##.....
		dg ##......
		dg ........
		dg ........
		dg ........
		dg ........
		ELSE
		db 4|ASCENDER ; width
		dg ..##....
		dg ..##....
		dg .##.....
		dg ........
		dg ........
		dg ........
		dg ........
		ENDIF
	ENDIF

	;db #18, #30, #30, #30, #30, #30, #18, #00 ; (
	db 3 ; width
	dg .##.....
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	dg .##.....

	;db #30, #18, #18, #18, #18, #18, #30, #00 ; )
	db 3 ; width
	dg ##......
	dg .##.....
	dg .##.....
	dg .##.....
	dg .##.....
	dg .##.....
	dg ##......

	;db #00, #6C, #38, #FE, #38, #6C, #00, #00 ; *
	db 7 ; width
	dg ........
	dg .##.##..
	dg ..###...
	dg #######.
	dg ..###...
	dg .##.##..
	dg ........

	;db #00, #18, #18, #7E, #18, #18, #00, #00 ; +
	db 6 ; width
	dg ........
	dg ..##....
	dg ..##....
	dg ######..
	dg ..##....
	dg ..##....
	dg ........

	;db #00, #00, #00, #00, #00, #18, #18, #30 ; ,
	db 3 ; width
	dg ........
	dg ........
	dg ........
	dg ........
	dg .##.....
	dg .##.....
	dg ##......

	;db #00, #00, #00, #7E, #00, #00, #00, #00 ; -
	db 6 ; width
	dg ........
	dg ........
	dg ........
	dg ######..
	dg ........
	dg ........
	dg ........

	;db #00, #00, #00, #00, #00, #18, #18, #00 ; .
	db 2 ; width
	dg ........
	dg ........
	dg ........
	dg ........
	dg ........
	dg ##......
	dg ##......
	
	;db #02, #06, #0C, #18, #30, #60, #40, #00 ; /
	db 8 ; width
	dg ......##
	dg .....##.
	dg ....##..
	dg ...##...
	dg ..##....
	dg .##.....
	dg ##......

	;db #3C, #66, #6E, #7E, #76, #66, #3C, #00 ; 0
	db 7 ; width - not proportional for numbers
	dg ..####..
	dg .##..##.
	dg .##.###.
	dg .######.
	dg .###.##.
	dg .##..##.
	dg ..####..

	;db #18, #38, #18, #18, #18, #18, #3C, #00 ; 1
	db 7 ; width - not proportional for numbers
	dg ...##...
	dg ..###...
	dg ...##...
	dg ...##...
	dg ...##...
	dg ...##...
	dg ..####..

	;db #3C, #66, #06, #1C, #30, #60, #7E, #00 ; 2
	db 7 ; width - not proportional for numbers
	dg ..####..
	dg .##..##.
	dg .....##.
	dg ...###..
	dg ..##....
	dg .##.....
	dg .######.

	;db #3C, #66, #06, #1C, #06, #66, #3C, #00 ; 3
	db 7 ; width - not proportional for numbers
	dg ..####..
	dg .##..##.
	dg .....##.
	dg ...###..
	dg .....##.
	dg .##..##.
	dg ..####..

	;db #1C, #3C, #6C, #CC, #FE, #0C, #0C, #00 ; 4
	db 7 ; width - not proportional for numbers
	dg ...###..
	dg ..####..
	dg .##.##..
	dg ##..##..
	dg #######.
	dg ....##..
	dg ....##..

	;db #7E, #60, #60, #7C, #06, #06, #7C, #00 ; 5
	db 7 ; width - not proportional for numbers
	dg .######.
	dg .##.....
	dg .##.....
	dg .#####..
	dg .....##.
	dg .....##.
	dg .#####..

	;db #3C, #66, #60, #7C, #66, #66, #3C, #00 ; 6
	db 7 ; width - not proportional for numbers
	dg ..####..
	dg .##..##.
	dg .##.....
	dg .#####..
	dg .##..##.
	dg .##..##.
	dg ..####..

	;db #7E, #06, #0C, #18, #18, #18, #18, #00 ; 7
	db 7 ; width - not proportional for numbers
	dg .######.
	dg .....##.
	dg ....##..
	dg ...##...
	dg ...##...
	dg ...##...
	dg ...##...

	;db #3C, #66, #66, #3C, #66, #66, #3C, #00 ; 8
	db 7 ; width - not proportional for numbers
	dg ..####..
	dg .##..##.
	dg .##..##.
	dg ..####..
	dg .##..##.
	dg .##..##.
	dg ..####..

	;db #3C, #66, #66, #3E, #06, #0C, #18, #00 ; 9
	db 7 ; width - not proportional for numbers
	dg ..####..
	dg .##..##.
	dg .##..##.
	dg ..#####.
	dg .....##.
	dg ....##..
	dg ...##...

	;db #00, #00, #18, #18, #00, #18, #18, #00 ; :
	db 2 ; width
	dg ........
	dg ........
	dg ##......
	dg ##......
	dg ........
	dg ##......
	dg ##......

	;db #00, #00, #18, #18, #00, #18, #18, #30 ; ;
	db 3 ; width
	dg ........
	dg ........
	dg .##.....
	dg .##.....
	dg ........
	dg .##.....
	dg ##......

	;db #0C, #18, #30, #60, #30, #18, #0C, #00 ; <
	db 5 ; width
	dg ...##...
	dg ..##....
	dg .##.....
	dg ##......
	dg .##.....
	dg ..##....
	dg ...##...

	;db #00, #00, #7E, #00, #7E, #00, #00, #00 ; =
	db 6 ; width
	dg ........
	dg ........
	dg ######..
	dg ........
	dg ######..
	dg ........
	dg ........

	;db #30, #18, #0C, #06, #0C, #18, #30, #00 ; >
	db 5 ; width
	dg ##......
	dg .##.....
	dg ..##....
	dg ...##...
	dg ..##....
	dg .##.....
	dg ##......

	;db #3C, #66, #06, #0C, #18, #00, #18, #00 ; ?
	db 6 ; width
	dg .####...
	dg ##..##..
	dg ....##..
	dg ...##...
	dg ..##....
	dg ........
	dg ..##....

	;db #7C, #CE, #D6, #D6, #CC, #E0, #7C, #00 ; @
	db 8 ; width
	dg .######.
	dg ##...###
	dg ##.##.##
	dg ##.##.##
	dg ##..###.
	dg ###.....
	dg .######.

	;db #3C, #66, #66, #7E, #66, #66, #66, #00 ; A
	db 6 ; width
	dg .####...
	dg ##..##..
	dg ##..##..
	dg ######..
	dg ##..##..
	dg ##..##..
	dg ##..##..

	;db #7C, #66, #66, #7C, #66, #66, #7C, #00 ; B
	db 6 ; width
	dg #####...
	dg ##..##..
	dg ##..##..
	dg #####...
	dg ##..##..
	dg ##..##..
	dg #####...

	;db #1C, #36, #60, #60, #60, #36, #1C, #00 ; C
	IF 0
	db 6 ; width
	dg ..###...
	dg .##.##..
	dg ##......
	dg ##......
	dg ##......
	dg .##.##..
	dg ..###...
	ELSE
	db 6 ; width
	dg .####...
	dg ##..##..
	dg ##......
	dg ##......
	dg ##......
	dg ##..##..
	dg .####...
	ENDIF

		;db #78, #6C, #66, #66, #66, #6C, #78, #00 ; D
	db 6 ; width
	dg ####....
	dg ##.##...
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg ##.##...
	dg ####....

	;db #7E, #60, #60, #7C, #60, #60, #7E, #00 ; E
	db 6 ; width
	dg ######..
	dg ##......
	dg ##......
	dg #####...
	dg ##......
	dg ##......
	dg ######..

	;db #7E, #60, #60, #7C, #60, #60, #60, #00 ; F
	db 6 ; width
	dg ######..
	dg ##......
	dg ##......
	dg #####...
	dg ##......
	dg ##......
	dg ##......

	;db #3C, #66, #C0, #CE, #C6, #66, #3C, #00 ; G
	IF 0
	db 7 ; width
	dg ..####..
	dg .##..##.
	dg ##......
	dg ##..###.
	dg ##...##.
	dg .##..##.
	dg ..####..
	ELSE
	db 6 ; width
	dg .####...
	dg ##..##..
	dg ##......
	dg ##.###..
	dg ##..##..
	dg ##..##..
	dg .#####..
	ENDIF

	;db #66, #66, #66, #7E, #66, #66, #66, #00 ; H
	db 6 ; width
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg ######..
	dg ##..##..
	dg ##..##..
	dg ##..##..

	;db #7E, #18, #18, #18, #18, #18, #7E, #00 ; I
	db 6 ; width
	dg ######..
	dg ..##....
	dg ..##....
	dg ..##....
	dg ..##....
	dg ..##....
	dg ######..

	;db #06, #06, #06, #06, #66, #66, #3C, #00 ; J
	db 6 ; width
	dg ....##..
	dg ....##..
	dg ....##..
	dg ....##..
	dg ##..##..
	dg ##..##..
	dg .####...

	;db #66, #6C, #78, #70, #78, #6C, #66, #00 ; K
	db 6 ; width
	dg ##..##..
	dg ##.##...
	dg ####....
	dg ###.....
	dg ####....
	dg ##.##...
	dg ##..##..

	;db #60, #60, #60, #60, #60, #60, #7E, #00 ; L
	db 6 ; width
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	dg ######..

	;db #C6, #EE, #FE, #D6, #D6, #C6, #C6, #00 ; M
	db 7 ; width
	dg ##...##.
	dg ###.###.
	dg #######.
	dg ##.#.##.
	dg ##...##.
	dg ##...##.
	dg ##...##.

	;db #C6, #E6, #F6, #DE, #CE, #C6, #C6, #00 ; N
	IF 0
	db 7 ; width
	dg ##...##.
	dg ###..##.
	dg ####.##.
	dg ##.####.
	dg ##..###.
	dg ##...##.
	dg ##...##.
	ELSE
	db 6 ; width
	dg ##..##..
	dg ###.##..
	dg ######..
	dg ##.###..
	dg ##..##..
	dg ##..##..
	dg ##..##..
	ENDIF

	;db #38, #6C, #C6, #C6, #C6, #6C, #38, #00 ; O
	IF 0
	db 7 ; width
	dg ..###...
	dg .##.##..
	dg ##...##.
	dg ##...##.
	dg ##...##.
	dg .##.##..
	dg ..###...
	ELSE
	db 6 ; width
	dg .####...
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg .####...
	ENDIF

	;db #7C, #66, #66, #7C, #60, #60, #60, #00 ; P
	db 6 ; width
	dg #####...
	dg ##..##..
	dg ##..##..
	dg #####...
	dg ##......
	dg ##......
	dg ##......

	;db #38, #6C, #C6, #C6, #DA, #6C, #36, #00 ; Q
	IF 0
	db 7 ; width
	dg ..###...
	dg .##.##..
	dg ##...##.
	dg ##...##.
	dg ##.##.#.
	dg .##.##..
	dg ..##.##.
	ELSE
	db 6|IS9TALL ; width
	dg .####...
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg .####...
	dg ..##....
	dg ...###..
	ENDIF

	;db #7C, #66, #66, #7C, #66, #66, #66, #00 ; R
	db 6 ; width
	dg #####...
	dg ##..##..
	dg ##..##..
	dg #####...
	dg ##.##...
	dg ##..##..
	dg ##..##..

	;db #3C, #66, #70, #3C, #0E, #66, #3C, #00 ; S
	db 6 ; width
	dg .####...
	dg ##..##..
	dg ###.....
	dg .####...
	dg ...###..
	dg ##..##..
	dg .####...

	;db #7E, #18, #18, #18, #18, #18, #18, #00 ; T
	db 6 ; width
	dg ######..
	dg ..##....
	dg ..##....
	dg ..##....
	dg ..##....
	dg ..##....
	dg ..##....

	;db #66, #66, #66, #66, #66, #66, #3C, #00 ; U
	db 6 ; width
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg .####...

	;db #66, #66, #66, #66, #66, #3C, #18, #00 ; V
	db 6 ; width
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg .####...
	dg ..##....

	;db #C6, #C6, #D6, #D6, #FE, #EE, #C6, #00 ; W
	db 7 ; width
	dg ##...##.
	dg ##...##.
	dg ##...##.
	dg ##.#.##.
	dg #######.
	dg ###.###.
	dg ##...##.

	;db #66, #66, #3C, #18, #3C, #66, #66, #00 ; X
	db 6 ; width
	dg ##..##..
	dg ##..##..
	dg .####...
	dg ..##....
	dg .####...
	dg ##..##..
	dg ##..##..

	;db #66, #66, #66, #3C, #18, #18, #18, #00 ; Y
	IF 0
	db 6 ; width
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg .####...
	dg ..##....
	dg ..##....
	dg ..##....
	ELSE
	db 6 ; width
	dg ##..##..
	dg ##..##..
	dg .####...
	dg ..##....
	dg ..##....
	dg ..##....
	dg ..##....
	ENDIF

	;db #7E, #06, #0C, #18, #30, #60, #7E, #00 ; Z
	db 6 ; width
	dg ######..
	dg ....##..
	dg ...##...
	dg ..##....
	dg .##.....
	dg ##......
	dg ######..

	;db #3C, #30, #30, #30, #30, #30, #3C, #00 ; [
	db 4 ; width
	dg ####....
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	dg ####....

	;db #C0, #60, #30, #18, #0C, #06, #02, #00 ; \
	db 8 ; width
	dg ##......
	dg .##.....
	dg ..##....
	dg ...##...
	dg ....##..
	dg .....##.
	dg ......##

	;db #3C, #0C, #0C, #0C, #0C, #0C, #3C, #00 ; ]
	db 4 ; width
	dg ####....
	dg ..##....
	dg ..##....
	dg ..##....
	dg ..##....
	dg ..##....
	dg ####....

	;db #10, #38, #6C, #C6, #00, #00, #00, #00 ; ^
	db 7 ; width
	dg ...#....
	dg ..###...
	dg .##.##..
	dg ##...##.
	dg ........
	dg ........
	dg ........

	;db #00, #00, #00, #00, #00, #00, #00, #FF ; _
	db 8 ; width - hmm this should take into account the letter spacing as well. Probably won't use this anyway...
	dg ........
	dg ........
	dg ........
	dg ........
	dg ........
	dg ........
	dg ########

	;db #3C, #66, #60, #FC, #60, #60, #FE, #00 ; £
	db 7 ; width
	dg ..####..
	dg .##..##.
	dg .##.....
	dg ######..
	dg .##.....
	dg .##.....
	dg #######.

	;db #00, #00, #3C, #06, #3E, #66, #3E, #00 ; a
	db 6 ; width
	dg ........
	dg ........
	dg .####...
	dg ....##..
	dg .#####..
	dg ##..##..
	dg .#####..

	;db #60, #60, #7C, #66, #66, #66, #7C, #00 ; b
	db 6 ; width
	dg ##......
	dg ##......
	dg #####...
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg #####...

	;db #00, #00, #3C, #66, #60, #66, #3C, #00 ; c
	db 6 ; width
	dg ........
	dg ........
	dg .####...
	dg ##..##..
	dg ##......
	dg ##..##..
	dg .####...

	;db #06, #06, #3E, #66, #66, #66, #3E, #00 ; d
	db 6 ; width
	dg ....##..
	dg ....##..
	dg .#####..
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg .#####..

	;db #00, #00, #3C, #66, #7E, #60, #3C, #00 ; e
	db 6 ; width
	dg ........
	dg ........
	dg .####...
	dg ##..##..
	dg ######..
	dg ##......
	dg .####...

	;db #1C, #30, #7C, #30, #30, #30, #30, #00 ; f
	db 5 ; width
	dg ..###...
	dg .##.....
	dg #####...
	dg .##.....
	dg .##.....
	dg .##.....
	dg .##.....

	;db #00, #00, #3E, #66, #66, #3E, #06, #3C ; g
	;db 6|SIMPLEDESCENDER ; width
	db 6|DESCENDER
	dg .#####..
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg .#####..
	dg ....##..
	dg .####...

	;db #60, #60, #7C, #66, #66, #66, #66, #00 ; h
	db 6 ; width
	dg ##......
	dg ##......
	dg #####...
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg ##..##..

	;db #18, #00, #38, #18, #18, #18, #3C, #00 ; i
	db 2 ; width
	dg ##......
	dg ........
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	dg ##......

	;db #0C, #00, #1C, #0C, #0C, #0C, #6C, #38 ; j
	IF MAYBE8TALL
	db 3|IS9TALL ; width
	dg .##.....
	dg ........
	dg .##.....
	dg .##..... ; this row gets copied thrice
	dg .##.....
	dg .##.....
	dg ##......
	ELSE
	db 3 ; width
	dg .##.....
	dg ........
	dg .##.....
	dg .##.....
	dg .##.....
	dg .##.....
	dg ##......
	ENDIF

	;db #60, #60, #66, #6C, #78, #6C, #66, #00 ; k
	db 6 ; width
	dg ##......
	dg ##......
	dg ##..##..
	dg ##.##...
	dg ####....
	dg ##.##...
	dg ##..##..

	;db #18, #18, #18, #18, #18, #18, #0C, #00 ; l
	IF 0 
	db 3 ; width
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	dg .##.....
	ELSE
	db 2 ; width
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	dg ##......
	ENDIF

	;db #00, #00, #EC, #FE, #D6, #D6, #C6, #00 ; m
	IF 1
	db 7 ; width
	dg ........
	dg ........
	dg ###.##..
	dg #######.
	dg ##.#.##.
	dg ##.#.##.
	dg ##...##.
	ELSE
	db 8 ; width
	dg ........
	dg ........
	dg ###..##.
	dg ########
	dg ##.##.##
	dg ##.##.##
	dg ##....##
	ENDIF

	;db #00, #00, #7C, #66, #66, #66, #66, #00 ; n
	db 6 ; width
	dg ........
	dg ........
	dg #####...
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg ##..##..

	;db #00, #00, #3C, #66, #66, #66, #3C, #00 ; o
	db 6 ; width
	dg ........
	dg ........
	dg .####...
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg .####...

	;db #00, #00, #7C, #66, #66, #7C, #60, #60 ; p
	;db 6|SIMPLEDESCENDER ; width
	db 6|DESCENDER
	dg #####...
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg #####...
	dg ##......
	dg ##......

	;db #00, #00, #7C, #CC, #CC, #7C, #0C, #0E ; q
	;db 7|SIMPLEDESCENDER ; width
	db 7|DESCENDER
	dg .#####..
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg .#####..
	dg ....##..
	dg ....###.

	;db #00, #00, #7C, #66, #60, #60, #60, #00 ; r
	db 6 ; width
	dg ........
	dg ........
	dg #####...
	dg ##..##..
	dg ##......
	dg ##......
	dg ##......

	;db #00, #00, #3C, #70, #3C, #0E, #7C, #00 ; s
	IF 0
	db 6 ; width
	dg ........
	dg ........
	dg .####...
	dg ###.....
	dg .####...
	dg ...###..
	dg #####...
	ELSE
	db 6 ; width
	dg ........
	dg ........
	dg .####...
	dg ##......
	dg .####...
	dg ....##..
	dg #####...
	ENDIF

	;db #30, #30, #7C, #30, #30, #36, #1C, #00 ; t
	db 6 ; width
	dg .##.....
	dg .##.....
	dg #####...
	dg .##.....
	dg .##.....
	dg .##.##..
	dg ..###...

	;db #00, #00, #66, #66, #66, #66, #3E, #00 ; u
	db 6 ; width
	dg ........
	dg ........
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg .#####..

	;db #00, #00, #66, #66, #66, #3C, #18, #00 ; v
	db 6 ; width
	dg ........
	dg ........
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg .####...
	dg ..##....

	;db #00, #00, #C6, #D6, #D6, #D6, #7E, #00 ; w
	IF 1
	db 7 ; width
	dg ........
	dg ........
	dg ##...##.
	dg ##.#.##.
	dg ##.#.##.
	dg ##.#.##.
	dg .######.
	ELSE
	db 8 ; width
	dg ........
	dg ........
	dg ##....##
	dg ##.##.##
	dg ##.##.##
	dg ##.##.##
	dg .#######
	ENDIF

	;db #00, #00, #66, #3C, #18, #3C, #66, #00 ; x
	db 6 ; width
	dg ........
	dg ........
	dg ##..##..
	dg .####...
	dg ..##....
	dg .####...
	dg ##..##..

	;db #00, #00, #66, #66, #66, #3E, #06, #3C ; y
	;db 6|SIMPLEDESCENDER ; width
	db 6|DESCENDER
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg ##..##..
	dg .#####..
	dg ....##..
	dg .####...

	;db #00, #00, #7E, #0C, #18, #30, #7E, #00 ; z
	db 6 ; width
	dg ........
	dg ........
	dg ######..
	dg ...##...
	dg ..##....
	dg .##.....
	dg ######..

	;db #0C, #18, #18, #70, #18, #18, #0C, #00 ; {
	db 5 ; width
	dg ...##...
	dg ..##....
	dg ..##....
	dg ###.....
	dg ..##....
	dg ..##....
	dg ...##...

	;db #18, #18, #18, #18, #18, #18, #18, #00 ; |
	db 2 ; width
	dg ##......
	dg ##......
	dg ##......
	dg ........
	dg ##......
	dg ##......
	dg ##......

	;db #30, #18, #18, #0E, #18, #18, #30, #00 ; }
	db 5 ; width
	dg ##......
	dg .##.....
	dg .##.....
	dg ..###...
	dg .##.....
	dg .##.....
	dg ##......

	;db #00, #00, #30, #5A, #5A, #0C, #00, #00 ; ~
	db 6 ; width
	dg ........
	dg .##.....
	dg #.##.#..
	dg #.##.#..
	dg ...##...
	dg ........
	dg ........

	;db #30, #18, #0C, #00, #00, #00, #00, #00 ; ` or (C)
	IF MAYBE8TALL
	db 8|IS8TALL ; width
	dg ..####..
	dg .#....#.
	dg #..##..#
	dg #.#....# ; this row gets copied twice
	dg #..##..#
	dg .#....#.
	dg ..####..
	ELSE
	db 4; width
	dg ##......
	dg .##.....
	dg ..##....
	dg ........
	dg ........
	dg ........
	dg ........
	ENDIF

	ALIGN 256
gpbuff0		; general purpose buffer 0

solar_power_erase0: ; only need 40 for these, maybe 41 if JSW
	BLOCK 42
solar_power_erase1:
	BLOCK 42
solar_power_draw0:
	BLOCK 42
solar_power_draw1:
	BLOCK 42

solar_startcell		db 0
solar_occupyindex	db 0
solar_startmask		db 0
solar_direction		db 0
SOLAR_DATA_LEN	EQU $-solar_power_erase0

	ALIGN 256
Final_Barrier_ImageData:
	DISPLAY	"Final Barrier Image address: ", /A, Final_Barrier_ImageData
	INCLUDE newfinal.asm
	DISPLAY	"Final Barrier Image length: ", /A, $-Final_Barrier_ImageData

	ALIGN 256
gfx_gnewwilly0
			dg	.....##......... ;0
			dg	..##.##......... ;1
			dg	.##............. ;2
			dg	.#..#........... ;3
			dg	..###.#......... ;4
			dg	...###.......... ;5
			dg	................ ;6
			dg	.##.#..#........ ;7
			dg	####.##......... ;8
			dg	##..####........ ;9
			dg	...##..#........ ;10
			dg	##..####........ ;11
			dg	##.####......... ;12
			dg	..##.##......... ;13
			dg	................ ;14
			dg	..###.##........ ;15

gfx_gnewwilly1
			dg	......##........ ;0
			dg	...##.##........ ;1
			dg	..##............ ;2
			dg	..#..#.......... ;3
			dg	...###.#........ ;4
			dg	....###......... ;5
			dg	.##............. ;6
			dg	####.#..#....... ;7
			dg	...#.###........ ;8
			dg	##...####....... ;9
			dg	##.###..#....... ;10
			dg	....#####....... ;11
			dg	...###.......... ;12
			dg	...##..###...... ;13
			dg	................ ;14
			dg	...###.......... ;15

gfx_gnewwilly2
			dg	.......##....... ;0
			dg	....##.##....... ;1
			dg	...##........... ;2
			dg	...#..#......... ;3
			dg	....###.#....... ;4
			dg	......##........ ;5
			dg	..###........... ;6
			dg	.##...#..#...... ;7
			dg	.#.##..##....... ;8
			dg	...##.###....... ;9
			dg	.....#####...... ;10
			dg	...####......... ;11
			dg	...###..###..... ;12
			dg	....##.......... ;13
			dg	...#............ ;14
			dg	....##.......... ;15

gfx_gnewwilly3
			dg	................ ;0
			dg	................ ;1
			dg	........##...... ;2
			dg	.....##.##...... ;3
			dg	....##.......... ;4
			dg	....#..#........ ;5
			dg	.....###.#...... ;6
			dg	......###....... ;7
			dg	.....#.......... ;8
			dg	....####.##..... ;9
			dg	.....###.##..... ;10
			dg	................ ;11
			dg	....#.#.##...... ;12
			dg	...#.####.#..... ;13
			dg	...#..##........ ;14
			dg	.........###.... ;15

gfx_gnewwilly4
			dg	................ ;0
			dg	.........##..... ;1
			dg	......##.##..... ;2
			dg	.....##......... ;3
			dg	.....#..#....... ;4
			dg	......###.#..... ;5
			dg	.......###...... ;6
			dg	......#....##... ;7
			dg	......####.##... ;8
			dg	.......####..... ;9
			dg	.....#.......... ;10
			dg	....#.#####..... ;11
			dg	....#.##..##.... ;12
			dg	..........##.... ;13
			dg	................ ;14
			dg	..........###... ;15

gfx_gnewwilly5
			dg	..........##.... ;0
			dg	..........##.... ;1
			dg	.......##....... ;2
			dg	......##........ ;3
			dg	......#..#...... ;4
			dg	.......###.#.... ;5
			dg	........###..... ;6
			dg	.......#........ ;7
			dg	......###....... ;8
			dg	......####.#.... ;9
			dg	.......##.###... ;10
			dg	.....#...#.##... ;11
			dg	.....#.##.#..... ;12
			dg	.....#....##.... ;13
			dg	................ ;14
			dg	..........###... ;15

gfx_gnewwilly6
			dg	................ ;0
			dg	...........##... ;1
			dg	........##.##... ;2
			dg	.......##....... ;3
			dg	.......#..#..... ;4
			dg	........###.#... ;5
			dg	.........###.... ;6
			dg	........#....... ;7
			dg	.......###.#.... ;8
			dg	.......###...... ;9
			dg	........#.##.... ;10
			dg	.......#..##.... ;11
			dg	........##...... ;12
			dg	.........###.... ;13
			dg	................ ;14
			dg	..........###... ;15

gfx_gnewwilly7
			dg	.............##. ;0
			dg	..........##.##. ;1
			dg	.........##..... ;2
			dg	.........#..#... ;3
			dg	..........###.#. ;4
			dg	...........###.. ;5
			dg	................ ;6
			dg	.........##.#... ;7
			dg	........####.#.. ;8
			dg	........##..###. ;9
			dg	..........##.##. ;10
			dg	..........##.##. ;11
			dg	............##.. ;12
			dg	...........###.. ;13
			dg	................ ;14
			dg	..........###... ;15

gfx_gnewwilly8
			dg	.##............. ;0
			dg	.##.##.......... ;1
			dg	.....##......... ;2
			dg	...#..#......... ;3
			dg	.#.###.......... ;4
			dg	..###........... ;5
			dg	................ ;6
			dg	...#.##......... ;7
			dg	..#.####........ ;8
			dg	.###..##........ ;9
			dg	.##.##.......... ;10
			dg	.##.##.......... ;11
			dg	..##............ ;12
			dg	..###........... ;13
			dg	................ ;14
			dg	...###.......... ;15

gfx_gnewwilly9
			dg	................ ;0
			dg	...##........... ;1
			dg	...##.##........ ;2
			dg	.......##....... ;3
			dg	.....#..#....... ;4
			dg	...#.###........ ;5
			dg	....###......... ;6
			dg	.......#........ ;7
			dg	....#.###....... ;8
			dg	......###....... ;9
			dg	....##.#........ ;10
			dg	....##..#....... ;11
			dg	......##........ ;12
			dg	....###......... ;13
			dg	................ ;14
			dg	...###.......... ;15

gfx_gnewwilly10
			dg	....##.......... ;0
			dg	....##.......... ;1
			dg	.......##....... ;2
			dg	........##...... ;3
			dg	......#..#...... ;4
			dg	....#.###....... ;5
			dg	.....###........ ;6
			dg	........#....... ;7
			dg	.......###...... ;8
			dg	....#.####...... ;9
			dg	...###.##....... ;10
			dg	...##.#...#..... ;11
			dg	.....#.##.#..... ;12
			dg	....##....#..... ;13
			dg	................ ;14
			dg	...###.......... ;15

gfx_gnewwilly11
			dg	................ ;0
			dg	.....##......... ;1
			dg	.....##.##...... ;2
			dg	.........##..... ;3
			dg	.......#..#..... ;4
			dg	.....#.###...... ;5
			dg	......###....... ;6
			dg	...##....#...... ;7
			dg	...##.####...... ;8
			dg	.....####....... ;9
			dg	..........#..... ;10
			dg	.....#####.#.... ;11
			dg	....##..##.#.... ;12
			dg	....##.......... ;13
			dg	................ ;14
			dg	...###.......... ;15

gfx_gnewwilly12
			dg	................ ;0
			dg	................ ;1
			dg	......##........ ;2
			dg	......##.##..... ;3
			dg	..........##.... ;4
			dg	........#..#.... ;5
			dg	......#.###..... ;6
			dg	.......###...... ;7
			dg	..........#..... ;8
			dg	.....##.####.... ;9
			dg	.....##.###..... ;10
			dg	................ ;11
			dg	......##.#.#.... ;12
			dg	.....#.####.#... ;13
			dg	........##..#... ;14
			dg	....###......... ;15

gfx_gnewwilly13
			dg	.......##....... ;0
			dg	.......##.##.... ;1
			dg	...........##... ;2
			dg	.........#..#... ;3
			dg	.......#.###.... ;4
			dg	........##...... ;5
			dg	...........###.. ;6
			dg	......#..#...##. ;7
			dg	.......##..##.#. ;8
			dg	.......###.##... ;9
			dg	......#####..... ;10
			dg	.........####... ;11
			dg	.....###..###... ;12
			dg	..........##.... ;13
			dg	............#... ;14
			dg	..........##.... ;15

gfx_gnewwilly14
			dg	........##...... ;0
			dg	........##.##... ;1
			dg	............##.. ;2
			dg	..........#..#.. ;3
			dg	........#.###... ;4
			dg	.........###.... ;5
			dg	.............##. ;6
			dg	.......#..#.#### ;7
			dg	........###.#... ;8
			dg	.......####...## ;9
			dg	.......#..###.## ;10
			dg	.......#####.... ;11
			dg	..........###... ;12
			dg	......###..##... ;13
			dg	................ ;14
			dg	..........###... ;15

gfx_gnewwilly15
			dg	.........##..... ;0
			dg	.........##.##.. ;1
			dg	.............##. ;2
			dg	...........#..#. ;3
			dg	.........#.###.. ;4
			dg	..........###... ;5
			dg	................ ;6
			dg	........#..#.##. ;7
			dg	.........##.#### ;8
			dg	........####..## ;9
			dg	........#..##... ;10
			dg	........####..## ;11
			dg	.........####.## ;12
			dg	.........##.##.. ;13
			dg	................ ;14
			dg	........##.###.. ;15

horizontal_guardians_page:
gfx_robot0	
			dg	...#.##..#...... ;0
			dg	..#.##.###...... ;1
			dg	.##.##.###...... ;2
			dg	.###.##..#...... ;3
			dg	................ ;4
			dg	##..##.##....... ;5
			dg	##.##.####...... ;6
			dg	.#####.......... ;7
			dg	##.##.####...... ;8
			dg	##.###.##....... ;9
			dg	....#........... ;10
			dg	##.#.##......... ;11
			dg	#.##...#........ ;12
			dg	##.....#........ ;13
			dg	#.....#.#....... ;14
			dg	......####...... ;15

gfx_robot1	
			dg	................ ;0
			dg	.....#.##..#.... ;1
			dg	....#.##.###.... ;2
			dg	...##.##.###.... ;3
			dg	...###.##..#.... ;4
			dg	......#..##..... ;5
			dg	......#.####.... ;6
			dg	..##.###........ ;7
			dg	...#####........ ;8
			dg	..##.###........ ;9
			dg	.....##.####.... ;10
			dg	......##.##..... ;11
			dg	.......#........ ;12
			dg	......#......... ;13
			dg	.....#.#........ ;14
			dg	.....####....... ;15
			
gfx_robot2	
			dg	.......#.##..#.. ;0
			dg	......#.##.###.. ;1
			dg	.....##.##.###.. ;2
			dg	.....###.##..#.. ;3
			dg	........#....... ;4
			dg	.......##.##.... ;5
			dg	......##.####... ;6
			dg	....####.####... ;7
			dg	......###.##.... ;8
			dg	......###....... ;9
			dg	.......#.##..... ;10
			dg	.......#...#.... ;11
			dg	....##.#..#.#... ;12
			dg	....#.#...####.. ;13
			dg	....##.......... ;14
			dg	....###......... ;15

gfx_robot3	
			dg	................ ;0
			dg	.........#.##..# ;1
			dg	........#.##.### ;2
			dg	.......##.##.### ;3
			dg	.......###.##..# ;4
			dg	..........#..... ;5
			dg	..........##.##. ;6
			dg	......##.##.#### ;7
			dg	.......#####.... ;8
			dg	......##.###.... ;9
			dg	.........##.#### ;10
			dg	..........##.##. ;11
			dg	.......##...#... ;12
			dg	.......#.#..#... ;13
			dg	.......##..#.#.. ;14
			dg	.......###.####. ;15

gfx_robot4
			dg	................ ;0
			dg	#..##.#......... ;1
			dg	###.##.#........ ;2
			dg	###.##.##....... ;3
			dg	#..##.###....... ;4
			dg	.....#.......... ;5
			dg	.##.##.......... ;6
			dg	####.##.##...... ;7
			dg	....#####....... ;8
			dg	....###.##...... ;9
			dg	####.##......... ;10
			dg	.##.##.......... ;11
			dg	...#...##....... ;12
			dg	...#..#.#....... ;13
			dg	..#.#..##....... ;14
			dg	.####.###....... ;15

gfx_robot5
			dg	..#..##.#....... ;0
			dg	..###.##.#...... ;1
			dg	..###.##.##..... ;2
			dg	..#..##.###..... ;3
			dg	.......#........ ;4
			dg	....##.##....... ;5
			dg	...####.##...... ;6
			dg	...####.####.... ;7
			dg	....##.###...... ;8
			dg	.......###...... ;9
			dg	.....##.#....... ;10
			dg	....#...#....... ;11
			dg	...#.#..#.##.... ;12
			dg	..####...#.#.... ;13
			dg	..........##.... ;14
			dg	.........###.... ;15

gfx_robot6
			dg	................ ;0
			dg	....#..##.#..... ;1
			dg	....###.##.#.... ;2
			dg	....###.##.##... ;3
			dg	....#..##.###... ;4
			dg	.....##..#...... ;5
			dg	....####.#...... ;6
			dg	........###.##.. ;7
			dg	........#####... ;8
			dg	........###.##.. ;9
			dg	....####.##..... ;10
			dg	.....##.##...... ;11
			dg	........#....... ;12
			dg	.........#...... ;13
			dg	........#.#..... ;14
			dg	.......####..... ;15

gfx_robot7
			dg	......#..##.#... ;0
			dg	......###.##.#.. ;1
			dg	......###.##.##. ;2
			dg	......#..##.###. ;3
			dg	................ ;4
			dg	.......##.##..## ;5
			dg	......####.##.## ;6
			dg	..........#####. ;7
			dg	......####.##.## ;8
			dg	.......##.###.## ;9
			dg	...........#.... ;10
			dg	.........##.#.## ;11
			dg	........#...##.# ;12
			dg	........#.....## ;13
			dg	.......#.#.....# ;14
			dg	......####...... ;15

			IF 0
gfx_penguin0
			dg	..##.##.#....... ;0
			dg	.#..##.#.#...... ;1
			dg	#...##.#.#...... ;2
			dg	#....##......... ;3
			dg	#.......##...... ;4
			dg	.#.............. ;5
			dg	#.....##........ ;6
			dg	#.....###....... ;7
			dg	#.......##...... ;8
			dg	#.......##...... ;9
			dg	#......###...... ;10
			dg	#...####.#...... ;11
			dg	#....##.#....... ;12
			dg	.#.....###...... ;13
			dg	.......##....... ;14
			dg	..###........... ;15

gfx_penguin1
			dg	....##.##.#..... ;0
			dg	...#..##.#.#.... ;1
			dg	..#...##.#.#.... ;2
			dg	..#....##....... ;3
			dg	..#.......##.... ;4
			dg	...#............ ;5
			dg	..#.....##...... ;6
			dg	..#......##..... ;7
			dg	.#........##.... ;8
			dg	.#........##.... ;9
			dg	.#......####.... ;10
			dg	..#.....####.... ;11
			dg	..#...#..##..... ;12
			dg	....####........ ;13
			dg	....###......... ;14
			dg	.........##..... ;15

gfx_penguin2
			dg	................ ;0
			dg	......##.##.#... ;1
			dg	.....#..##.#.#.. ;2
			dg	....#...##.#.#.. ;3
			dg	....#....##..... ;4
			dg	....#.......##.. ;5
			dg	.....#.......... ;6
			dg	....#.....##.... ;7
			dg	...#.....####... ;8
			dg	...#.......###.. ;9
			dg	...#........##.. ;10
			dg	...#.......###.. ;11
			dg	...#....######.. ;12
			dg	....#....####... ;13
			dg	.....#....##.... ;14
			dg	.......###...... ;15

gfx_penguin3
			dg	........##.##.#. ;0
			dg	.......#..##.#.# ;1
			dg	......#...##.#.# ;2
			dg	......#....##... ;3
			dg	......#.......## ;4
			dg	.......#........ ;5
			dg	......#.....##.. ;6
			dg	......#.......#. ;7
			dg	......#.......## ;8
			dg	......#......### ;9
			dg	......#....##### ;10
			dg	......#...###### ;11
			dg	.......#...####. ;12
			dg	................ ;13
			dg	.........###.##. ;14
			dg	................ ;15

gfx_penguin4
			dg	.#.##.##........ ;0
			dg	#.#.##..#....... ;1
			dg	#.#.##...#...... ;2
			dg	...##....#...... ;3
			dg	##.......#...... ;4
			dg	........#....... ;5
			dg	..##.....#...... ;6
			dg	.#.......#...... ;7
			dg	##.......#...... ;8
			dg	###......#...... ;9
			dg	#####....#...... ;10
			dg	######...#...... ;11
			dg	.####...#....... ;12
			dg	................ ;13
			dg	.##.###......... ;14
			dg	................ ;15

gfx_penguin5
			dg	................ ;0
			dg	...#.##.##...... ;1
			dg	..#.#.##..#..... ;2
			dg	..#.#.##...#.... ;3
			dg	.....##....#.... ;4
			dg	..##.......#.... ;5
			dg	..........#..... ;6
			dg	....##.....#.... ;7
			dg	...####.....#... ;8
			dg	..###.......#... ;9
			dg	..##........#... ;10
			dg	..###.......#... ;11
			dg	..######....#... ;12
			dg	...####....#.... ;13
			dg	....##....#..... ;14
			dg	......###....... ;15

gfx_penguin6
			dg	.....#.##.##.... ;0
			dg	....#.#.##..#... ;1
			dg	....#.#.##...#.. ;2
			dg	.......##....#.. ;3
			dg	....##.......#.. ;4
			dg	............#... ;5
			dg	......##.....#.. ;6
			dg	.....##......#.. ;7
			dg	....##........#. ;8
			dg	....##........#. ;9
			dg	....####......#. ;10
			dg	....####.....#.. ;11
			dg	.....##..#...#.. ;12
			dg	........####.... ;13
			dg	.........###.... ;14
			dg	.....##......... ;15

gfx_penguin7
			dg	.......#.##.##.. ;0
			dg	......#.#.##..#. ;1
			dg	......#.#.##...# ;2
			dg	.........##....# ;3
			dg	......##.......# ;4
			dg	..............#. ;5
			dg	........##.....# ;6
			dg	.......###.....# ;7
			dg	......##.......# ;8
			dg	......##.......# ;9
			dg	......###......# ;10
			dg	......#.####...# ;11
			dg	.......#.##....# ;12
			dg	......###.....#. ;13
			dg	.......##....... ;14
			dg	...........###.. ;15
			ELSE
			IF 0
gfx_penguin0
			dg	..##.##.#....... ;0
			dg	.#..##.#.#...... ;1
			dg	#...##.#.#...... ;2
			dg	#....##......... ;3
			dg	#.......##...... ;4
			dg	.#.............. ;5
			dg	.#....##........ ;6
			dg	#.....###....... ;7
			dg	#.......##...... ;8
			dg	#.......##...... ;9
			dg	#......###...... ;10
			dg	#...####.#...... ;11
			dg	#....##.#....... ;12
			dg	.#.....###...... ;13
			dg	#......##....... ;14
			dg	..###........... ;15

gfx_penguin1
			dg	....##.##.#..... ;0
			dg	...#..##.#.#.... ;1
			dg	..#...##.#.#.... ;2
			dg	..#....##....... ;3
			dg	..#.......##.... ;4
			dg	...#............ ;5
			dg	..#.....##...... ;6
			dg	..#......##..... ;7
			dg	.#........##.... ;8
			dg	.#........##.... ;9
			dg	.#......####.... ;10
			dg	..#.....####.... ;11
			dg	..#...#..##..... ;12
			dg	.#..####........ ;13
			dg	..#.###......... ;14
			dg	.........##..... ;15

gfx_penguin2
			dg	................ ;0
			dg	......##.##.#... ;1
			dg	.....#..##.#.#.. ;2
			dg	....#...##.#.#.. ;3
			dg	....#....##..... ;4
			dg	....#.......##.. ;5
			dg	.....#.......... ;6
			dg	....#.....##.... ;7
			dg	...#.....####... ;8
			dg	...#.......###.. ;9
			dg	...#........##.. ;10
			dg	...#.......###.. ;11
			dg	...#....######.. ;12
			dg	....#....####... ;13
			dg	...#......##.... ;14
			dg	....##.###...... ;15

gfx_penguin3
			dg	........##.##.#. ;0
			dg	.......#..##.#.# ;1
			dg	......#...##.#.# ;2
			dg	......#....##... ;3
			dg	......#.......## ;4
			dg	.......#........ ;5
			dg	.......#....##.. ;6
			dg	......#.......#. ;7
			dg	......#.......## ;8
			dg	......#......### ;9
			dg	......#....##### ;10
			dg	.....#....###### ;11
			dg	.....#.....####. ;12
			dg	....#..#........ ;13
			dg	.....##..###.##. ;14
			dg	................ ;15

gfx_penguin4
			dg	.#.##.##........ ;0
			dg	#.#.##..#....... ;1
			dg	#.#.##...#...... ;2
			dg	...##....#...... ;3
			dg	##.......#...... ;4
			dg	........#....... ;5
			dg	..##....#....... ;6
			dg	.#.......#...... ;7
			dg	##.......#...... ;8
			dg	###......#...... ;9
			dg	#####....#...... ;10
			dg	######....#..... ;11
			dg	.####.....#..... ;12
			dg	........#..#.... ;13
			dg	.##.###..##..... ;14
			dg	................ ;15

gfx_penguin5
			dg	................ ;0
			dg	...#.##.##...... ;1
			dg	..#.#.##..#..... ;2
			dg	..#.#.##...#.... ;3
			dg	.....##....#.... ;4
			dg	..##.......#.... ;5
			dg	..........#..... ;6
			dg	....##.....#.... ;7
			dg	...####.....#... ;8
			dg	..###.......#... ;9
			dg	..##........#... ;10
			dg	..###.......#... ;11
			dg	..######....#... ;12
			dg	...####....#.... ;13
			dg	....##......#... ;14
			dg	......###.##.... ;15

gfx_penguin6
			dg	.....#.##.##.... ;0
			dg	....#.#.##..#... ;1
			dg	....#.#.##...#.. ;2
			dg	.......##....#.. ;3
			dg	....##.......#.. ;4
			dg	............#... ;5
			dg	......##.....#.. ;6
			dg	.....##......#.. ;7
			dg	....##........#. ;8
			dg	....##........#. ;9
			dg	....####......#. ;10
			dg	....####.....#.. ;11
			dg	.....##..#...#.. ;12
			dg	........####..#. ;13
			dg	.........###.#.. ;14
			dg	.....##......... ;15

gfx_penguin7
			dg	.......#.##.##.. ;0
			dg	......#.#.##..#. ;1
			dg	......#.#.##...# ;2
			dg	.........##....# ;3
			dg	......##.......# ;4
			dg	..............#. ;5
			dg	........##....#. ;6
			dg	.......###.....# ;7
			dg	......##.......# ;8
			dg	......##.......# ;9
			dg	......###......# ;10
			dg	......#.####...# ;11
			dg	.......#.##....# ;12
			dg	......###.....#. ;13
			dg	.......##......# ;14
			dg	...........###.. ;15
			ELSE
gfx_penguin0
			dg	..##.##.#.......
			dg	.#..##.#.#......
			dg	.#..##.#.#......
			dg	#....##.........
			dg	#.......##......
			dg	#...............
			dg	.#....##........
			dg	#.....###.......
			dg	#.......#.......
			dg	#.......##......
			dg	#......###......
			dg	#....#####......
			dg	.##.#####.......
			dg	###..####.......
			dg	##....##.##.....
			dg	#........##.....


			dg	....##.##.#.....
			dg	...#..##.#.#....
			dg	..#...##.#.#....
			dg	..#....##.......
			dg	..#.......##....
			dg	...#............
			dg	..#.....##......
			dg	..#.....###.....
			dg	.#......####....
			dg	.#.....#####....
			dg	.#.....#####....
			dg	.#.......###....
			dg	.#....##.##.....
			dg	#....####.......
			dg	.##.............
			dg	.......##.......



			dg	................
			dg	......##.##.#...
			dg	.....#..##.#.#..
			dg	....#...##.#.#..
			dg	....#....##.....
			dg	....#.......##..
			dg	.....#..........
			dg	....#.....##....
			dg	.###.....####...
			dg	........######..
			dg	........######..
			dg	......#######...
			dg	..#...#######...
			dg	.#..............
			dg	..#..#.....###..
			dg	....###.....###.


			dg	................
			dg	................
			dg	........##.##.#.
			dg	.......#..##.#.#
			dg	......#...##.#.#
			dg	......#....##...
			dg	......#.......##
			dg	.......#........
			dg	......#.....##..
			dg	......#.....###.
			dg	..........#####.
			dg	......#...######
			dg	......#...######
			dg	.....#..........
			dg	....#......####.
			dg	.....##..#####..

gfx_penguin4
			dg	................
			dg	................
			dg	.#.##.##........
			dg	#.#.##..#.......
			dg	#.#.##...#......
			dg	...##....#......
			dg	##.......#......
			dg	........#.......
			dg	..##.....#......
			dg	.###.....#......
			dg	.#####..........
			dg	######...#......
			dg	######...#......
			dg	..........#.....
			dg	.####......#....
			dg	..#####..##.....


			dg	................
			dg	...#.##.##......
			dg	..#.#.##..#.....
			dg	..#.#.##...#....
			dg	.....##....#....
			dg	..##.......#....
			dg	..........#.....
			dg	....##.....#....
			dg	...####.....###.
			dg	..######........
			dg	..######........
			dg	...#######......
			dg	...#######...#..
			dg	..............#.
			dg	..###.....#..#..
			dg	.###.....###....



			dg	.....#.##.##....
			dg	....#.#.##..#...
			dg	....#.#.##...#..
			dg	.......##....#..
			dg	....##.......#..
			dg	............#...
			dg	......##.....#..
			dg	.....###.....#..
			dg	....####......#.
			dg	....#####.....#.
			dg	....#####.....#.
			dg	....###.......#.
			dg	.....##.##....#.
			dg	.......####....#
			dg	.............##.
			dg	.......##.......


			dg	.......#.##.##..
			dg	......#.#.##..#.
			dg	......#.#.##..#.
			dg	.........##....#
			dg	......##.......#
			dg	...............#
			dg	........##....#.
			dg	.......###.....#
			dg	.......#.......#
			dg	......##.......#
			dg	......###......#
			dg	......#####....#
			dg	.......#####.##.
			dg	.......####..###
			dg	.....##.##....##
			dg	.....##........#

			ENDIF
			ENDIF

gfx_flamingo0
			dg	................ ;0
			dg	.#.##.#.##...... ;1
			dg	#.##.#.####..... ;2
			dg	##.##.#..##..... ;3
			dg	.##.....##...... ;4
			dg	...##........... ;5
			dg	....#........... ;6
			dg	..####.......... ;7
			dg	.###.#.......... ;8
			dg	####.#.......... ;9
			dg	##..#........... ;10
			dg	#.##.###........ ;11
			dg	#.....#.#....... ;12
			dg	...#............ ;13
			dg	...#............ ;14
			dg	..#.#........... ;15

gfx_flamingo1
			dg	...#.##.#....... ;0
			dg	..#.##.#.##..... ;1
			dg	..##.##.####.... ;2
			dg	...##.....##.... ;3
			dg	.....##..##..... ;4
			dg	......#......... ;5
			dg	#.######........ ;6
			dg	.#####.#........ ;7
			dg	..#....#........ ;8
			dg	.....##......... ;9
			dg	..###........... ;10
			dg	.....#.#........ ;11
			dg	........#....... ;12
			dg	.........#...... ;13
			dg	........#.#..... ;14
			dg	................ ;15

gfx_flamingo2
			dg	.....#.##.#..... ;0
			dg	....#.##.#.##... ;1
			dg	....##.##.####.. ;2
			dg	.....##.....##.. ;3
			dg	...#...##..##... ;4
			dg	....##..#....... ;5
			dg	...#######...... ;6
			dg	.....###.#...... ;7
			dg	....#...##...... ;8
			dg	......###....... ;9
			dg	....#....##..... ;10
			dg	.....#.....#.... ;11
			dg	......#......... ;12
			dg	.......#........ ;13
			dg	.......#........ ;14
			dg	......#.#....... ;15

gfx_flamingo3
			dg	................ ;0
			dg	.......#.##.#... ;1
			dg	......#.##.#.##. ;2
			dg	......##.##.#### ;3
			dg	.......##.....## ;4
			dg	.........##..##. ;5
			dg	..........#..... ;6
			dg	.....#.#####.... ;7
			dg	......####.#.... ;8
			dg	....####..##.... ;9
			dg	.....#...##..... ;10
			dg	.......##....... ;11
			dg	...........#.... ;12
			dg	............#... ;13
			dg	............#... ;14
			dg	...........#.#.. ;15

gfx_flamingo4
			dg	................ ;0
			dg	...#.##.#....... ;1
			dg	.##.#.##.#...... ;2
			dg	####.##.##...... ;3
			dg	##.....##....... ;4
			dg	.##..##......... ;5
			dg	.....#.......... ;6
			dg	....#####.#..... ;7
			dg	....#.####...... ;8
			dg	....##..####.... ;9
			dg	.....##...#..... ;10
			dg	.......##....... ;11
			dg	....#........... ;12
			dg	...#............ ;13
			dg	...#............ ;14
			dg	..#.#........... ;15

gfx_flamingo5
			dg	.....#.##.#..... ;0
			dg	...##.#.##.#.... ;1
			dg	..####.##.##.... ;2
			dg	..##.....##..... ;3
			dg	...##..##...#... ;4
			dg	.......#..##.... ;5
			dg	......#######... ;6
			dg	......#.###..... ;7
			dg	......##...#.... ;8
			dg	.......###...... ;9
			dg	.....##....#.... ;10
			dg	....#.....#..... ;11
			dg	.........#...... ;12
			dg	........#....... ;13
			dg	........#....... ;14
			dg	.......#.#...... ;15

gfx_flamingo6
			dg	.......#.##.#... ;0
			dg	.....##.#.##.#.. ;1
			dg	....####.##.##.. ;2
			dg	....##.....##... ;3
			dg	.....##..##..... ;4
			dg	.........#....#. ;5
			dg	........######.. ;6
			dg	........#.#####. ;7
			dg	........#....#.. ;8
			dg	.........##..... ;9
			dg	...........###.. ;10
			dg	........#.#..... ;11
			dg	.......#........ ;12
			dg	......#......... ;13
			dg	.....#.#........ ;14
			dg	................ ;15

gfx_flamingo7
			dg	................ ;0
			dg	......##.#.##.#. ;1
			dg	.....####.#.##.# ;2
			dg	.....##..#.##.## ;3
			dg	......##.....##. ;4
			dg	...........##... ;5
			dg	...........#.... ;6
			dg	..........####.. ;7
			dg	..........#.###. ;8c
			dg	..........#.#### ;9
			dg	...........#..## ;10
			dg	........###.##.# ;11
			dg	.......#.#.....# ;12
			dg	............#... ;13
			dg	............#... ;14
			dg	...........#.#.. ;15

			IF 0
gfx_walrus0
			dg	................ ; 0
			dg	......##.##..... ; 1
			dg	....#.#.#.#..... ; 2
			dg	....##.#.#...... ; 3
			dg	...##.###.#..... ; 4
			dg	...##.#####..... ; 5
			dg	...##.#..#...... ; 6
			dg	....##.##.#..... ; 7
			dg	....##.##.#..... ; 8
			dg	#.#..#..#.#..... ; 9
			dg	###.###.#.#..... ; 10
			dg	.#.####.#.#..... ; 11
			dg	########........ ; 12
			dg	####.##.#....... ; 13
			dg	####.##.#....... ; 14
			dg	.##.##..##...... ; 15

gfx_walrus1
			dg	................ ; 0
			dg	................ ; 1
			dg	........##.##... ; 2
			dg	......#.#.#.#... ; 3
			dg	......##.#.#.... ; 4
			dg	.....##.###.#... ; 5
			dg	.....##.#####... ; 6
			dg	.....##.#..#.... ; 7
			dg	......##.##.#... ; 8
			dg	......##.##.#... ; 9
			dg	#.#....#..#.#... ; 10
			dg	###...###.#.#... ; 11
			dg	.########.#.#... ; 12
			dg	.####.####...... ; 13
			dg	..####.##.##.... ; 14
			dg	...###.###.##... ; 15

gfx_walrus2
			dg	................ ; 0
			dg	.........##.##.. ; 1
			dg	.......#.#.#.#.. ; 2
			dg	.......##.#.#... ; 3
			dg	......##.###.#.. ; 4
			dg	......##.#####.. ; 5
			dg	......##.#..#... ; 6
			dg	.......##.##.#.. ; 7
			dg	.......##.##.#.. ; 8
			dg	...#.#..#..#.#.. ; 9
			dg	...###.###.#.#.. ; 10
			dg	....#..###.#.#.. ; 11
			dg	...########.#... ; 12
			dg	...####.##.##... ; 13
			dg	...####.##.##... ; 14
			dg	....###.###.##.. ; 15

gfx_walrus3
			dg	................ ; 0
			dg	................ ; 1
			dg	...........##.## ; 2
			dg	.........#.#.#.# ; 3
			dg	.........##.#.#. ; 4
			dg	........##.###.# ; 5
			dg	........##.##### ; 6
			dg	........##.#..#. ; 7
			dg	.........##.##.# ; 8
			dg	..........#.##.# ; 9
			dg	..........#..#.# ; 10
			dg	.........###.#.# ; 11
			dg	...#.#...###.#.# ; 12
			dg	....#########.#. ; 13
			dg	.....####.##.##. ; 14
			dg	......##.###.### ; 15

gfx_walrus4
			dg	................ ; 0
			dg	................ ; 1
			dg	##.##........... ; 2
			dg	#.#.#.#......... ; 3
			dg	.#.#.##......... ; 4
			dg	#.###.##........ ; 5
			dg	#####.##........ ; 6
			dg	.#..#.##........ ; 7
			dg	#.##.##......... ; 8
			dg	#.##.#.......... ; 9
			dg	#.#..#.......... ; 10
			dg	#.#.###......... ; 11
			dg	#.#.###...#.#... ; 12
			dg	.#.#########.... ; 13
			dg	.##.##.####..... ; 14
			dg	###.###.##...... ; 15

gfx_walrus5
			dg	................ ; 0
			dg	..##.##......... ; 1
			dg	..#.#.#.#....... ; 2
			dg	...#.#.##....... ; 3
			dg	..#.###.##...... ; 4
			dg	..#####.##...... ; 5
			dg	...#..#.##...... ; 6
			dg	..#.##.##....... ; 7
			dg	..#.##.##....... ; 8
			dg	..#.#..#..#.#... ; 9
			dg	..#.#.###.###... ; 10
			dg	..#.#.###..#.... ; 11
			dg	...#.########... ; 12
			dg	...##.##.####... ; 13
			dg	...##.##.####... ; 14
			dg	..##.###.###.... ; 15

gfx_walrus6
			dg	................ ; 0
			dg	................ ; 1
			dg	...##.##........ ; 2
			dg	...#.#.#.#...... ; 3
			dg	....#.#.##...... ; 4
			dg	...#.###.##..... ; 5
			dg	...#####.##..... ; 6
			dg	....#..#.##..... ; 7
			dg	...#.##.##...... ; 8
			dg	...#.##.##...... ; 9
			dg	...#.#..#....#.# ; 10
			dg	...#.#.###...### ; 11
			dg	...#.#.########. ; 12
			dg	......####.####. ; 13
			dg	....##.##.####.. ; 14
			dg	...##.###.###... ; 15

gfx_walrus7
			dg	................ ; 0
			dg	.....##.##...... ; 1
			dg	.....#.#.#.#.... ; 2
			dg	......#.#.##.... ; 3
			dg	.....#.###.##... ; 4
			dg	.....#####.##... ; 5
			dg	......#..#.##... ; 6
			dg	.....#.##.##.... ; 7
			dg	.....#.##.##.... ; 8
			dg	.....#.#..#..#.# ; 9
			dg	.....#.#.###.### ; 10
			dg	.....#.#.####.#. ; 11
			dg	........######## ; 12
			dg	.......#.##.#### ; 13
			dg	.......#.##.#### ; 14
			dg	......##..##.##. ; 15
			ENDIF

gfx_seal0 ; not changed in new version
			dg	.....####....... ; 0
			dg	....###..#...... ; 1
			dg	....###..#...... ; 2
			dg	....###..#...... ; 3
			dg	.....####....... ; 4
			dg	.......#........ ; 5
			dg	......##........ ; 6
			dg	.....#.##....... ; 7
			dg	.....####....... ; 8
			dg	......###....... ; 9
			dg	#.#..####....... ; 10
			dg	###..####....... ; 11
			dg	.#..#####....... ; 12
			dg	.########....... ; 13
			dg	.######.#....... ; 14
			dg	..##.###.#...... ; 15

gfx_seal1 ; changed from row 9 down
			dg	................ ; 0
			dg	.......####..... ; 1
			dg	......##..##.... ; 2
			dg	......##..##.... ; 3
			dg	......##..##.... ; 4
			dg	.......####..... ; 5
			dg	.........#...... ; 6
			dg	........##...... ; 7
			dg	.......#.##..... ; 8
			dg	.......####..... ; 9
			dg	........###..... ; 10
			dg	..#.#..####..... ; 11
			dg	..###.#####..... ; 12
			dg	...#..###.#..... ; 13
			dg	...#######.#.... ; 14
			dg	....###.###..... ; 15

gfx_seal2 ; ball moved up and row 12 changed
			dg	.........####... ; 0
			dg	........#..###.. ; 1
			dg	........#..###.. ; 2
			dg	........#..###.. ; 3
			dg	.........####... ; 4
			dg	................ ; 5
			dg	................ ; 6
			dg	...........#.... ; 7
			dg	.....#....##.... ; 8
			dg	.....###.#.##... ; 9
			dg	.....##..####... ; 10
			dg	....##....###... ; 11
			dg	....###..####... ; 12
			dg	....#########... ; 13
			dg	.....#####.#.... ; 14
			dg	......##.##.#... ; 15

gfx_seal3 ; row 11, 12, 15 changed
			dg	................ ; 0
			dg	................ ; 1
			dg	................ ; 2
			dg	...........####. ; 3
			dg	..........#.##.# ; 4
			dg	..........#.##.# ; 5
			dg	..........#.##.# ; 6
			dg	...........####. ; 7
			dg	.......#.#...#.. ; 8
			dg	.......##...##.. ; 9
			dg	.......#...#.##. ; 10
			dg	......##...####. ; 11
			dg	......###..####. ; 12
			dg	......#########. ; 13
			dg	.......#####.#.. ; 14
			dg	.........####.#. ; 15

gfx_seal4 ; row 11, 12, 15 changed
			dg	................ ; 0
			dg	................ ; 1
			dg	................ ; 2
			dg	.####........... ; 3
			dg	#.##.#.......... ; 4
			dg	#.##.#.......... ; 5
			dg	#.##.#.......... ; 6
			dg	.####........... ; 7
			dg	..#...#.#....... ; 8
			dg	..##...##....... ; 9
			dg	.##.#...#....... ; 10
			dg	.####...##...... ; 11
			dg	.####..###...... ; 12
			dg	.#########...... ; 13
			dg	..#.#####....... ; 14
			dg	.#.####......... ; 15

gfx_seal5 ; ball moved up and row 12 changed
			dg	...####......... ; 0
			dg	..###..#........ ; 1
			dg	..###..#........ ; 2
			dg	..###..#........ ; 3
			dg	...####......... ; 4
			dg	................ ; 5
			dg	................ ; 6
			dg	....#........... ; 7
			dg	....##....#..... ; 8
			dg	...##.#.###..... ; 9
			dg	...####..##..... ; 10
			dg	...###....##.... ; 11
			dg	...####..###.... ; 12
			dg	...#########.... ; 13
			dg	....#.#####..... ; 14
			dg	...#.##.##...... ; 15

gfx_seal6  ; changed from row 9 down
			dg	................ ; 0
			dg	.....####....... ; 1
			dg	....##..##...... ; 2
			dg	....##..##...... ; 3
			dg	....##..##...... ; 4
			dg	.....####....... ; 5
			dg	......#......... ; 6
			dg	......##........ ; 7
			dg	.....##.#....... ; 8
			dg	.....####....... ; 9
			dg	.....###........ ; 10
			dg	.....####..#.#.. ; 11
			dg	.....#####.###.. ; 12
			dg	.....#.###..#... ; 13
			dg	....#.#######... ; 14
			dg	.....###.###.... ; 15

gfx_seal7  ; not changed in new version
			dg	.......####..... ; 0
			dg	......#..###.... ; 1
			dg	......#..###.... ; 2
			dg	......#..###.... ; 3
			dg	.......####..... ; 4
			dg	........#....... ; 5
			dg	........##...... ; 6
			dg	.......##.#..... ; 7
			dg	.......####..... ; 8
			dg	.......###...... ; 9
			dg	.......####..#.# ; 10
			dg	.......####..### ; 11
			dg	.......#####..#. ; 12
			dg	.......########. ; 13
			dg	.......#.######. ; 14
			dg	......#.###.##.. ; 15

gfx_toilet0
C300	DEFB $C0,$00,$C0,$00,$C0,$00,$C0,$00
C308	DEFB $C0,$00,$C0,$00,$C0,$00,$DF,$C0
C310	DEFB $DF,$C0,$FF,$C0,$1F,$C0,$0F,$80
C318	DEFB $77,$80,$FF,$00,$DF,$00,$DF,$00
C320	DEFB $30,$00,$30,$00,$30,$00,$30,$00
C328	DEFB $30,$20,$30,$C0,$33,$00,$34,$00
C330	DEFB $37,$F0,$3F,$F0,$07,$F0,$03,$E0
C338	DEFB $1D,$E0,$3F,$C0,$37,$C0,$37,$C0
C340	DEFB $0C,$00,$0C,$00,$0C,$20,$0C,$40
C348	DEFB $0C,$40,$0C,$80,$0C,$80,$0D,$00
C350	DEFB $0D,$FC,$0F,$FC,$01,$FC,$00,$F8
C358	DEFB $07,$78,$0F,$F0,$0D,$F0,$0D,$F0
C360	DEFB $03,$00,$03,$00,$03,$00,$03,$00
C368	DEFB $03,$02,$03,$0C,$03,$30,$03,$40
C370	DEFB $03,$7F,$03,$FF,$00,$7F,$00,$3E
C378	DEFB $01,$DE,$03,$FC,$03,$7C,$03,$7C
C380	DEFB $00,$C0,$00,$C0,$00,$C0,$00,$C0
C388	DEFB $40,$C0,$30,$C0,$0C,$C0,$02,$C0
C390	DEFB $FE,$C0,$FF,$C0,$FE,$00,$7C,$00
C398	DEFB $7B,$80,$3F,$C0,$3E,$C0,$3E,$C0
C3A0	DEFB $00,$30,$00,$30,$04,$30,$02,$30
C3A8	DEFB $02,$30,$01,$30,$01,$30,$00,$B0
C3B0	DEFB $3F,$B0,$3F,$F0,$3F,$80,$1F,$00
C3B8	DEFB $1E,$E0,$0F,$F0,$0F,$B0,$0F,$B0
C3C0	DEFB $00,$0C,$00,$0C,$00,$0C,$00,$0C
C3C8	DEFB $04,$0C,$03,$0C,$00,$CC,$00,$2C
C3D0	DEFB $0F,$EC,$0F,$FC,$0F,$E0,$07,$C0
C3D8	DEFB $07,$B8,$03,$FC,$03,$EC,$03,$EC
C3E0	DEFB $00,$03,$00,$03,$00,$03,$00,$03
C3E8	DEFB $00,$03,$00,$03,$00,$03,$03,$FB
C3F0	DEFB $03,$FB,$03,$FF,$03,$F8,$01,$F0
C3F8	DEFB $01,$EE,$00,$FF,$00,$FB,$00,$FB

gfx_pacman0
C700	DEFB $1F,$00,$7F,$C0,$73,$E0,$F3,$80	
C708	DEFB $FE,$00,$F8,$00,$FE,$00,$FF,$80
C710	DEFB $7F,$E0,$7F,$C0,$1F,$00,$0A,$00
C718	DEFB $0A,$00,$0A,$00,$0A,$00,$1F,$00
C720	DEFB $07,$C0,$1F,$F0,$1E,$70,$3E,$78
C728	DEFB $3F,$F8,$3E,$00,$3F,$F8,$3F,$F8
C730	DEFB $1F,$F0,$1F,$F0,$07,$C0,$02,$80
C738	DEFB $02,$80,$07,$C0,$00,$00,$00,$00
C740	DEFB $01,$F0,$07,$FC,$07,$3E,$0F,$38
C748	DEFB $0F,$E0,$0F,$80,$0F,$E0,$0F,$F8
C750	DEFB $07,$FE,$07,$FC,$01,$F0,$01,$F0
C758	DEFB $00,$00,$00,$00,$00,$00,$00,$00
C760	DEFB $00,$7C,$01,$CF,$01,$CE,$03,$FC
C768	DEFB $03,$F0,$03,$E0,$03,$F0,$03,$FC
C770	DEFB $01,$FE,$01,$FF,$00,$7C,$00,$28
C778	DEFB $00,$28,$00,$7C,$00,$00,$00,$00
C780	DEFB $3E,$00,$F3,$80,$73,$80,$3F,$C0
C788	DEFB $0F,$C0,$07,$C0,$0F,$C0,$3F,$C0
C790	DEFB $7F,$80,$FF,$80,$3E,$00,$14,$00
C798	DEFB $14,$00,$3E,$00,$00,$00,$00,$00
C7A0	DEFB $0F,$80,$3F,$E0,$7C,$E0,$1C,$F0
C7A8	DEFB $07,$F0,$01,$F0,$07,$F0,$1F,$F0
C7B0	DEFB $7F,$E0,$3F,$E0,$0F,$80,$0F,$80
C7B8	DEFB $00,$00,$00,$00,$00,$00,$00,$00
C7C0	DEFB $03,$E0,$0F,$F8,$0E,$78,$1E,$7C
C7C8	DEFB $1F,$FC,$00,$7C,$1F,$FC,$1F,$FC
C7D0	DEFB $0F,$F8,$0F,$F8,$03,$E0,$01,$40
C7D8	DEFB $01,$40,$03,$E0,$00,$00,$00,$00
C7E0	DEFB $00,$F8,$03,$FE,$07,$CE,$01,$CF
C7E8	DEFB $00,$7F,$00,$1F,$00,$7F,$01,$FF
C7F0	DEFB $07,$FE,$03,$FE,$00,$F8,$00,$50
C7F8	DEFB $00,$50,$00,$50,$00,$50,$00,$F8

gfx_kangaroo0
			dg	##..##.#........ ; 0
			dg	.##.#.#......... ; 1
			dg	..#.##.##....... ; 2
			dg	...#..####...... ; 3
			dg	...#...###...... ; 4
			dg	...#....#....... ; 5
			dg	...###.......... ; 6
			dg	...##.#.#....... ; 7
			dg	..###........... ; 8
			dg	..#####......... ; 9
			dg	..######........ ; a
			dg	..######........ ; b
			dg	..#####......... ; c
			dg	..##............ ; d
			dg	..##.##.#....... ; e
			dg	##.####.#....... ; f

gfx_kangaroo1
			dg	..##..##.#...... ; 0
			dg	...##.#.#....... ; 1
			dg	....#.##.##..... ; 2
			dg	.....#..####.... ; 3
			dg	.....#...###.... ; 4
			dg	.....###..#..... ; 5
			dg	....###.#....... ; 6
			dg	....####..#..... ; 7
			dg	...######....... ; 8
			dg	...######....... ; 9
			dg	...####......... ; a
			dg	...#.#.......... ; b
			dg	...#.#.##.#..... ; c
			dg	..#..####.#..... ; d
			dg	.#.............. ; e
			dg	................ ; f

gfx_kangaroo2
			dg	....##..##.#.... ; 0
			dg	.....##.#.#..... ; 1
			dg	......#.##.##... ; 2
			dg	.......#..####.. ; 3
			dg	.......#...###.. ; 4
			dg	.......###..#... ; 5
			dg	......#####..... ; 6
			dg	.....#####.#.#.. ; 7
			dg	....##..#....... ; 8
			dg	....####.##..... ; 9
			dg	....####.##.#... ; a
			dg	#...##.###.##... ; b
			dg	.####..##.#..... ; c
			dg	................ ; d
			dg	................ ; e
			dg	................ ; f

gfx_kangaroo3
			dg	................ ; 0
			dg	................ ; 1
			dg	......##..##.#.. ; 2
			dg	.......##.#.#... ; 3
			dg	........#.##.##. ; 4
			dg	.........#..#### ; 5
			dg	.........#...### ; 6
			dg	.........###..#. ; 7
			dg	........###.#... ; 8
			dg	........####..#. ; 9
			dg	.......######... ; a
			dg	.......######... ; b
			dg	.......####..... ; c
			dg	.......#.#...... ; d
			dg	...#...#.#.##.#. ; e
			dg	....###..####.#. ; f

gfx_kangaroo4
			dg	................ ; 0
			dg	................ ; 1
			dg	..#.##..##...... ; 2
			dg	...#.#.##....... ; 3
			dg	.##.##.#........ ; 4
			dg	####..#......... ; 5
			dg	###...#......... ; 6
			dg	.#..###......... ; 7
			dg	...#.###........ ; 8
			dg	.#..####........ ; 9
			dg	...######....... ; a
			dg	...######....... ; b
			dg	.....####....... ; c
			dg	......#.#....... ; d
			dg	.#.##.#.#...#... ; e
			dg	.#.####..###.... ; f

gfx_kangaroo5
			dg	....#.##..##.... ; 0
			dg	.....#.#.##..... ; 1
			dg	...##.##.#...... ; 2
			dg	..####..#....... ; 3
			dg	..###...#....... ; 4
			dg	...#..###....... ; 5
			dg	.....#####...... ; 6
			dg	..#.#.#####..... ; 7
			dg	.......#..##.... ; 8
			dg	.....##.####.... ; 9
			dg	...#.##.####.... ; a
			dg	...##.###.##...# ; b
			dg	.....#.##..####. ; c
			dg	................ ; d
			dg	................ ; e
			dg	................ ; f

gfx_kangaroo6
			dg	......#.##..##.. ; 0
			dg	.......#.#.##... ; 1
			dg	.....##.##.#.... ; 2
			dg	....####..#..... ; 3
			dg	....###...#..... ; 4
			dg	.....#..###..... ; 5
			dg	.......#.###.... ; 6
			dg	.....#..####.... ; 7
			dg	.......######... ; 8
			dg	.......######... ; 9
			dg	.........####... ; a
			dg	..........#.#... ; b
			dg	.....#.##.#.#... ; c
			dg	.....#.####..#.. ; d
			dg	..............#. ; e
			dg	................ ; f

gfx_kangaroo7
			dg	........#.##..## ; 0
			dg	.........#.#.##. ; 1
			dg	.......##.##.#.. ; 2
			dg	......####..#... ; 3
			dg	......###...#... ; 4
			dg	.......#....#... ; 5
			dg	..........###... ; 6
			dg	.......#.#.##... ; 7
			dg	...........###.. ; 8
			dg	.........#####.. ; 9
			dg	........######.. ; a
			dg	........######.. ; b
			dg	.........#####.. ; c
			dg	............##.. ; d
			dg	.......#.##.##.. ; e
			dg	.......#.####.## ; f


	IF 0
CB00	DEFB $18,$00,$1C,$00,$0A,$80,$0F,$80	
CB08	DEFB $0C,$00,$1C,$00,$1E,$00,$1D,$00
CB10	DEFB $3C,$00,$3E,$00,$3E,$00,$6E,$00
CB18	DEFB $44,$00,$42,$00,$81,$00,$00,$00
CB20	DEFB $00,$00,$00,$00,$06,$00,$07,$00
CB28	DEFB $02,$A0,$03,$E0,$03,$80,$07,$00
CB30	DEFB $07,$80,$07,$40,$0F,$00,$0F,$80
CB38	DEFB $0F,$80,$1B,$80,$33,$00,$40,$C0
CB40	DEFB $00,$00,$00,$00,$00,$00,$00,$00
CB48	DEFB $01,$80,$01,$C0,$00,$A8,$00,$F8
CB50	DEFB $00,$E0,$01,$C0,$01,$E0,$01,$D0
CB58	DEFB $03,$C0,$03,$E0,$07,$E0,$3E,$F8
CB60	DEFB $00,$00,$00,$00,$00,$60,$00,$70
CB68	DEFB $00,$2A,$00,$3E,$00,$38,$00,$70
CB70	DEFB $00,$78,$00,$74,$00,$F0,$00,$F8
CB78	DEFB $01,$F8,$01,$B0,$03,$0C,$04,$00
CB80	DEFB $00,$00,$00,$00,$06,$00,$0E,$00
CB88	DEFB $54,$00,$7C,$00,$1C,$00,$0E,$00
CB90	DEFB $1E,$00,$2E,$00,$0F,$00,$1F,$00
CB98	DEFB $1F,$80,$0D,$80,$30,$C0,$00,$20
CBA0	DEFB $00,$00,$00,$00,$00,$00,$00,$00
CBA8	DEFB $01,$80,$03,$80,$15,$00,$1F,$00
CBB0	DEFB $07,$00,$03,$80,$07,$80,$0B,$80
CBB8	DEFB $03,$C0,$07,$C0,$07,$E0,$1F,$7C
CBC0	DEFB $00,$00,$00,$00,$00,$60,$00,$E0
CBC8	DEFB $05,$40,$07,$C0,$01,$C0,$00,$E0
CBD0	DEFB $01,$E0,$02,$E0,$00,$F0,$01,$F0
CBD8	DEFB $01,$F0,$01,$D8,$00,$CC,$03,$02
CBE0	DEFB $00,$18,$00,$38,$01,$50,$01,$F0
CBE8	DEFB $00,$30,$00,$38,$00,$78,$00,$B8
CBF0	DEFB $00,$3C,$00,$7C,$00,$7C,$00,$76
CBF8	DEFB $00,$22,$00,$42,$00,$81,$00,$00
	ENDIF

gfx_ewok0
			dg	##..##.......... ; 0
			dg	######.......... ; 1
			dg	.##............. ; 2
			dg	.#.###.......... ; 3
			dg	#.#.#........... ; 4
			dg	#.##.#.#........ ; 5
			dg	#.##.#..##...... ; 6
			dg	##.###..###..... ; 7
			dg	##.....####..... ; 8
			dg	##.######..#.... ; 9
			dg	##..####....#... ; 10
			dg	..##.........#.. ; 11
			dg	.#.#####......#. ; 12
			dg	.##..#####...... ; 13
			dg	###..#####...... ; 14
			dg	###............. ; 15

			dg	................ ; 0
			dg	.##..##....#.... ; 1
			dg	.#######...##... ; 2
			dg	..##.......##... ; 3
			dg	..#.####....#... ; 4
			dg	.#.##.#.....#... ; 5
			dg	.#.###.#....#... ; 6
			dg	.#.##........... ; 7
			dg	##...##.....#... ; 8
			dg	##.###########.. ; 9
			dg	.#.##########... ; 10
			dg	.#.............. ; 11
			dg	..######.....#.. ; 12
			dg	.#########...#.. ; 13
			dg	####...#####.#.. ; 14
			dg	###......###.#.. ; 15

			dg	....##...##..#.. ; 0
			dg	....#######.###. ; 1
			dg	.....##.....###. ; 2
			dg	.....#.####..#.. ; 3
			dg	....#.#.#.#..#.. ; 4
			dg	....#.##.##..#.. ; 5
			dg	...##.#...#..#.. ; 6
			dg	..###..###...... ; 7
			dg	.###..#.....##.. ; 8
			dg	###.#.#########. ; 9
			dg	#..##..#######.. ; 10
			dg	...##.#......... ; 11
			dg	...#.#######.#.. ; 12
			dg	....########.#.. ; 13
			dg	...###..###..#.. ; 14
			dg	..###...####.#.. ; 15

			dg	.......##.....## ; 0
			dg	.......#####.### ; 1
			dg	........##....#. ; 2
			dg	........#.###.#. ; 3
			dg	.......#.#.#.##. ; 4
			dg	.......#.##..#.. ; 5
			dg	.......#.#...#.. ; 6
			dg	......##..##.... ; 7
			dg	......##.#...#.. ; 8
			dg	......#..##.###. ; 9
			dg	.....#...######. ; 10
			dg	.........#####.. ; 11
			dg	..........###... ; 12
			dg	......####...... ; 13
			dg	......#####.#... ; 14
			dg	......#.....#... ; 15

			dg	##.....##....... ; 0
			dg	###.#####....... ; 1
			dg	.#....##........ ; 2
			dg	.#.###.#........ ; 3
			dg	.##.#.#.#....... ; 4
			dg	..#..##.#....... ; 5
			dg	..#...#.#....... ; 6
			dg	....##..##...... ; 7
			dg	..#...#.##...... ; 8
			dg	.###.##..#...... ; 9
			dg	.######...#..... ; 10
			dg	..#####......... ; 11
			dg	...###.......... ; 12
			dg	......####...... ; 13
			dg	...#.#####...... ; 14
			dg	...#.....#...... ; 15

			dg	..#..##...##.... ; 0
			dg	.###.#######.... ; 1
			dg	.###.....##..... ; 2
			dg	..#..####.#..... ; 3
			dg	..#..#.#.#.#.... ; 4
			dg	..#..##.##.#.... ; 5
			dg	..#..#...#.##... ; 6
			dg	......###..###.. ; 7
			dg	..##.....#..###. ; 8
			dg	.#########.#.### ; 9
			dg	..#######..##..# ; 10
			dg	.........#.##... ; 11
			dg	..#.#######.#... ; 12
			dg	..#.########.... ; 13
			dg	..#..###..###... ; 14
			dg	..#.####...###.. ; 15

			dg	................ ; 0
			dg	....#....##..##. ; 1
			dg	...##...#######. ; 2
			dg	...##.......##.. ; 3
			dg	...#....####.#.. ; 4
			dg	...#.....#.##.#. ; 5
			dg	...#....#.###.#. ; 6
			dg	...........##.#. ; 7
			dg	...#.....##...## ; 8
			dg	..###########.## ; 9
			dg	...##########.#. ; 10
			dg	..............#. ; 11
			dg	..#.....######.. ; 12
			dg	..#...#########. ; 13
			dg	..#.#####...#### ; 14
			dg	..#.###......### ; 15

			dg	..........##..## ; 0
			dg	..........###### ; 1
			dg	.............##. ; 2
			dg	..........###.#. ; 3
			dg	...........#.#.# ; 4
			dg	........#.#.##.# ; 5
			dg	......##..#.##.# ; 6
			dg	.....###..###.## ; 7
			dg	.....####.....## ; 8
			dg	....#..######.## ; 9
			dg	...#....####..## ; 10
			dg	..#.........##.. ; 11
			dg	.#......#####.#. ; 12
			dg	......#####..##. ; 13
			dg	......#####..### ; 14
			dg	.............### ; 15

	IF 0
D700	DEFB $70,$00,$50,$00,$7C,$00,$34,$00
D708	DEFB $3E,$00,$3E,$00,$18,$00,$3C,$00
D710	DEFB $7E,$00,$7E,$00,$F7,$00,$FB,$00
D718	DEFB $3C,$00,$76,$00,$6E,$00,$77,$00
D720	DEFB $1C,$00,$14,$00,$1F,$00,$0D,$00
D728	DEFB $0F,$80,$0F,$80,$06,$00,$0F,$00
D730	DEFB $1B,$80,$1B,$80,$1B,$80,$1D,$80
D738	DEFB $0F,$00,$06,$00,$06,$00,$07,$00
D740	DEFB $07,$00,$05,$00,$07,$C0,$03,$40
D748	DEFB $03,$E0,$03,$E0,$01,$80,$03,$C0
D750	DEFB $07,$E0,$07,$E0,$0F,$70,$0F,$B0
D758	DEFB $03,$C0,$07,$60,$06,$E0,$07,$70
D760	DEFB $01,$C0,$01,$40,$01,$F0,$00,$D0
D768	DEFB $00,$F8,$00,$F8,$00,$60,$00,$F0
D770	DEFB $01,$F8,$03,$FC,$07,$FE,$06,$F6
D778	DEFB $00,$F8,$01,$DA,$03,$0E,$03,$84
D780	DEFB $03,$80,$06,$80,$0F,$80,$0B,$00
D788	DEFB $1F,$00,$1F,$00,$06,$00,$0F,$00
D790	DEFB $1F,$80,$3F,$C0,$7F,$E0,$6F,$60
D798	DEFB $1F,$00,$5B,$80,$70,$C0,$21,$C0
D7A0	DEFB $00,$E0,$01,$A0,$03,$E0,$02,$C0
D7A8	DEFB $07,$C0,$07,$C0,$01,$80,$03,$C0
D7B0	DEFB $07,$E0,$07,$E0,$0E,$F0,$0D,$F0
D7B8	DEFB $03,$C0,$06,$E0,$07,$60,$0E,$E0
D7C0	DEFB $00,$38,$00,$68,$00,$F8,$00,$B0
D7C8	DEFB $01,$F0,$01,$F0,$00,$60,$00,$F0
D7D0	DEFB $01,$F8,$01,$D8,$01,$D8,$01,$B8
D7D8	DEFB $00,$F0,$00,$60,$00,$60,$00,$E0
D7E0	DEFB $00,$0E,$00,$1A,$00,$3E,$00,$2C
D7E8	DEFB $00,$7C,$00,$7C,$00,$18,$00,$3C
D7F0	DEFB $00,$7E,$00,$7E,$00,$EF,$00,$DF
D7F8	DEFB $00,$3C,$00,$6E,$00,$76,$00,$EE
	ENDIF

gfx_flagbug0
EF00	DEFB $00,$00,$08,$00,$14,$00,$2A,$00
EF08	DEFB $55,$00,$4A,$00,$84,$00,$80,$C0
EF10	DEFB $80,$C0,$41,$00,$7F,$80,$3F,$C0
EF18	DEFB $1F,$80,$0F,$00,$0A,$80,$12,$40
EF20	DEFB $2A,$00,$15,$00,$2A,$00,$15,$00
EF28	DEFB $20,$00,$20,$00,$20,$00,$20,$30
EF30	DEFB $20,$30,$10,$40,$1F,$E0,$0F,$F0
EF38	DEFB $07,$E0,$03,$C0,$02,$A0,$04,$90
EF40	DEFB $00,$00,$10,$00,$28,$00,$54,$00
EF48	DEFB $AA,$00,$51,$00,$21,$00,$01,$0C
EF50	DEFB $02,$0C,$02,$10,$03,$F8,$03,$FC
EF58	DEFB $01,$F8,$00,$F0,$00,$A8,$01,$24
EF60	DEFB $05,$40,$0A,$80,$05,$40,$0A,$80
EF68	DEFB $00,$40,$00,$40,$00,$40,$00,$43
EF70	DEFB $00,$83,$00,$84,$00,$FE,$00,$FF
EF78	DEFB $00,$7E,$00,$3C,$00,$2A,$00,$49
EF80	DEFB $02,$A0,$01,$50,$02,$A0,$01,$50
EF88	DEFB $02,$00,$02,$00,$02,$00,$C2,$00
EF90	DEFB $C1,$00,$21,$00,$7F,$00,$FF,$00
EF98	DEFB $7E,$00,$3C,$00,$54,$00,$92,$00
EFA0	DEFB $00,$00,$00,$08,$00,$14,$00,$2A
EFA8	DEFB $00,$55,$00,$8A,$00,$84,$30,$80
EFB0	DEFB $30,$40,$08,$40,$1F,$C0,$3F,$C0
EFB8	DEFB $1F,$80,$0F,$00,$15,$00,$24,$80
EFC0	DEFB $00,$54,$00,$A8,$00,$54,$00,$A8
EFC8	DEFB $00,$04,$00,$04,$00,$04,$0C,$04
EFD0	DEFB $0C,$04,$02,$08,$07,$F8,$0F,$F0
EFD8	DEFB $07,$E0,$03,$C0,$05,$40,$09,$20
EFE0	DEFB $00,$00,$00,$10,$00,$28,$00,$54
EFE8	DEFB $00,$AA,$00,$52,$00,$21,$03,$01
EFF0	DEFB $03,$01,$00,$82,$01,$FE,$03,$FC
EFF8	DEFB $01,$F8,$00,$F0,$01,$50,$02,$48

	IF 0
gfx_horz4framecollision0
			dg	##########...... ;0
			dg	#........#...... ;1
			dg	#........#...... ;2
			dg	#........#...... ;3
			dg	#...#....#...... ;4
			dg	#...##...#...... ;5
			dg	#....##..#...... ;6
			dg	#.######.#...... ;7
			dg	#.######.#...... ;8
			dg	#....##..#...... ;9
			dg	#...##...#...... ;10
			dg	#...#....#...... ;11
			dg	#........#...... ;12
			dg	#........#...... ;13
			dg	#........#...... ;14
			dg	##########...... ;15

gfx_horz4framecollision1
			dg	..##########.... ;0
			dg	..#........#.... ;1
			dg	..#........#.... ;2
			dg	..#........#.... ;3
			dg	..#...#....#.... ;4
			dg	..#...##...#.... ;5
			dg	..#....##..#.... ;6
			dg	..#.######.#.... ;7
			dg	..#.######.#.... ;8
			dg	..#....##..#.... ;9
			dg	..#...##...#.... ;10
			dg	..#...#....#.... ;11
			dg	..#........#.... ;12
			dg	..#........#.... ;13
			dg	..#........#.... ;14
			dg	..##########.... ;15

gfx_horz4framecollision2
			dg	....##########.. ;0
			dg	....#........#.. ;1
			dg	....#........#.. ;2
			dg	....#........#.. ;3
			dg	....#...#....#.. ;4
			dg	....#...##...#.. ;5
			dg	....#....##..#.. ;6
			dg	....#.######.#.. ;7
			dg	....#.######.#.. ;8
			dg	....#....##..#.. ;9
			dg	....#...##...#.. ;10
			dg	....#...#....#.. ;11
			dg	....#........#.. ;12
			dg	....#........#.. ;13
			dg	....#........#.. ;14
			dg	....##########.. ;15

gfx_horz4framecollision3
			dg	......########## ;0
			dg	......#........# ;1
			dg	......#........# ;2
			dg	......#........# ;3
			dg	......#...#....# ;4
			dg	......#...##...# ;5
			dg	......#....##..# ;6
			dg	......#.######.# ;7
			dg	......#.######.# ;8
			dg	......#....##..# ;9
			dg	......#...##...# ;10
			dg	......#...#....# ;11
			dg	......#........# ;12
			dg	......#........# ;13
			dg	......#........# ;14
			dg	......########## ;15

gfx_horz4framecollision4
			dg	##########...... ;0
			dg	#........#...... ;1
			dg	#........#...... ;2
			dg	#........#...... ;3
			dg	#....#...#...... ;4
			dg	#...##...#...... ;5
			dg	#..##....#...... ;6
			dg	#.######.#...... ;7
			dg	#.######.#...... ;8
			dg	#..##....#...... ;9
			dg	#...##...#...... ;10
			dg	#....#...#...... ;11
			dg	#........#...... ;12
			dg	#........#...... ;13
			dg	#........#...... ;14
			dg	##########...... ;15

gfx_horz4framecollision5
			dg	..##########.... ;0
			dg	..#........#.... ;1
			dg	..#........#.... ;2
			dg	..#........#.... ;3
			dg	..#....#...#.... ;4
			dg	..#...##...#.... ;5
			dg	..#..##....#.... ;6
			dg	..#.######.#.... ;7
			dg	..#.######.#.... ;8
			dg	..#..##....#.... ;9
			dg	..#...##...#.... ;10
			dg	..#....#...#.... ;11
			dg	..#........#.... ;12
			dg	..#........#.... ;13
			dg	..#........#.... ;14
			dg	..##########.... ;15

gfx_horz4framecollision6
			dg	....##########.. ;0
			dg	....#........#.. ;1
			dg	....#........#.. ;2
			dg	....#........#.. ;3
			dg	....#....#...#.. ;4
			dg	....#...##...#.. ;5
			dg	....#..##....#.. ;6
			dg	....#.######.#.. ;7
			dg	....#.######.#.. ;8
			dg	....#..##....#.. ;9
			dg	....#...##...#.. ;10
			dg	....#....#...#.. ;11
			dg	....#........#.. ;12
			dg	....#........#.. ;13
			dg	....#........#.. ;14
			dg	....##########.. ;15

gfx_horz4framecollision7
			dg	......########## ;0
			dg	......#........# ;1
			dg	......#........# ;2
			dg	......#........# ;3
			dg	......#....#...# ;4
			dg	......#...##...# ;5
			dg	......#..##....# ;6
			dg	......#.######.# ;7
			dg	......#.######.# ;8
			dg	......#..##....# ;9
			dg	......#...##...# ;10
			dg	......#....#...# ;11
			dg	......#........# ;12
			dg	......#........# ;13
			dg	......#........# ;14
			dg	......########## ;15

gfx_4framehguardian0:
			dg	##########...... ;0
			dg	###......#...... ;1
			dg	###......#...... ;2
			dg	###......#...... ;3
			dg	###......#...... ;4
			dg	###......#...... ;5
			dg	###......#...... ;6
			dg	###......#...... ;7
			dg	###......#...... ;8
			dg	###......#...... ;9
			dg	###......#...... ;10
			dg	###......#...... ;11
			dg	###......#...... ;12
			dg	###......#...... ;13
			dg	###......#...... ;14
			dg	##########...... ;15

gfx_4framehguardian1
			dg	..##########.... ;0
			dg	..#..##....#.... ;1
			dg	..#..##....#.... ;2
			dg	..#..##....#.... ;3
			dg	..#..##....#.... ;4
			dg	..#..##....#.... ;5
			dg	..#..##....#.... ;6
			dg	..#..##....#.... ;7
			dg	..#..##....#.... ;8
			dg	..#..##....#.... ;9
			dg	..#..##....#.... ;10
			dg	..#..##....#.... ;11
			dg	..#..##....#.... ;12
			dg	..#..##....#.... ;13
			dg	..#..##....#.... ;14
			dg	..##########.... ;15

gfx_4framehguardian2
			dg	....##########.. ;0
			dg	....#....##..#.. ;1
			dg	....#....##..#.. ;2
			dg	....#....##..#.. ;3
			dg	....#....##..#.. ;4
			dg	....#....##..#.. ;5
			dg	....#....##..#.. ;6
			dg	....#....##..#.. ;7
			dg	....#....##..#.. ;8
			dg	....#....##..#.. ;9
			dg	....#....##..#.. ;10
			dg	....#....##..#.. ;11
			dg	....#....##..#.. ;12
			dg	....#....##..#.. ;13
			dg	....#....##..#.. ;14
			dg	....##########.. ;15

gfx_4framehguardian3
			dg	......########## ;0
			dg	......#......### ;1
			dg	......#......### ;2
			dg	......#......### ;3
			dg	......#......### ;4
			dg	......#......### ;5
			dg	......#......### ;6
			dg	......#......### ;7
			dg	......#......### ;8
			dg	......#......### ;9
			dg	......#......### ;10
			dg	......#......### ;11
			dg	......#......### ;12
			dg	......#......### ;13
			dg	......#......### ;14
			dg	......########## ;15

	ENDIF

gfx_satellitedish0
			dg	....##.......... ; 0
			dg	...#.##......... ; 1
			dg	..#.##.#........ ; 2
			dg	.#..##..#....... ; 3
			dg	#...##...#...... ; 4
			dg	#...##...#...... ; 5
			dg	.#..##..#....... ; 6
			dg	..#.##.#........ ; 7
			dg	...#.##......... ; 8
			dg	....##.......... ; 9
			dg	..##.###........ ; 10
			dg	.#..##.......... ; 11
			dg	.#########...... ; 12
			dg	##########...... ; 13
			dg	.#......#....... ; 14
			dg	..#.###......... ; 15

gfx_satellitedish1
			dg	......##........ ; 0
			dg	......##........ ; 1
			dg	.....#.##....... ; 2
			dg	.....####....... ; 3
			dg	....#.##.#...... ; 4
			dg	....#.##.#...... ; 5
			dg	.....####....... ; 6
			dg	.....#.##....... ; 7
			dg	......##........ ; 8
			dg	......##........ ; 9
			dg	.....#####...... ; 10
			dg	......##..#..... ; 11
			dg	..#########..... ; 12
			dg	..##########.... ; 13
			dg	...#......#..... ; 14
			dg	.....###.#...... ; 15

gfx_satellitedish2
			dg	........##...... ; 0
			dg	........##...... ; 1
			dg	........##...... ; 2
			dg	........##...... ; 3
			dg	........#....... ; 4
			dg	........#....... ; 5
			dg	........##...... ; 6
			dg	........##...... ; 7
			dg	........##...... ; 8
			dg	........##...... ; 9
			dg	.......###.#.... ; 10
			dg	.....#..##..#... ; 11
			dg	....##########.. ; 12
			dg	....#########... ; 13
			dg	............#... ; 14
			dg	......###.##.... ; 15

gfx_satellitedish3
			dg	..........##.... ; 0
			dg	.........##.#... ; 1
			dg	........#.##.#.. ; 2
			dg	........#.##.#.. ; 3
			dg	.......#..##..#. ; 4
			dg	.......#..##..#. ; 5
			dg	........#.##.#.. ; 6
			dg	........#.##.#.. ; 7
			dg	.........##.#... ; 8
			dg	..........##.... ; 9
			dg	........#.###... ; 10
			dg	.......#..##..#. ; 11
			dg	......########## ; 12
			dg	.......######### ; 13
			dg	.......#........ ; 14
			dg	........##.###.. ; 15

gfx_barrel0
CF80	DEFB $08,$00,$05,$00,$08,$80,$25,$00
CF88	DEFB $48,$80,$21,$00,$4C,$00,$33,$00
CF90	DEFB $44,$80,$44,$80,$88,$40,$84,$40
CF98	DEFB $48,$80,$48,$80,$33,$00,$0C,$00
CFA0	DEFB $02,$00,$11,$20,$0A,$40,$11,$20
CFA8	DEFB $0A,$40,$10,$20,$03,$00,$0C,$C0
CFB0	DEFB $10,$20,$10,$60,$22,$90,$25,$10
CFB8	DEFB $18,$20,$10,$20,$0C,$C0,$03,$00
CFC0	DEFB $00,$40,$02,$20,$04,$48,$02,$24
CFC8	DEFB $04,$48,$02,$04,$00,$C8,$03,$30
CFD0	DEFB $04,$08,$04,$08,$0B,$44,$08,$B4
CFD8	DEFB $04,$08,$04,$08,$03,$30,$00,$C0
CFE0	DEFB $00,$44,$01,$22,$02,$44,$01,$22
CFE8	DEFB $02,$44,$01,$02,$02,$30,$00,$CC
CFF0	DEFB $01,$42,$01,$22,$02,$11,$02,$21
CFF8	DEFB $01,$12,$01,$0A,$00,$CC,$00,$30

gfx_pole0
D380	DEFB $0C,$00,$0C,$00,$0C,$00,$0C,$00
D388	DEFB $0C,$00,$0C,$00,$0C,$00,$0C,$00
D390	DEFB $0C,$00,$0C,$00,$FF,$C0,$0C,$00
D398	DEFB $61,$80,$D2,$C0,$B3,$40,$61,$80
D3A0	DEFB $03,$00,$03,$00,$03,$00,$03,$00
D3A8	DEFB $03,$00,$03,$00,$03,$00,$03,$00
D3B0	DEFB $03,$00,$03,$00,$3F,$F0,$03,$00
D3B8	DEFB $18,$60,$24,$D0,$3C,$D0,$18,$60
D3C0	DEFB $00,$C0,$00,$C0,$00,$C0,$00,$C0
D3C8	DEFB $00,$C0,$00,$C0,$00,$C0,$00,$C0
D3D0	DEFB $00,$C0,$00,$C0,$0F,$FC,$00,$C0
D3D8	DEFB $06,$18,$0B,$34,$0D,$2C,$06,$18
D3E0	DEFB $00,$30,$00,$30,$00,$30,$00,$30
D3E8	DEFB $00,$30,$00,$30,$00,$30,$00,$30
D3F0	DEFB $00,$30,$00,$30,$03,$FF,$00,$30
D3F8	DEFB $01,$86,$02,$4D,$03,$CD,$01,$86

gfx_oremachine0
E380	DEFB $61,$80,$B2,$40,$B3,$C0,$61,$80
E388	DEFB $0C,$00,$FF,$C0,$52,$80,$12,$00
E390	DEFB $12,$00,$1E,$00,$0C,$00,$0C,$00
E398	DEFB $0C,$00,$0C,$00,$1E,$00,$3F,$00
E3A0	DEFB $18,$60,$24,$D0,$3C,$D0,$18,$60
E3A8	DEFB $03,$00,$3F,$F0,$14,$A0,$04,$80
E3B0	DEFB $04,$80,$07,$80,$03,$00,$03,$00
E3B8	DEFB $07,$80,$0F,$C0,$00,$00,$00,$00
E3C0	DEFB $06,$18,$0D,$3C,$0D,$24,$06,$18
E3C8	DEFB $00,$C0,$0F,$FC,$05,$28,$01,$20
E3D0	DEFB $01,$20,$01,$E0,$01,$E0,$03,$F0
E3D8	DEFB $00,$00,$00,$00,$00,$00,$00,$00
E3E0	DEFB $01,$86,$03,$CB,$02,$4B,$01,$86
E3E8	DEFB $00,$30,$03,$FF,$01,$4A,$00,$48
E3F0	DEFB $00,$48,$00,$78,$00,$30,$00,$30
E3F8	DEFB $00,$78,$00,$FC,$00,$00,$00,$00

gfx_cheque0
EB80	DEFB $00,$00,$00,$00,$00,$00,$00,$00
EB88	DEFB $00,$00,$00,$00,$00,$00,$00,$00
EB90	DEFB $00,$00,$00,$00,$FF,$C0,$81,$C0
EB98	DEFB $FF,$C0,$82,$40,$FE,$40,$FF,$C0
EBA0	DEFB $00,$00,$00,$00,$00,$00,$00,$00
EBA8	DEFB $00,$00,$00,$00,$3F,$F0,$20,$70
EBB0	DEFB $3F,$F0,$20,$90,$3F,$90,$3F,$F0
EBB8	DEFB $00,$00,$00,$00,$00,$00,$00,$00
EBC0	DEFB $00,$00,$00,$00,$00,$00,$00,$00
EBC8	DEFB $0F,$FC,$08,$1C,$0F,$FC,$08,$24
EBD0	DEFB $0F,$E4,$0F,$FC,$00,$00,$00,$00
EBD8	DEFB $00,$00,$00,$00,$00,$00,$00,$00
EBE0	DEFB $00,$00,$00,$00,$00,$00,$00,$00
EBE8	DEFB $00,$00,$00,$00,$03,$FF,$02,$07
EBF0	DEFB $03,$FF,$02,$09,$03,$F9,$03,$FF
EBF8	DEFB $00,$00,$00,$00,$00,$00,$00,$00

gfx_warehouse0
F380	DEFB $7E,$00,$99,$00,$FF,$00,$DB,$00
F388	DEFB $E7,$00,$7E,$00,$24,$00,$24,$00
F390	DEFB $24,$00,$42,$00,$42,$00,$42,$00
F398	DEFB $81,$00,$81,$00,$C3,$00,$C3,$00
F3A0	DEFB $00,$00,$1F,$80,$26,$40,$3F,$C0
F3A8	DEFB $36,$C0,$39,$C0,$1F,$80,$10,$80
F3B0	DEFB $20,$40,$20,$40,$40,$20,$40,$20
F3B8	DEFB $80,$10,$80,$30,$C0,$30,$C0,$00
F3C0	DEFB $00,$00,$00,$00,$00,$00,$07,$E0
F3C8	DEFB $09,$90,$0F,$F0,$0D,$B0,$0E,$70
F3D0	DEFB $07,$E0,$08,$10,$10,$08,$20,$04
F3D8	DEFB $40,$02,$80,$01,$C0,$03,$C0,$03
F3E0	DEFB $00,$00,$01,$F8,$02,$64,$03,$FC
F3E8	DEFB $03,$6C,$03,$9C,$01,$F8,$01,$08
F3F0	DEFB $02,$04,$02,$04,$04,$02,$04,$02
F3F8	DEFB $08,$01,$0C,$01,$0C,$03,$00,$03

gfx_thresher0
FF80	DEFB $12,$00,$0C,$00,$1E,$00,$BF,$40
FF88	DEFB $73,$80,$73,$80,$BF,$40,$5E,$80
FF90	DEFB $4C,$80,$52,$80,$7F,$80,$0C,$00
FF98	DEFB $61,$80,$92,$C0,$B2,$40,$61,$80
FFA0	DEFB $03,$00,$07,$80,$07,$80,$1C,$E0
FFA8	DEFB $3B,$70,$3B,$70,$1C,$E0,$17,$A0
FFB0	DEFB $17,$A0,$13,$20,$1F,$E0,$03,$00
FFB8	DEFB $18,$60,$24,$90,$34,$B0,$18,$60
FFC0	DEFB $01,$E0,$01,$E0,$01,$20,$0E,$DC
FFC8	DEFB $0D,$EC,$0D,$EC,$0E,$DC,$05,$28
FFD0	DEFB $05,$E8,$05,$E8,$07,$F8,$00,$C0
FFD8	DEFB $06,$18,$0D,$24,$09,$34,$06,$18
FFE0	DEFB $00,$78,$00,$48,$00,$30,$03,$7B
FFE8	DEFB $02,$FD,$02,$FD,$03,$7B,$01,$32
FFF0	DEFB $01,$4A,$01,$7A,$01,$FE,$00,$30
FFF8	DEFB $01,$86,$02,$CD,$02,$49,$01,$86

gfx_reflector0
FB80	DEFB $06,$00,$0C,$00,$18,$00,$38,$00
FB88	DEFB $74,$00,$CA,$80,$85,$C0,$03,$C0
FB90	DEFB $06,$40,$CE,$C0,$D8,$40,$FF,$C0
FB98	DEFB $E2,$00,$C8,$80,$D5,$40,$08,$80
FBA0	DEFB $01,$80,$03,$00,$06,$00,$0E,$00
FBA8	DEFB $1D,$00,$32,$A0,$21,$70,$00,$F0
FBB0	DEFB $01,$90,$63,$B0,$66,$10,$7F,$F0
FBB8	DEFB $78,$80,$62,$20,$65,$50,$02,$20
FBC0	DEFB $00,$60,$00,$C0,$01,$80,$03,$80
FBC8	DEFB $07,$40,$0C,$A8,$08,$5C,$00,$3C
FBD0	DEFB $00,$64,$30,$EC,$31,$84,$3F,$FC
FBD8	DEFB $3E,$20,$30,$88,$31,$54,$00,$88
FBE0	DEFB $00,$18,$00,$30,$00,$60,$00,$E0
FBE8	DEFB $01,$D0,$03,$2A,$02,$17,$00,$0F
FBF0	DEFB $00,$19,$06,$3B,$06,$61,$07,$FF
FBF8	DEFB $07,$88,$06,$22,$06,$55,$00,$22

	ALIGN 256
vertical_guardians_page

gfx_skylab0:
E700	DEFB $03,$C0,$FF,$FF,$AB,$D5,$FF,$FF
E708	DEFB $13,$C8,$29,$94,$15,$A8,$0B,$D0
E710	DEFB $05,$A0,$03,$C0,$03,$C0,$05,$A0
E718	DEFB $0A,$50,$14,$28,$28,$14,$10,$08
E720	DEFB $00,$00,$00,$00,$03,$C0,$FF,$FF
E728	DEFB $AB,$D5,$FF,$FF,$13,$C8,$29,$94
E730	DEFB $15,$A8,$0B,$D0,$05,$A0,$03,$C0
E738	DEFB $03,$C0,$25,$A0,$4A,$54,$14,$2A
E740	DEFB $00,$00,$00,$00,$00,$00,$00,$07
E748	DEFB $03,$FD,$FF,$D7,$AB,$F8,$FF,$C0
E750	DEFB $03,$C0,$01,$80,$15,$A4,$4B,$D2
E758	DEFB $05,$A4,$23,$C2,$0B,$D0,$25,$A8
E760	DEFB $00,$00,$00,$00,$00,$00,$00,$20
E768	DEFB $02,$02,$00,$15,$03,$CE,$0F,$D4
E770	DEFB $CB,$C8,$B7,$C2,$E3,$C8,$31,$81
E778	DEFB $07,$E4,$C3,$C8,$17,$C2,$23,$FC
E780	DEFB $00,$00,$01,$00,$00,$00,$08,$20
E788	DEFB $00,$00,$00,$00,$21,$02,$00,$11
E790	DEFB $03,$8A,$0E,$90,$4B,$C0,$37,$02
E798	DEFB $62,$C0,$31,$01,$05,$E2,$C3,$44
E7A0	DEFB $00,$00,$00,$00,$00,$00,$00,$00
E7A8	DEFB $00,$00,$02,$00,$00,$00,$00,$20
E7B0	DEFB $10,$08,$0A,$84,$00,$20,$65,$00
E7B8	DEFB $22,$68,$08,$A0,$03,$D0,$17,$E0
E7C0	DEFB $00,$00,$00,$00,$00,$00,$00,$00
E7C8	DEFB $00,$00,$00,$00,$00,$00,$00,$00
E7D0	DEFB $02,$00,$00,$20,$10,$00,$00,$00
E7D8	DEFB $05,$10,$00,$68,$22,$A0,$0D,$D0
E7E0	DEFB $00,$00,$00,$00,$00,$00,$00,$00
E7E8	DEFB $00,$00,$00,$00,$00,$00,$00,$00
E7F0	DEFB $00,$00,$00,$00,$00,$00,$00,$80
E7F8	DEFB $00,$20,$08,$00,$02,$C0,$07,$60

gfx_telephone0 ; unchanged I think
			dg	..#############. ; 0
			dg	.###..###.###### ; 1
			dg	###.##...#.###.# ; 2
			dg	###.######.##..# ; 3
			dg	.#.#######.##### ; 4
			dg	..##....##..###. ; 5
			dg	.##.####.###.... ; 6
			dg	.#.##.###.#####. ; 7
			dg	.#.####.#.##.### ; 8
			dg	.#.#.####.###.## ; 9
			dg	.#.###.##.##..## ; 10
			dg	.##.####.#..##.# ; 11
			dg	.###....#.##...# ; 12
			dg	..#######....### ; 13
			dg	...############. ; 14
			dg	................ ; 15

gfx_telephone1
	IF 0 ; been replaced by former frame 2
			dg	..##########.... ; 0
			dg	.###..###.#####. ; 1
			dg	###.##...#.##### ; 2
			dg	###.######.###.# ; 3
			dg	.#.#....##.##..# ; 4
			dg	..#.####.#.##### ; 5
			dg	.#.###.##.#.###. ; 6
			dg	.#.#.####.##.... ; 7
			dg	.#.####.#.#####. ; 8
			dg	.#.##.###.##.### ; 9
			dg	.##.####.##..### ; 10
			dg	.###....#..##.## ; 11
			dg	.#######.##...## ; 12
			dg	.###.........### ; 13
			dg	..#############. ; 14
			dg	................ ; 15

gfx_telephone2
	ENDIF
			dg	................ ; 0
			dg	..##########.... ; 1
			dg	.###..###.#####. ; 2
			dg	###.##...#.##### ; 3
			dg	###.######.###.# ; 4
			dg	.#.#....##.##..# ; 5
			dg	..#.####.#.##### ; 6
			dg	.#.##.#.#.#.###. ; 7
			dg	.#.######.##.... ; 8
			dg	.#.#.##.#.#####. ; 9
			dg	.##.####.###..## ; 10
			dg	.###....##..##.# ; 11
			dg	.########.##...# ; 12
			dg	..#####.......## ; 13
			dg	............###. ; 14
			dg	.......######... ; 15

gfx_telephone2

			dg	........#######. ; 0
			dg	....#####..##### ; 1
			dg	..##..##.##.##.# ; 2
			dg	.##.##...##.#..# ; 3
			dg	###.....###.#### ; 4
			dg	.##.####.##.###. ; 5
			dg	...##.###.##.... ; 6
			dg	.#.#.##.#.#####. ; 7
			dg	.#.##.###.##.### ; 8
			dg	.##.####.#....## ; 9
			dg	.###....#.#.#..# ; 10
			dg	..######.......# ; 11
			dg	.............#.# ; 12
			dg	....##.####.#..# ; 13
			dg	...##.###.....## ; 14
			dg	....####..#####. ; 15

gfx_telephone3
			
			dg	....###########. ; 0
			dg	..##..###..##### ; 1
			dg	.##.##.#.##.##.# ; 2
			dg	###......##.#..# ; 3
			dg	.##.####.##.#### ; 4
			dg	...###.##.#.###. ; 5
			dg	...#.####.##.... ; 6
			dg	.#.###.##.#####. ; 7
			dg	.#...###.####### ; 8
			dg	..###...##....## ; 9
			dg	.##.##.##.#.#..# ; 10
			dg	.##.###........# ; 11
			dg	..##..###....#.# ; 12
			dg	...###.####.#..# ; 13
			dg	..............## ; 14
			dg	..........#####. ; 15

gfx_amoeba0
D300	DEFB $0A,$20,$16,$68,$01,$50,$39,$62	
D308	DEFB $65,$CE,$03,$D0,$FF,$EE,$87,$F1
D310	DEFB $77,$E4,$C7,$FF,$8B,$F1,$32,$AC
D318	DEFB $64,$A6,$49,$A2,$12,$90,$36,$98
D320	DEFB $00,$00,$05,$20,$03,$40,$31,$74
D328	DEFB $1D,$CC,$03,$D0,$3F,$EC,$07,$F4
D330	DEFB $3F,$E0,$67,$FC,$0B,$F2,$32,$AC
D338	DEFB $24,$A4,$0B,$90,$1A,$D8,$00,$C0
D340	DEFB $00,$00,$00,$00,$02,$20,$09,$60
D348	DEFB $05,$C8,$03,$D0,$1F,$E0,$07,$F8
D350	DEFB $1F,$E0,$17,$F8,$0F,$F0,$12,$A8
D358	DEFB $05,$A0,$0A,$B0,$00,$C0,$00,$00
D360	DEFB $00,$00,$05,$20,$03,$40,$31,$74
D368	DEFB $1D,$CC,$03,$D0,$3F,$EC,$07,$F4
D370	DEFB $3F,$E0,$67,$FC,$0B,$F2,$32,$AC
D378	DEFB $24,$A4,$0B,$90,$1A,$D8,$00,$C0

gfx_safe0
EB00	DEFB $61,$86,$9F,$F9,$9F,$F9,$61,$86
EB08	DEFB $03,$C0,$FF,$FF,$80,$01,$AA,$A9
EB10	DEFB $9F,$FD,$B5,$59,$90,$0D,$B5,$59
EB18	DEFB $9F,$FD,$AA,$A9,$80,$01,$FF,$FF
EB20	DEFB $1D,$B8,$22,$F4,$22,$F4,$1D,$B8
EB28	DEFB $03,$C0,$FF,$FF,$D5,$55,$BF,$FF
EB30	DEFB $EA,$AD,$B0,$07,$E5,$4D,$B0,$07
EB38	DEFB $EA,$AD,$BF,$FF,$D5,$55,$FF,$FF
EB40	DEFB $07,$E0,$08,$10,$08,$10,$07,$E0
EB48	DEFB $03,$C0,$FF,$FF,$FF,$FF,$D5,$57
EB50	DEFB $E0,$03,$CA,$A7,$E7,$F3,$CA,$A7
EB58	DEFB $E0,$03,$D5,$57,$FF,$FF,$FF,$FF
EB60	DEFB $1D,$B8,$2F,$44,$2F,$44,$1D,$B8
EB68	DEFB $03,$C0,$FF,$FF,$AA,$AB,$C0,$01
EB70	DEFB $95,$53,$CF,$F9,$9A,$B3,$CF,$F9
EB78	DEFB $95,$53,$C0,$01,$AA,$AB,$FF,$FF

gfx_penrosetriangle0:
			dg	.##############. ; 0
			dg	#.#...........#. ; 1
			dg	#.#.############ ; 2
			dg	#..#.########### ; 3
			dg	.#.#.########### ; 4
			dg	.#..#.#...#..##. ; 5
			dg	..#.#.#...#.###. ; 6
			dg	..#..#.#.#..##.. ; 7
			dg	...#.#.#.#.###.. ; 8
			dg	...#..#.#..##... ; 9
			dg	....#.#.#.###... ; 10
			dg	....#..#..##.... ; 11
			dg	.....#.#.###.... ; 12
			dg	.....#...##..... ; 13
			dg	......#.###..... ; 14
			dg	......####...... ; 15

gfx_penrosetriangle1:
			dg	............###. ; 0
			dg	..........##...# ; 1
			dg	........##...### ; 2
			dg	......##...##..# ; 3
			dg	....##...##..#.# ; 4
			dg	..##...##..###.# ; 5
			dg	##...##..#####.# ; 6
			dg	#..##..##..###.# ; 7
			dg	##...##....###.# ; 8
			dg	####...##..###.# ; 9
			dg	.#####...#####.# ; 10
			dg	...#####...###.# ; 11
			dg	.....#####.###.# ; 12
			dg	.......#######.# ; 13
			dg	.........####### ; 14
			dg	...........###.. ; 15

gfx_penrosetriangle2:
			dg	......####...... ; 0
			dg	.....###.#...... ; 1
			dg	.....##...#..... ; 2
			dg	....###.#.#..... ; 3
			dg	....##..#..#.... ; 4
			dg	...###.#.#.#.... ; 5
			dg	...##..#.#..#... ; 6
			dg	..###.#.#.#.#... ; 7
			dg	..##..#.#.#..#.. ; 8
			dg	.###.#...#.#.#.. ; 9
			dg	.##..#...#.#..#. ; 10
			dg	###########.#.#. ; 11
			dg	###########.#..# ; 12
			dg	############.#.# ; 13
			dg	.#...........#.# ; 14
			dg	.##############. ; 15

gfx_penrosetriangle3:
			dg	..###........... ; 0
			dg	#######......... ; 1
			dg	#.#######....... ; 2
			dg	#.###.#####..... ; 3
			dg	#.###...#####... ; 4
			dg	#.#####...#####. ; 5
			dg	#.###..##...#### ; 6
			dg	#.###....##...## ; 7
			dg	#.###..##..##..# ; 8
			dg	#.#####..##...## ; 9
			dg	#.###..##...##.. ; 10
			dg	#.#..##...##.... ; 11
			dg	#..##...##...... ; 12
			dg	###...##........ ; 13
			dg	#...##.......... ; 14
			dg	.###............ ; 15

	IF 0
	; warehouse machine from Bug Byte version
F300	DEFB $55,$55,$FF,$FF,$FF,$FF,$08,$10
F308	DEFB $08,$10,$08,$10,$F8,$1F,$55,$55
F310	DEFB $FF,$FF,$FF,$FF,$08,$10,$08,$10
F318	DEFB $08,$10,$58,$15,$FF,$FF,$FF,$FF
F320	DEFB $00,$00,$55,$55,$FF,$FF,$FF,$FF
F328	DEFB $08,$10,$F8,$1F,$08,$10,$3F,$FE
F330	DEFB $38,$1E,$08,$10,$5F,$F5,$FF,$FF
F338	DEFB $FF,$FF,$00,$00,$FF,$FF,$00,$00
F340	DEFB $00,$00,$00,$00,$FF,$FF,$55,$55
F348	DEFB $FF,$FF,$FF,$FF,$08,$10,$38,$1E
F350	DEFB $3F,$FE,$08,$10,$F8,$1F,$5F,$F5
F358	DEFB $FF,$FF,$FF,$FF,$00,$00,$00,$00
F360	DEFB $00,$00,$55,$55,$FF,$FF,$F8,$1F
F368	DEFB $08,$10,$55,$55,$FF,$FF,$FF,$FF
F370	DEFB $78,$1D,$F8,$1F,$F8,$1F,$08,$10
F378	DEFB $55,$55,$FF,$FF,$FF,$FF,$00,$00
	ENDIF

; amoeba_b
gfx_amoeba_b0	
			dg	......####...... ;0		03c0
			dg	....###..###.... ;1		0e70
			dg	...#..####..#... ;2		13c8
			dg	..##...##...##.. ;3		318c
			dg	..###..##..###.. ;4		399c
			dg	.#.##########.#. ;5		5ffa
			dg	#...##.##.##..#. ;6		8db2
			dg	#....#..#.#..#.. ;7		84a4
			dg	.#..#..#..#..#.. ;8		4924
			dg	..#.#..#...#..#. ;9		2912
			dg	..#..#..#...#..# ;10	2489
			dg	.#....#..#..#..# ;11	4249
			dg	#.....#..#.#..#. ;12	8252
			dg	.....#..#..#.... ;13	0490
			dg	....#...#...#... ;14	0888
			dg	.........#...... ;15	0040

gfx_amoeba_b1	
			dg	......####...... ;0		03c0
			dg	....###..###.... ;1		0e70
			dg	...#..####..#... ;2		13c8
			dg	..##...##...##.. ;3		318c
			dg	..###..##..###.. ;4		399c
			dg	.#.##########.#. ;5		5ffa
			dg	.#..##.##.##...# ;6		4db1
			dg	#....#.#...#...# ;7		8511
			dg	#....#..#..#..#. ;8		8492
			dg	.#..#...#.#..#.. ;9		48a4
			dg	..#.#..#..#..#.. ;10	2924
			dg	..#.#..#...#..#. ;11	2912
			dg	.#...#..#...#..# ;12	4489
			dg	......#..#..#... ;13	0248
			dg	......#..#.#.... ;14	0250
			dg	.....#.......... ;15	0400

gfx_amoeba_b2	
			dg	......####...... ;0		03c0
			dg	....###..###.... ;1		0e70
			dg	...#..####..#... ;2		13c8
			dg	..##...##...##.. ;3		318c
			dg	..###..##..###.. ;4		399c
			dg	.#.##########.#. ;5		5ffa
			dg	.#..##.##.##...# ;6		4db1
			dg	.#...#..#..#...# ;7		4491
			dg	#.....#..#..#..# ;8		8249
			dg	#.....#..#..#.#. ;9		824a
			dg	.#...#..#..#.#.. ;10	4494
			dg	..#..#.#..#..#.. ;11	2524
			dg	..#.#..#..#...#. ;12	2922
			dg	....#...#..#.... ;13	0890
			dg	.....#...#..#... ;14	0448
			dg	.........#...... ;15	0040

gfx_amoeba_b3	
			dg	......####...... ;0		03c0
			dg	....###..###.... ;1		0e70
			dg	...#..####..#... ;2		13c8
			dg	..##...##...##.. ;3		318c
			dg	..###..##..###.. ;4		399c
			dg	.#.##########.#. ;5		5ffa
			dg	.#..##.##.##..#. ;6		4db2
			dg	..#.#..#...#..#. ;7		2912
			dg	..#..#..#..#...# ;8		2491
			dg	.#....#..#..#..# ;9		4249
			dg	#.....#..#..#.#. ;10	824a
			dg	#....#...#..#.#. ;11	844a
			dg	.#..#...#..#...# ;12	4891
			dg	....#..#..#..... ;13	0920
			dg	....#..#........ ;14	0900
			dg	........#....... ;15	0080

gfx_eye0
FF00	DEFB $00,$00,$00,$00,$00,$00,$03,$C0
FF08	DEFB $0C,$30,$10,$08,$20,$04,$40,$02
FF10	DEFB $80,$01,$40,$02,$20,$04,$D0,$0B
FF18	DEFB $2C,$34,$4B,$D2,$12,$48,$02,$40
FF20	DEFB $00,$00,$00,$00,$00,$00,$03,$C0
FF28	DEFB $0C,$30,$10,$08,$20,$04,$40,$02
FF30	DEFB $F8,$1F,$57,$EA,$2B,$D4,$12,$48
FF38	DEFB $0C,$30,$03,$C0,$00,$00,$00,$00
FF40	DEFB $04,$20,$04,$20,$12,$48,$4B,$D2
FF48	DEFB $2C,$34,$93,$C9,$A7,$E5,$46,$62
FF50	DEFB $86,$61,$47,$E2,$23,$C4,$10,$08
FF58	DEFB $0C,$30,$03,$C0,$00,$00,$00,$00
FF60	DEFB $00,$00,$00,$00,$00,$00,$03,$C0
FF68	DEFB $0C,$30,$12,$48,$2A,$54,$5F,$FA
FF70	DEFB $F6,$7F,$47,$E2,$23,$C4,$10,$08
FF78	DEFB $0C,$30,$03,$C0,$00,$00,$00,$00

gfx_prism0
FB00	DEFB $03,$40,$0F,$70,$3F,$3C,$3F,$4C
FB08	DEFB $5F,$66,$5F,$76,$9F,$7F,$00,$7F
FB10	DEFB $8E,$00,$8E,$FF,$46,$FE,$40,$F2
FB18	DEFB $20,$04,$30,$0C,$0C,$30,$02,$C0
FB20	DEFB $03,$C0,$0F,$F0,$3F,$AC,$3F,$CC
FB28	DEFB $5F,$C6,$47,$B6,$99,$BF,$9E,$7F
FB30	DEFB $8E,$7F,$8D,$9F,$45,$E6,$41,$F2
FB38	DEFB $20,$04,$30,$0C,$0C,$30,$03,$C0
FB40	DEFB $03,$C0,$0F,$F0,$2F,$BC,$37,$C8
FB48	DEFB $5B,$E6,$5D,$E6,$9E,$DF,$9E,$3F
FB50	DEFB $8C,$7F,$8B,$7F,$47,$BE,$41,$C2
FB58	DEFB $00,$04,$30,$04,$0C,$30,$03,$C0
FB60	DEFB $03,$C0,$0B,$F0,$3D,$BC,$3D,$CC
FB68	DEFB $5D,$E6,$5E,$F4,$9E,$E3,$9E,$1F
FB70	DEFB $88,$7F,$87,$7F,$07,$7E,$41,$B2
FB78	DEFB $20,$04,$30,$0C,$0C,$10,$03,$C0

gfx_kong0
	IF 0
CF00	DEFB $13,$C8,$1D,$B8,$0F,$F0,$06,$60
CF08	DEFB $05,$A0,$02,$40,$07,$E0,$0F,$F0
CF10	DEFB $1F,$F8,$33,$CC,$63,$C6,$46,$62
CF18	DEFB $2C,$34,$06,$60,$02,$40,$0E,$70
CF20	DEFB $0B,$D0,$0D,$B0,$0F,$F0,$06,$60
CF28	DEFB $05,$A0,$02,$40,$03,$C0,$1F,$F8
CF30	DEFB $7F,$FE,$E7,$E7,$83,$C1,$C7,$E3
CF38	DEFB $06,$60,$0C,$30,$08,$10,$38,$1C
CF40	DEFB $1C,$38,$06,$60,$0C,$30,$66,$66
CF48	DEFB $23,$C4,$67,$E6,$37,$EC,$1F,$F8
CF50	DEFB $0F,$F0,$07,$E0,$02,$40,$05,$A0
CF58	DEFB $06,$60,$0F,$F0,$0D,$B0,$0B,$D0
CF60	DEFB $70,$0E,$18,$18,$0C,$30,$06,$60
CF68	DEFB $63,$C6,$27,$E4,$67,$E6,$37,$EC
CF70	DEFB $1F,$F8,$0F,$F0,$02,$40,$05,$A0
CF78	DEFB $16,$68,$0F,$F0,$0D,$B0,$03,$C0
	ELSE

			dg	.....#.#.#...... ; 0
			dg	##...##.##...##. ; 1
			dg	.#..#..#..#..#.. ; 2
			dg	..##.##.##.##... ; 3
			dg	...#.#####.#.... ; 4
			dg	..#.........##.. ; 5
			dg	.#.##.###.##.##. ; 6
			dg	##.###...###.### ; 7
			dg	###...#.#...#### ; 8
			dg	###.##...##.#### ; 9
			dg	.####.#.#.#####. ; 10
			dg	..#####.#####... ; 11
			dg	....###.###..#.. ; 12
			dg	.###.......####. ; 13
			dg	#####.......#### ; 14
			dg	####........#### ; 15

			dg	.###.#.#.#...... ; 0
			dg	#####.#.##...##. ; 1
			dg	#.#.#..#..#..#.. ; 2
			dg	.....##.##.##... ; 3
			dg	#.##.#####.#.... ; 4
			dg	##..........##.. ; 5
			dg	.#.##.###.##.##. ; 6
			dg	.#.###...###.### ; 7
			dg	..#.###.#...#### ; 8
			dg	.........##.#### ; 9
			dg	....###.#.#####. ; 10
			dg	.##.###.#####... ; 11
			dg	####.##.###..#.. ; 12
			dg	####.......####. ; 13
			dg	............#### ; 14
			dg	............#### ; 15

			dg	................ ; 0
			dg	.....#.#.#...... ; 1
			dg	##...##.##...##. ; 2
			dg	.#..#..#..#..#.. ; 3
			dg	..##.##.##.##... ; 4
			dg	...#.#####.#.... ; 5
			dg	..#.........##.. ; 6
			dg	.#.##.....##.##. ; 7
			dg	##.##.###.##.### ; 8
			dg	###.##...##.#### ; 9
			dg	###.........#### ; 10
			dg	.##.##...##.###. ; 11
			dg	..###.#.#.####.. ; 12
			dg	.#.####.#####.#. ; 13
			dg	###.###.###..### ; 14
			dg	####........#### ; 15

			dg	......#.#.#.###. ; 0
			dg	.##...##.#.##### ; 1
			dg	..#..#..#..#.#.# ; 2
			dg	...##.##.##..... ; 3
			dg	....#.#####.##.# ; 4
			dg	..##..........## ; 5
			dg	.##.##.###.##.#. ; 6
			dg	###.###...###.#. ; 7
			dg	####...#.###.#.. ; 8
			dg	####.##......... ; 9
			dg	.#####.#.###.... ; 10
			dg	...#####.###.##. ; 11
			dg	..#..###.##.#### ; 12
			dg	.####.......#### ; 13
			dg	####............ ; 14
			dg	####............ ; 15

	ENDIF

gfx_eugene:
	IF 1
			dg	......###....... ; 0
			dg	....######...... ; 1
			dg	..#########..... ; 2
			dg	.######.###.#... ; 3
			dg	.######.###.##.. ; 4
			dg	..####.###.#.#.. ; 5
			dg	.#.##.##..####.. ; 6
			dg	..##....#####... ; 7
			dg	.#....###....#.. ; 8
			dg	..####.#.####... ; 9
			dg	..##.#...##.#... ; 10
			dg	...##..##.##.... ; 11
			dg	.#...#..##...#.. ; 12
			dg	#..######.##..#. ; 13
			dg	##..##...##..##. ; 14
			dg	##...#####...##. ; 15
	ELSE
			dg	......####...... ; 0
			dg	....#######..... ; 1
			dg	..##########.... ; 2
			dg	.#######.###.#.. ; 3
			dg	.#######.###.##. ; 4
			dg	..####..###.#.#. ; 5
			dg	.#.##.###..####. ; 6
			dg	..##.....#####.. ; 7
			dg	.#....####....#. ; 8
			dg	..####.##.####.. ; 9
			dg	..##.#....##.#.. ; 10
			dg	...##..###.##... ; 11
			dg	.#...#...##...#. ; 12
			dg	#..#######.##..# ; 13
			dg	##..##....##..## ; 14
			dg	##...######...## ; 15
	ENDIF

gfx_boot
			dg	..#.#.#.##...... ;0 2ac0
			dg	..##.#.#.#...... ;1 3540
			dg	..########...... ;2 3fc0
			dg	....#..#........ ;3 0900
			dg	....#..#........ ;4 0900
			dg	...######....... ;5 1f80
			dg	...#....#....... ;6 1080
			dg	...#....#....... ;7 1080
			dg	...#...##....... ;8 1180
			dg	..#...#..#...... ;9 2240
			dg	..#.....#.###... ;a 20b8
			dg	.#.##..#..#..#.. ;b 5924
			dg	.#...#...#....#. ;c 4442
			dg	.#...#........#. ;d 4402
			dg	.#...#........#. ;e 4402
			dg	################ ;f ffff

	ALIGN 256
; exit portals
gfx_exit0	; Central Cavern exit
			dg	################ ;0 ffff
			dg	#..#..#..#..#..# ;1 9249
			dg	#.##.##.##.##.## ;2 b6db
			dg	################ ;3 ffff
			dg	#..#..#..#..#..# ;4 9249
			dg	#.##.##.##.##.## ;5 b6db
			dg	################ ;6 ffff
			dg	#..#..#..#..#..# ;7 9249
			dg	#.##.##.##.##.## ;8 b6db
			dg	################ ;9 ffff
			dg	#..#..#..#..#..# ;a 9249
			dg	#.##.##.##.##.## ;b b6db
			dg	################ ;c ffff
			dg	#..#..#..#..#..# ;d 9249
			dg	#.##.##.##.##.## ;e b6db
			dg	################ ;f ffff

gfx_exit1	; Cold Room exit
			dg	################ ;0 ffff
			dg	#..#..#..#..#..# ;1 9249
			dg	#..#..#..#..#..# ;2 9249
			dg	#..#..#..#..#..# ;3 9249
			dg	#..#..#..#..#..# ;4 9249
			dg	#..#..#..#..#..# ;5 9249
			dg	#..#..#..#..#..# ;6 9249
			dg	#..#..#..#..#..# ;7 9249
			dg	#..#..#..#..#..# ;8 9249
			dg	#..#..#..#..#..# ;9 9249
			dg	#..#..#..#..#..# ;a 9249
			dg	#..#..#..#..#..# ;b 9249
			dg	#..#..#..#..#..# ;c 9249
			dg	#..#..#..#..#..# ;d 9249
			dg	#..#..#..#..#..# ;e 9249
			dg	################ ;f ffff

gfx_exit2	
			dg	################ ;0 ffff
			dg	.#...#...#...#.. ;1 4444
			dg	#..##..##..##..# ;2 9999
			dg	..#...#...#...#. ;3 2222
			dg	..#...#...#...#. ;4 2222
			dg	#..##..##..##..# ;5 9999
			dg	.#...#...#...#.. ;6 4444
			dg	.#...#...#...#.. ;7 4444
			dg	#..##..##..##..# ;8 9999
			dg	..#...#...#...#. ;9 2222
			dg	..#...#...#...#. ;a 2222
			dg	#..##..##..##..# ;b 9999
			dg	.#...#...#...#.. ;c 4444
			dg	.#...#...#...#.. ;d 4444
			dg	#..##..##..##..# ;e 9999
			dg	################ ;f ffff

gfx_exit3	
			dg	..#...#...#...#. ;0 2222
			dg	...#...#...#...# ;1 1111
			dg	#...#...#...#... ;2 8888
			dg	.#...#...#...#.. ;3 4444
			dg	..#...#...#...#. ;4 2222
			dg	...#...#...#...# ;5 1111
			dg	#...#...#...#... ;6 8888
			dg	.#...#...#...#.. ;7 4444
			dg	..#...#...#...#. ;8 2222
			dg	...#...#...#...# ;9 1111
			dg	#...#...#...#... ;a 8888
			dg	.#...#...#...#.. ;b 4444
			dg	..#...#...#...#. ;c 2222
			dg	...#...#...#...# ;d 1111
			dg	#...#...#...#... ;e 8888
			dg	.#...#...#...#.. ;f 4444

gfx_exit4	
			dg	################ ;0 ffff
			dg	#.#.#.#.#.#.#.#. ;1 aaaa
			dg	#.#.#.#.#.#.#.#. ;2 aaaa
			dg	#.#.#.#.#.#.#.#. ;3 aaaa
			dg	#.#.#.#.#.#.#.#. ;4 aaaa
			dg	#.#.#.#.#.#.#.#. ;5 aaaa
			dg	#.#.#.#.#.#.#.#. ;6 aaaa
			dg	#.#.#.#.#.#.#.#. ;7 aaaa
			dg	#.#.#.#.#.#.#.#. ;8 aaaa
			dg	#.#.#.#.#.#.#.#. ;9 aaaa
			dg	#.#.#.#.#.#.#.#. ;a aaaa
			dg	#.#.#.#.#.#.#.#. ;b aaaa
			dg	#.#.#.#.#.#.#.#. ;c aaaa
			dg	#.#.#.#.#.#.#.#. ;d aaaa
			dg	#.#.#.#.#.#.#.#. ;e aaaa
			dg	################ ;f ffff

gfx_exit5	
			dg	################ ;0 ffff
			dg	#......##......# ;1 8181
			dg	#.############.# ;2 bffd
			dg	#.############.# ;3 bffd
			dg	#.##........##.# ;4 b00d
			dg	#.##........##.# ;5 b00d
			dg	#.##........##.# ;6 b00d
			dg	####........#### ;7 f00f
			dg	####........#### ;8 f00f
			dg	#.##........##.# ;9 b00d
			dg	#.##........##.# ;a b00d
			dg	#.##........##.# ;b b00d
			dg	#.############.# ;c bffd
			dg	#.############.# ;d bffd
			dg	#......##......# ;e 8181
			dg	################ ;f ffff


gfx_exit6	
			dg	################ ;0 ffff
			dg	#......##......# ;1 8181
			dg	#......##......# ;2 8181
			dg	#......##......# ;3 8181
			dg	#......##......# ;4 8181
			dg	#......##......# ;5 8181
			dg	#......##......# ;6 8181
			dg	################ ;7 ffff
			dg	################ ;8 ffff
			dg	#......##......# ;9 8181
			dg	#......##......# ;a 8181
			dg	#......##......# ;b 8181
			dg	#......##......# ;c 8181
			dg	#......##......# ;d 8181
			dg	#......##......# ;e 8181
			dg	################ ;f ffff

gfx_exit7	
			dg	################ ;0 ffff
			dg	#..............# ;1 8001
			dg	##............## ;2 c003
			dg	#.#..........#.# ;3 a005
			dg	#..#........#..# ;4 9009
			dg	##..#......#..## ;5 c813
			dg	#.#..#....#..#.# ;6 a425
			dg	#..#..#..#..#..# ;7 9249
			dg	##..#..##..#..## ;8 c993
			dg	#.#..#....#..#.# ;9 a425
			dg	#..#..#..#..#..# ;a 9249
			dg	##..#..##..#..## ;b c993
			dg	#.#..#....#..#.# ;c a425
			dg	##..#..##..#..## ;d c993
			dg	#..#..#..#..#..# ;e 9249
			dg	################ ;f ffff

gfx_exit8	
			dg	################ ;0 ffff
			dg	#..............# ;1 8001
			dg	#......##......# ;2 8181
			dg	#.....#..#.....# ;3 8241
			dg	#....#....#....# ;4 8421
			dg	#...#......#...# ;5 8811
			dg	#..#........#..# ;6 9009
			dg	#.#....##....#.# ;7 a185
			dg	#.#....##....#.# ;8 a185
			dg	#..#........#..# ;9 9009
			dg	#...#......#...# ;a 8811
			dg	#....#....#....# ;b 8421
			dg	#.....#..#.....# ;c 8241
			dg	#......##......# ;d 8181
			dg	#..............# ;e 8001
			dg	################ ;f ffff
			

gfx_exit9	
			dg	################ ;0 ffff
			dg	#####...#...#### ;1 f88f
			dg	#...#...#..#...# ;2 8891
			dg	#.#.#.#.#..#...# ;3 aa91
			dg	#.#.#.#.#..#.#.# ;4 aa95
			dg	#...#.#.#....#.# ;5 8a85
			dg	#..#....#..#...# ;6 9091
			dg	##.#.#.##.###..# ;7 d5b9
			dg	##.#.#.#.#.#.#.# ;8 d555
			dg	##.#...#.#...#.# ;9 d145
			dg	#...#..#..###..# ;a 8939
			dg	#...#..#......## ;b 8903
			dg	#.#.#...#.#.#.## ;c a8ab
			dg	#.#.#.#.#.#.#.## ;d aaab
			dg	#...#.#.#...#..# ;e 8a89
			dg	################ ;f ffff

gfx_exit10	
			dg	################ ;0 ffff
			dg	##.##.#.#.#.#.## ;1 daab
			dg	###.#.#..##.#.## ;2 ea6b
			dg	################ ;3 ffff
			dg	#..#........#..# ;4 9009
			dg	#..#........#..# ;5 9009
			dg	################ ;6 ffff
			dg	#..#........#..# ;7 9009
			dg	#..#........#..# ;8 9009
			dg	################ ;9 ffff
			dg	#..#........#..# ;a 9009
			dg	#..#........#..# ;b 9009
			dg	################ ;c ffff
			dg	#..#........#..# ;d 9009
			dg	#..#........#..# ;e 9009
			dg	################ ;f ffff

gfx_exit11	
			dg	################ ;0 ffff
			dg	#..............# ;1 8001
			dg	#...########...# ;2 8ff1
			dg	#...########...# ;3 8ff1
			dg	#...########...# ;4 8ff1
			dg	#...########...# ;5 8ff1
			dg	#...########...# ;6 8ff1
			dg	#...##....##...# ;7 8c31
			dg	#...##....##...# ;8 8c31
			dg	#...########...# ;9 8ff1
			dg	#...########...# ;a 8ff1
			dg	#...########...# ;b 8ff1
			dg	#...########...# ;c 8ff1
			dg	#...########...# ;d 8ff1
			dg	#..............# ;e 8001
			dg	################ ;f ffff

gfx_exit12	
			dg	......####...... ;0 03c0
			dg	.....######..... ;1 07e0
			dg	....########.... ;2 0ff0
			dg	....#..##..#.... ;3 0990
			dg	....#..##..#.... ;4 0990
			dg	.....######..... ;5 07e0
			dg	.....#.##.#..... ;6 05a0
			dg	......#..#...... ;7 0240
			dg	.##....##....##. ;8 6186
			dg	#####......##### ;9 f81f
			dg	#######..####### ;a fe7f
			dg	.....#.####..... ;b 05e0
			dg	.....####.#..... ;c 07a0
			dg	#######..####### ;d fe7f
			dg	#####......##### ;e f81f
			dg	.##..........##. ;f 6006

gfx_exit13	
			dg	################ ;0 ffff
			dg	################ ;1 ffff
			dg	######....###### ;2 fc3f
			dg	#####......##### ;3 f81f
			dg	####........#### ;4 f00f
			dg	###..........### ;5 e007
			dg	##.....##.....## ;6 c183
			dg	##....#..#....## ;7 c243
			dg	##....#..#....## ;8 c243
			dg	##.....##.....## ;9 c183
			dg	###..........### ;a e007
			dg	####........#### ;b f00f
			dg	#####......##### ;c f81f
			dg	######....###### ;d fc3f
			dg	################ ;e ffff
			dg	################ ;f ffff


gfx_exit14	
			dg	################ ;0 ffff
			dg	#..............# ;1 8001
			dg	#..............# ;2 8001
			dg	#..............# ;3 8001
			dg	#..............# ;4 8001
			dg	#...#..........# ;5 8801
			dg	#.#.#.#........# ;6 aa01
			dg	#..###....####.# ;7 9c3d
			dg	########.#...### ;8 ff47
			dg	#..###.........# ;9 9c01
			dg	#.#.#.#........# ;a aa01
			dg	#...#..........# ;b 8801
			dg	#..............# ;c 8001
			dg	#..............# ;d 8001
			dg	#..............# ;e 8001
			dg	################ ;f ffff

gfx_exit15	
			dg	################ ;0 ffff
			dg	#......##......# ;1 8181
			dg	#......##......# ;2 8181
			dg	################ ;3 ffff
			dg	#......##......# ;4 8181
			dg	#......##......# ;5 8181
			dg	################ ;6 ffff
			dg	#......##......# ;7 8181
			dg	#......##......# ;8 8181
			dg	################ ;9 ffff
			dg	#......##......# ;a 8181
			dg	#......##......# ;b 8181
			dg	################ ;c ffff
			dg	#......##......# ;d 8181
			dg	#......##......# ;e 8181
			dg	################ ;f ffff

gfx_exit16	
			dg	################ ;0 ffff
			dg	#..............# ;1 8001
			dg	#.############.# ;2 bffd
			dg	#.#..........#.# ;3 a005
			dg	#.#..#.##.#..#.# ;4 a5a5
			dg	#.#..#.##.#..#.# ;5 a5a5
			dg	#.#..#.##.#..#.# ;6 a5a5
			dg	#.#..#.##.#..#.# ;7 a5a5
			dg	#.#..#.##.#..#.# ;8 a5a5
			dg	#.#..#.##.#..#.# ;9 a5a5
			dg	#.#.########.#.# ;a aff5
			dg	#.#..#.##.#..#.# ;b a5a5
			dg	#.#..#.##.#..#.# ;c a5a5
			dg	#.#..#.##.#..#.# ;d a5a5
			dg	#.#..#.##.#..#.# ;e a5a5
			dg	################ ;f ffff

gfx_exit17	
			dg	################ ;0 ffff
			dg	#..............# ;1 8001
			dg	#.##........##.# ;2 b00d
			dg	#.#..........#.# ;3 a005
			dg	#.#.#.#..#.#.#.# ;4 aa55
			dg	#.#.#.#..#.#.#.# ;5 aa55
			dg	#.#.#.#..#.#.#.# ;6 aa55
			dg	#.#.#.#..#.#.#.# ;7 aa55
			dg	#.#.#.#..#.#.#.# ;8 aa55
			dg	#.#.#.#..#.#.#.# ;9 aa55
			dg	#.#.#.#..#.#.#.# ;a aa55
			dg	#.#.#.#..#.#.#.# ;b aa55
			dg	#.#..........#.# ;c a005
			dg	#.##........##.# ;d b00d
			dg	#..............# ;e 8001
			dg	################ ;f ffff
			

gfx_exit18	
			dg	################ ;0 ffff
			dg	#..............# ;1 8001
			dg	#.############.# ;2 bffd
			dg	#.#..........#.# ;3 a005
			dg	#.#.########.#.# ;4 aff5
			dg	#.#.#......#.#.# ;5 a815
			dg	#.#.#.####.#.#.# ;6 abd5
			dg	#.#.#.#..#.#.#.# ;7 aa55
			dg	#.#.#.#..#.#.#.# ;8 aa55
			dg	#.#.#.####.#.#.# ;9 abd5
			dg	#.#.#......#.#.# ;a a815
			dg	#.#.########.#.# ;b aff5
			dg	#.#..........#.# ;c a005
			dg	#.############.# ;d bffd
			dg	#..............# ;e 8001
			dg	################ ;f ffff

gfx_exit19	
			dg	................ ;0 0000
			dg	.....######..... ;1 07e0
			dg	...##......##... ;2 1818
			dg	..#...####...#.. ;3 23c4
			dg	.#...#....#...#. ;4 4422
			dg	.#..#......#..#. ;5 4812
			dg	.#..#......#..#. ;6 4812
			dg	.#..#......#..#. ;7 4812
			dg	.#...#....#...#. ;8 4422
			dg	..#...#..#...#.. ;9 2244
			dg	...##.#..#.##... ;a 1a58
			dg	.#..#.#..#.#..#. ;b 4a52
			dg	.####.#..#.####. ;c 7a5e
			dg	.#....#..#....#. ;d 4242
			dg	.######..######. ;e 7e7e
			dg	................ ;f 0000

gfx_plinth
			dg	################ ;0 ffff
			dg	.###..#..#..###. ;1 724e
			dg	#...#.#..#.#...# ;2 8a51
			dg	#.#.#.#..#.#.#.# ;3 aa55
			dg	.#..#.#..#.#..#. ;4 4a52
			dg	...#..#..#..#... ;5 1248
			dg	..#...#..#...#.. ;6 2244
			dg	..#.#.#..#.#.#.. ;7 2a54
			dg	..#.#.#..#.#.#.. ;8 2a54
			dg	..#.#.#..#.#.#.. ;9 2a54
			dg	..#.#.#..#.#.#.. ;a 2a54
			dg	..#.#.#..#.#.#.. ;b 2a54
			dg	..#.#.#..#.#.#.. ;c 2a54
			dg	..#.#.#..#.#.#.. ;d 2a54
			dg	..#.#.#..#.#.#.. ;e 2a54
			dg	..#.#.#..#.#.#.. ;f 2a54


	ALIGN 256
gfx_8x8page:
gfx_platform0	; Spectrum platform
			dg	######## ; ff
			dg	######## ; ff
			dg	##.##.## ; db
			dg	###.###. ; ee
			dg	##...#.# ; c5
			dg	.#...... ; 40
			dg	........ ; 00
			dg	........ ; 00

			IF 0
gfx_platform0L	; Spectrum platform
			dg	######## ; ff
			dg	.####### ; ff
			dg	...##.## ; db
			dg	....###. ; ee
			dg	.....#.# ; c5
			dg	........ ; 40
			dg	........ ; 00
			dg	........ ; 00

gfx_platform0R	; Spectrum platform
			dg	######## ; ff
			dg	#######. ; ff
			dg	##.##... ; db
			dg	###.#... ; ee
			dg	##...... ; c5
			dg	.#...... ; 40
			dg	........ ; 00
			dg	........ ; 00
			ENDIF

gfx_platform2	; Menagerie platform
			dg	######## ; ff
			dg	######## ; ff
			dg	.##..##. ; 66
			dg	#..##..# ; 99
			dg	.##..##. ; 66
			dg	#..##..# ; 99
			dg	######## ; ff
			dg	........ ; 00

gfx_platform3	; string
			dg	...#.... ; 10
			dg	...#.... ; 10
			dg	...#.... ; 10
			dg	...#.... ; 10
			dg	...#.... ; 10
			dg	...#.... ; 10
			dg	...#.... ; 10
			dg	...#.... ; 10

gfx_platform4	; Processing Plant platform
			dg	######## ; ff
			dg	######## ; ff
			dg	#..##..# ; 99
			dg	#..##..# ; 99
			dg	######## ; ff
			dg	#..##..# ; 99
			dg	.##..##. ; 66
			dg	........ ; 00

gfx_platform5	; leaf
			dg	.#####.. ; 7c
			dg	######## ; ff
			dg	###.#### ; ef
			dg	...####. ; 1e
			dg	....##.. ; 0c
			dg	....#... ; 08
			dg	....#... ; 08
			dg	....#... ; 08

gfx_platform6	; endorian platform
			dg	######## ; ff
			dg	######## ; ff
			dg	##..#.#. ; ca
			dg	.##..#.# ; 65
			dg	#..#..#. ; 92
			dg	..#.#... ; 28
			dg	#.....#. ; 82
			dg	........ ; 00

gfx_platform7	; telephones platform
			dg	######## ; ff
			dg	######## ; ff
			dg	######## ; ff
			dg	######## ; ff
			dg	######## ; ff
			dg	#.#.#.#. ; aa
			dg	........ ; 00
			dg	........ ; 00

gfx_platform8	; ore platform a
			dg	######## ; ff
			dg	######## ; ff
			dg	...#...# ; 11
			dg	..#...#. ; 22
			dg	.#...#.. ; 44
			dg	#...#... ; 88
			dg	######## ; ff
			dg	######## ; ff

gfx_platform9	; ore ladder
			dg	######## ; ff
			dg	#......# ; 81
			dg	#......# ; 81
			dg	#......# ; 81
			dg	#......# ; 81
			dg	#......# ; 81
			dg	#......# ; 81
			dg	#......# ; 81

gfx_platform10	; skylab platform l
			dg	######## ; ff
			dg	######## ; ff
			dg	.##...#. ; 62
			dg	.##..#.. ; 64
			dg	.####... ; 78
			dg	.###.... ; 70
			dg	.##..... ; 60
			dg	.##..... ; 60

gfx_platform11	; skylab platform r
			dg	######## ; ff
			dg	######## ; ff
			dg	.#...##. ; 46
			dg	..#..##. ; 26
			dg	...####. ; 1e
			dg	....###. ; 0e
			dg	.....##. ; 06
			dg	.....##. ; 06

gfx_platform12	;db #ff, #ff, #f7, #dd, #aa, #55, #22, #00	; bank platform
			dg	######## ; ff
			dg	######## ; ff
			dg	##.###.# ; dd this was wrong in 2010 version
			dg	.###.### ; 77 so was this!
			dg	#.#.#.#. ; aa
			dg	.#.#.#.# ; 55
			dg	..#...#. ; 22
			dg	........ ; 00

gfx_platform13	;db #ff, #ff, #18, #18, #18, #18, #18, #18	; bank ladder
			dg	######## ; ff
			dg	######## ; ff
			dg	...##... ; 18
			dg	...##... ; 18
			dg	...##... ; 18
			dg	...##... ; 18
			dg	...##... ; 18
			dg	...##... ; 18

gfx_wall0	; Spectrum wall
			dg	..#...#. ; 22
			dg	######## ; ff
			dg	#...#... ; 88
			dg	######## ; ff
			dg	..#...#. ; 22
			dg	######## ; ff
			dg	#...#... ; 88
			dg	######## ; ff

gfx_wall2	; girder
			dg	#......# ; 81
			dg	##....## ; c3
			dg	#.#..#.# ; a5
			dg	#..##..# ; 99
			dg	#..##..# ; 99
			dg	#.#..#.# ; a5
			dg	##....## ; c3
			dg	#......# ; 81

gfx_wall3	; Processing Plant wall
			dg	######## ; ff
			dg	#..##..# ; 99
			dg	######## ; ff
			dg	.##..##. ; 66
			dg	######## ; ff
			dg	#..##..# ; 99
			dg	######## ; ff
			dg	.##..##. ; 66

gfx_wall4	; Amoebas1 wall
			dg	.#.##.#. ; 5a
			dg	.#.##.#. ; 5a
			dg	.#.##.#. ; 5a
			dg	.#.##.#. ; 5a
			dg	.#.##.#. ; 5a
			dg	.#.##.#. ; 5a
			dg	.#.##.#. ; 5a
			dg	.#.##.#. ; 5a

gfx_wall5	; tree
			dg	.#..#.#. ; 4a
			dg	.#..#.#. ; 4a
			dg	.#..#.#. ; 4a
			dg	.#.#..#. ; 52
			dg	.#.#.#.. ; 54
			dg	.#..#.#. ; 4a
			dg	..#.#.#. ; 2a
			dg	..#.#.#. ; 2a

gfx_wall6	; telephones/bank wall
			dg	#.#.#.#. ; aa
			dg	.#.#.#.# ; 55
			dg	#.#.#.#. ; aa
			dg	.#.#.#.# ; 55
			dg	#.#.#.#. ; aa
			dg	.#.#.#.# ; 55
			dg	#.#.#.#. ; aa
			dg	.#.#.#.# ; 55

gfx_wall7	; skylab wall
			dg	.......# ; 01
			dg	#.....#. ; 82
			dg	##...#.. ; c4
			dg	###.#... ; e8
			dg	###..... ; e0
			dg	##.##... ; d8
			dg	#.####.. ; bc
			dg	.######. ; 7e

gfx_wall8	;db #49, #f9, #4f, #49, #ff, #48, #78, #cf	; 16th cavern wall
			dg	.#..#..# ; 49
			dg	#####..# ; f9
			dg	.#..#### ; 4f
			dg	.#..#..# ; 49
			dg	######## ; ff
			dg	.#..#... ; 48
			dg	.####... ; 78
			dg	##..#### ; cf

gfx_wall9	;db #ff, #99, #bb, #ff, #ff, #99, #bb, #ff	; warehouse wall
			dg	######## ; ff
			dg	#..##..# ; 99
			dg	#.###.## ; bb
			dg	######## ; ff
			dg	######## ; ff
			dg	#..##..# ; 99
			dg	#.###.## ; bb
			dg	######## ; ff

gfx_wall10	; Amoebas2 wall
			dg	######## ; ff
			dg	#......# ; 81
			dg	#......# ; 81
			dg	######## ; ff
			dg	######## ; ff
			dg	#......# ; 81
			dg	#......# ; 81
			dg	######## ; ff

gfx_spiky0	; Spectrum bush
			dg	.#...#.. ; 44
			dg	..#.#... ; 28
			dg	#..#.#.. ; 94
			dg	.#.#...# ; 51
			dg	..##.#.# ; 35
			dg	##.#.##. ; d6
			dg	.#.##... ; 58
			dg	...#.... ; 10

gfx_spiky1	; Spectrum Stalactite
			dg	######## ; ff
			dg	#######. ; fe
			dg	.######. ; 7e
			dg	.#####.. ; 7c
			dg	.#..##.. ; 4c
			dg	.#..##.. ; 4c
			dg	....##.. ; 0c
			dg	....#... ; 08

gfx_spiky5	; spider
			dg	...#.... ; 10
			dg	##.#.##. ; d6
			dg	..###... ; 38
			dg	##.#.##. ; d6
			dg	..###... ; 38
			dg	.#...#.. ; 44
			dg	##...##. ; c6
			dg	..#.#... ; 28

gfx_spiky6	; mace
			dg	...#.... ; 10
			dg	...#.... ; 10
			dg	...#.... ; 10
			dg	.#.#.#.. ; 54
			dg	..###... ; 38
			dg	##.#.##. ; d6
			dg	..###... ; 38
			dg	.#.#.#.. ; 54

gfx_spiky7	; Eugene/Kong stalactite
			dg	.######. ; 7e
			dg	..####.. ; 3c
			dg	...###.. ; 1c
			dg	...##... ; 18
			dg	...##... ; 18
			dg	....#... ; 08
			dg	....#... ; 08
			dg	....#... ; 08

gfx_spiky8	; Processing Plant spiky
			dg	..####.. ; 3c
			dg	...##... ; 18
			dg	#.####.# ; bd
			dg	###..### ; e7
			dg	###..### ; e7
			dg	#.####.# ; bd
			dg	...##... ; 18
			dg	..####.. ; 3c

gfx_spiky9	; lobster
			dg	#.#..#.# ; a5
			dg	.#....#. ; 42
			dg	..####.. ; 3c
			dg	##.##.## ; db
			dg	..####.. ; 3c
			dg	.######. ; 7e
			dg	#.#..#.# ; a5
			dg	..#..#.. ; 24

gfx_spiky10	; prickly plant
			dg	.#..#... ; 48
			dg	#.##..#. ; b2
			dg	.#.###.# ; 5d
			dg	...#..#. ; 12
			dg	.###.... ; 70
			dg	#.#.###. ; ae
			dg	#.#.#..# ; a9
			dg	.#...### ; 47

gfx_spiky11	; spider
			dg	...#.... ; 10
			dg	...#.... ; 10
			dg	##.#.##. ; d6
			dg	..###... ; 38
			dg	##.#.##. ; d6
			dg	..###... ; 38
			dg	.#.#.#.. ; 54
			dg	#..#..#. ; 92

gfx_spiky12	; 16th cavern spikes
			dg	.#...#.. ; 44
			dg	.#...#.. ; 44
			dg	.#...#.. ; 44
			dg	.#...#.. ; 44
			dg	.##..##. ; 66
			dg	###.###. ; ee
			dg	###.###. ; ee
			dg	######## ; ff

gfx_spiky13	;db #bd, #28, #01, #9a, #59, #82, #11, #28	; warehouse spiky
			dg	#.####.# ; bd
			dg	..#.#... ; 28
			dg	.......# ; 01
			dg	#..##.#. ; 9a
			dg	.#.##..# ; 59
			dg	#.....#. ; 82
			dg	...#...# ; 11
			dg	..#.#... ; 28

gfx_crumbly0	; Spectrum crumbly
			dg	######## ; ff
			dg	##.##.## ; db
			dg	#.#..#.# ; a5
			dg	..#..#.. ; 24
			dg	.#.#..#. ; 52
			dg	..#..... ; 20
			dg	....#... ; 80
			dg	........ ; 00

gfx_crumbly2	; Menagerie crumbly
			dg	######## ; ff
			dg	######## ; ff
			dg	.##..##. ; 66
			dg	#..##..# ; 99
			dg	.#....#. ; 42
			dg	...##... ; 18
			dg	###.#.#. ; ea
			dg	........ ; 00

gfx_crumbly3	; Vat/Warehouse crumbly
			dg	######## ; ff
			dg	#.#.#.#. ; aa
			dg	.#.#.#.# ; 55
			dg	#.#.#.#. ; aa
			dg	.#.#.#.# ; 55
			dg	#.#.#.#. ; aa
			dg	.#.#.#.# ; 55
			dg	#.#.#.#. ; aa

gfx_crumbly4	; crumbly leaf
			dg	######.. ; fc
			dg	######## ; ff
			dg	#....### ; 87
			dg	....##.. ; 0c
			dg	....#... ; 08
			dg	....#... ; 08
			dg	....#... ; 08
			dg	........ ; 00

gfx_conveyor0	; Spectrum conveyor
			dg	####.... ; f0
			dg	.##..##. ; 66
			dg	####.... ; f0
			dg	.##..##. ; 66
			dg	........ ; 00
			dg	#..##..# ; 99
			dg	######## ; ff
			dg	........ ; 00

gfx_conveyor2	; Menagerie conveyor
			dg	####.... ; f0
			dg	#.#.#.#. ; aa
			dg	####.... ; f0
			dg	.##..##. ; 66
			dg	.##..##. ; 66
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00

gfx_conveyor3	; Eugene conveyor
			dg	######.. ; fc
			dg	.##..##. ; 66
			dg	######.. ; fc
			dg	.##..##. ; 66
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00

gfx_conveyor4	; Vat conveyor
			dg	####.#.. ; f4
			dg	.##..##. ; 66
			dg	####.#.. ; f4
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00

gfx_conveyor5	; Kong conveyor
			dg	####.... ; f0
			dg	#.#.#.#. ; aa
			dg	####.... ; f0
			dg	#.#.#.#. ; aa
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00

gfx_conveyor6	; Amoebas1 conveyor
			dg	####.... ; f0
			dg	.##..##. ; 66
			dg	####.... ; f0
			dg	.##..##. ; 66
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00

gfx_conveyor7	; Telephones conveyor
			dg	#######. ; fe
			dg	.##..##. ; 66
			dg	#######. ; fe
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00

gfx_switch0	; switch (off)
			dg	######## ; ff
			dg	#......# ; 81
			dg	#......# ; 81
			dg	.#....#. ; 42
			dg	..####.. ; 3c
			dg	...#.... ; 10
			dg	.##..... ; 60
			dg	.##..... ; 60

gfx_switch1	; switch (on)
			dg	######## ; ff
			dg	#......# ; 81
			dg	#......# ; 81
			dg	.#....#. ; 42
			dg	..####.. ; 3c
			dg	....#... ; 08
			dg	.....##. ; 06
			dg	.....##. ; 06

gfx_pianobot	; bottom of piano key
			dg	.......# ; 01
			dg	.......# ; 01
			dg	.......# ; 01
			dg	.......# ; 01
			dg	.......# ; 01
			dg	.......# ; 01
			dg	.......# ; 01
			dg	.......# ; 01

gfx_pianosharp	; sharp key to the right
			dg	.....### ; 01
			dg	.....### ; 01
			dg	.....### ; 01
			dg	.....### ; 01
			dg	.....### ; 01
			dg	.....### ; 01
			dg	.....### ; 01
			dg	.....### ; 01

gfx_pianosharpandflat	;  sharp and flat notes
			dg	##...### ; c7
			dg	##...### ; c7
			dg	##...### ; c7
			dg	##...### ; c7
			dg	##...### ; c7
			dg	##...### ; c7
			dg	##...### ; c7
			dg	##...### ; c7

gfx_pianoflat	;  flat note to the left
			dg	##.....# ; c1
			dg	##.....# ; c1
			dg	##.....# ; c1
			dg	##.....# ; c1
			dg	##.....# ; c1
			dg	##.....# ; c1
			dg	##.....# ; c1
			dg	##.....# ; c1

gfx_blank
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00
			dg	........ ; 00

			DISPLAY "Gap between tile page and key page: ", /A, gfx_keypage-$

	ALIGN 256
gfx_keypage:
gfx_key0	; Spectrum key sprite
			dg	..##.... ; 30
			dg	.#..#... ; 48
			dg	#...#... ; 88
			dg	#..#.... ; 90
			dg	.##.#... ; 68
			dg	.....#.. ; 04
			dg	....#.#. ; 0a
			dg	.....#.. ; 04

gfx_key2	; snow shoes
			dg	.#.#.... ; 50
			dg	#.#.#... ; a8
			dg	.#.#.#.. ; 54
			dg	#.#.#... ; a8
			dg	.#.#.#.. ; 54
			dg	..#.##.. ; 2c
			dg	......#. ; 02
			dg	.......# ; 01

gfx_key3	; credit cards
			dg	...##### ; 1f
			dg	..#....# ; 21
			dg	.#....## ; 43
			dg	######## ; ff
			dg	#...#### ; 8f
			dg	#...###. ; 8e
			dg	#...##.. ; 8c
			dg	#####... ; f8

gfx_key4	; banana
			dg	#....... ; 80
			dg	##...... ; c0
			dg	###.##.. ; ec
			dg	.###..#. ; 72
			dg	..#.#... ; 28
			dg	.#.#.#.. ; 54
			dg	#...#.#. ; 8a
			dg	#....### ; 87

gfx_key5	; apple
			dg	....#... ; 08
			dg	....#... ; 08
			dg	..#####. ; 3e
			dg	.#.##### ; 5f
			dg	.#.##### ; 5f
			dg	.#...### ; 47
			dg	.##....# ; 61
			dg	..#####. ; 3e

gfx_key6	; 10p
			dg	..####.. ; 3c
			dg	.#.##.#. ; 5a
			dg	#..#.#.# ; 95
			dg	##.#.#.# ; d5
			dg	##.#.#.# ; d5
			dg	##.#.#.# ; d5
			dg	.#.##.#. ; 5a
			dg	..####.. ; 3c

gfx_key7	; ore
			dg	...##... ; 18
			dg	.##.###. ; 6e
			dg	.#....#. ; 42
			dg	##.##.## ; db
			dg	##..#..# ; c9
			dg	.##...#. ; 62
			dg	.######. ; 7e
			dg	...##... ; 18

gfx_key8	; silicon chip
			dg	#.#.#.#. ; aa
			dg	#.#.#.#. ; aa
			dg	#######. ; fe
			dg	#######. ; fe
			dg	#######. ; fe
			dg	#######. ; fe
			dg	#.#.#.#. ; aa
			dg	#.#.#.#. ; aa

gfx_key9	; money bag

			dg	.#####.. ; 7c
			dg	..###... ; 38
			dg	.##..#.. ; 64
			dg	##.####. ; de
			dg	#...###. ; 8e
			dg	##.####. ; de
			dg	#.....#. ; 82
			dg	.#####.. ; 7c

gfx_key10	;db #0f, #09, #3d, #27, #f4, #9c, #90, #f0	; squares
			dg	....#### ; 0f
			dg	....#..# ; 09
			dg	..####.# ; 3d
			dg	..#..### ; 27
			dg	####.#.. ; f4
			dg	#..###.. ; 9c
			dg	#..#.... ; 90
			dg	####.... ; f0

	STRUCT GUARDIAN_DATA
gfx			dw 0
frame		db 0
scraddr		dw 0
;scraddrrow2	dw 0
attrib		db 0
flags		db 0
minaddr		db 0
maxaddr		db 0
vgy			db 0 ; vertical guardian current y
yvel		db 0 ; vertical guardian y velocity
	ENDS

	ALIGN 256
guardian0:
	GUARDIAN_DATA
guardian1:
	GUARDIAN_DATA
guardian2:
	GUARDIAN_DATA
guardian3:
	GUARDIAN_DATA
guardian4:
	GUARDIAN_DATA
guardian5:
	GUARDIAN_DATA
guardian6:
	GUARDIAN_DATA
guardian7:
	GUARDIAN_DATA
	db 0 ; terminator

	ASSERT $-guardian0 <= 256

tokenise_buff BLOCK 64 

; format of this list
; WORD gfx (big endian)
; WORD scraddress
redrawcellslist	BLOCK 4*6+1

	DISPLAY "Gap before tbl_rows: ", /A, tbl_rows-$

	ALIGN 256
tbl_rows
	REPT 24, row
	DWXYTOSCRADDR 0, row*8
	ENDR

	STRUCT TILEDISTTBL
platformidx		db 0
spikyidx		db 0
crumblyidx		db 0
lconveyoridx	db 0
rconveyoridx	db 0
bgtileidx		db 0
	ENDS

tile_dist_table:
	TILEDISTTBL

willy_colpos	dw 0
willy_xpos	db 0
willy_ypos	db 0

; I DON'T THINK I NEED THESE POINTERS TO THE LISTS... everything is done in just 1 function?
erase16xNlistptr dw erase16xNlist
erase8x16listptr dw erase8x16list
willy_dance_frame	db 0
willy_dance_idx		db 0

bdr_colour	db 0
tune_offset		db 0
tune_tick		db TUNETICKINITIAL
tune_play		db 1

	IF TUNE3LOOPSTHEN2
tune_numloops db 2
	ENDIF
	IF DOUBLEUPTUNE
tune_data	
			REPT 1
			db #80, #80, #72, #72, #66, #66, #60, #60, #56, #56, #66, #66, #56, #56, #56, #56, #51, #51, #60, #60, #51, #51, #51, #51, #56, #56, #66, #66
			db #56, #56, #56, #56, #80, #80, #72, #72, #66, #66, #60, #60, #56, #56, #66, #66, #56, #56, #56, #56, #51, #51, #60, #60, #51, #51, #51, #51
			db #56, #56, #56, #56, #56, #56, #56, #56, #80, #80, #72, #72, #66, #66, #60, #60, #56, #56, #66, #66, #56, #56, #56, #56, #51, #51, #60, #60
			db #51, #51, #51, #51, #56, #56, #66, #66, #56, #56, #56, #56, #80, #80, #72, #72, #66, #66, #60, #60, #56, #56, #66, #66, #56, #56, #40, #40
			db #56, #56, #66, #66, #80, #80, #66, #66, #56, #56, #56, #56, #56, #56, #56, #56
			ENDR
	ELSE
tune_data	db #80, #72, #66, #60, #56, #66, #56, #56, #51, #60, #51, #51, #56, #66
			db #56, #56, #80, #72, #66, #60, #56, #66, #56, #56, #51, #60, #51, #51
			db #56, #56, #56, #56, #80, #72, #66, #60, #56, #66, #56, #56, #51, #60
			db #51, #51, #56, #66, #56, #56, #80, #72, #66, #60, #56, #66, #56, #40
			db #56, #66, #80, #66, #56, #56, #56, #56
	ENDIF

; HL: tokenised string address
; DE: buffer to expand into
; at exit-
; C: proportional string length
expand_measurepr_tokenised_string:
.prelude
	ld c, 0
.body
	ld ix, ('A' - 'a')&255 ; IXL: Camel Case adjustment. IXH: 0 (to set not capitals, which happens most of the time)
	ld a, (hl)
	or a ; first token TOKEN_LEADINGTHE?
	jr z, .leadingthe
.loop
	ld a, (hl)
	inc hl
	cp 31
	jr nz, .not_eos
	dec c ; no need for letter separator at EOS
	xor a
	ld (de), a
	ret
.not_eos
	or a
	jr nz, .notspace

	ld a, ' '
	call add_pr_letterwidth
	jr .body

.notspace
	cp 27
	jr nc, .isspecial
	add 'a'-1
	add ixl ; adjust for Camel Case if required
	ld ixl, ixh ; set to lowercase again now
.readytoput

	call add_pr_letterwidth
	jr .loop

.leadingthe
	inc hl
	push hl
	ld hl, strThe
	jr .docompound
.isspecial

	sub TOKEN_QUESTIONMARK
	ld b, '?'
	jr z, .donedecode
	dec a ; TOKEN_APOSTROPHE?
	ld b, "'"
	jr z, .donedecode

	push hl
	ld hl, strOfThe
	dec a ; TOKEN_MEETSTHE?
	jr z, .docompound
	ld l, strMeetsThe&255 ; ok cos strMeetsThe is aligned to not cross a page
.docompound
	ld a, (hl)
	and 127 ; mask out terminator bit

	call add_pr_letterwidth
	bit 7, (hl)
	inc hl
	jr z, .docompound

	pop hl
	jr .body

.donedecode
	ld a, b
	jr .readytoput

add_pr_letterwidth:
	ld (de), a
	inc de

	; get width
	exx
	rlca ; char*2
	ld bc, FONTBASE
	ld h, c ; 0
	ld l, a
	add hl, hl ; char*4
	add hl, hl ; char*8
	add hl, bc
	ld a, (hl)
	and 15 ; remove flags
	inc a ; account for letter spacing
	exx

	add c
	ld c, a
	ret

	DISPLAY "expand_measurepr_tokenised_string size: ", /A, $-expand_measurepr_tokenised_string

erasecellslist:
	BLOCK 6*2+1 ; max 6 tiles behind willy to be erased per frame

	DISPLAY "Gap before level_tiles: ", /A, level_tiles-$

	ALIGN 256 ; align this for faster access
level_tiles:
	BLOCK 16*2

erase16xNlist:	; list of address to erase, count of rows to erase + 1 byte 0 terminator
	BLOCK 16*3+1

ERASE16xNLISTSIZE	EQU	$-erase16xNlist

	DISPLAY "ERASE16xNLISTSIZE: ",/A, ERASE16xNLISTSIZE

erase8x16list:	; list of 16x16 address to erase + 1 byte 0 terminator
	BLOCK 8*2+1

conveyorlist: 
	; entry in this list
	; WORD screen address
	; BYTE conveyor graphic (top row)
	; BYTE count
	; BYTE conveyor graphic (third row)
	BLOCK 5*MAXCONVEYORS+1

strAir db "AIR", 0
strScore
	db "Score", 0
strHighScore
	db "High Score", 0
strGame
	db "Demo", 0
strOver
	db "Over", 0

key_positions BLOCK MAXKEYS*2+1
frame_counter	db 0
exit_gfx		dw 0
gfx_exit		dw 0
exit_xpos		db 0
exit_ypos		db 0
exit_attrib		db 0
willy_facing	db 0 ; facing. 1 for right, -1 for left
keydata			db 0
num_guardians	db 0
has_solar		db 0
gfx_copyevent	dw 0
air_column		db 0
air_tick		db 0
game_state		db 1;0

score		db 0, 0, 0, 0, 0, 0, 0, 0 ;"00000000"
highscore	db 0, 0, 0, 0, 1, 0, 0, 0 ;"00001000"
scoreishighscore	db 0
time		db 0, 0, 0, 0, 0, 0, 0, 0 ;"00000000"
lvl_time	db 0, 0, 0, 0, 0, 0, 0, 0 ;"00000000"

textoutaddr dw SCRBASE
textxoffset	db 0
lvl_next	dw 0

	; this gap is packed with data now
	DISPLAY "Gap before tile_attribs: ", /A, tile_attribs-$

	ASSERT $-level_tiles <= 256, You'll have to move some data before tile_attribs, it needs to be 256 ahead of level_tiles

	ALIGN 256 ; align this for faster access, and put it 256 after level_tiles
; tile attribs
tile_attribs:
	BLOCK 16 ; can either use tile_attribs[0] as background colour or key paper colour. Not decided yet. tile_attribs[1:15] is the tile attribs anyway

current_level:
	;dw Game_Over
	dw Central_Cavern
	;dw The_Cold_Room
	;dw The_Menagerie
	;dw Abandoned_Uranium_Workings
	;dw Eugenes_Lair
	;dw Processing_Plant
	;dw The_Vat
	;dw Miner_Willy_meets_the_Kong_Beast
	;dw Wacky_Amoebatrons
	;dw The_Endorian_Forest
	;dw Attack_of_the_Mutant_Telephones
	;dw Return_of_the_Alien_Kong_Beast
	;dw Ore_Refinery
	;dw Skylab_Landing_Bay
	;dw The_Bank
	;dw The_Sixteenth_Cavern
	;dw The_Warehouse
	;dw Amoebatrons_Revenge
	;dw Solar_Power_Generator
	;dw The_Final_Barrier

	ALIGN 256
guardian_occupycell_bitmap	BLOCK 64

	ALIGN 256
shift_tbl
	db %1100'0000
	db %0110'0000
	db %0011'0000
	db %0001'1000
	db %0000'1100
	db %0000'0110
	db %0000'0011
	db %0000'0001

Game_Over:
	db 0 ; last in list
	db 0 ; num keys, bright<<7, paper<<4 for key
	
	IF !PACKED
	XY 15, 12 ; willy start pos/facing
	db 15, 14 ; exit position
	IPB 7, 0, 1 ; exit colour
	db 0 ; border colour
	db 0 ; paper colour
	;db 128 ; empty string
	db TOKEN_LEADINGTHE, "Slick Willy Rolling Demo"C
	ENDIF

	EXITANDGUARDIANS gfx_plinth, FALSE, TRUE
	
	VGUARDIANTYPE gfx_eugene
	IPB_LAST	7, 0, 1
	VGUARDIAN	15, 0, 0
	db 0, 98
	VGUARDIANVELFLAGS 2, VGEUGENE

	TILEDIST	0, 0, 0, 0, 0, 1
	IPB_LAST	7, 0, 1
	celltype gfx_blank ; wall is always first tile in list

	db EVENT_GAMEOVER

	db #FF

	DISPLAY "Game Over (unpacked) size: ", /A, $-Game_Over

Central_Cavern:
	db 0 ; last in list
	db 5 ; num keys, bright<<7, paper<<4 for key

	IF !PACKED
	XY 2, 13 ; willy start pos/facing
	db 29, 13 ; exit position
	IPB 6, 1, 0 ; exit colour
	db 2 ; border colour
	db 0 ; paper colour
	db "Central Cavern"C
	ENDIF

	EXITANDGUARDIANS gfx_exit0, TRUE, FALSE

	HGUARDIANTYPE gfx_robot0
	IPB_LAST 6, 0, 1
	HG8FRAME_XY_FLAGS 8, 7, 0, FALSE
	db 8, 15

	keytype gfx_key0

	IF !PACKED
	XY 9, 0
	XY 29, 0
	XY 16, 1
	XY 24, 4
	XY 30, 6
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 3, 5, 6, 0, 7

	IPB			6, 2, 0
	celltype gfx_wall0 ; wall is always first tile in list
	IPB			2, 0, 1
	celltype gfx_platform0
	IPB			4, 0, 1
	celltype gfx_spiky0
	IPB			5, 0, 0
	celltype gfx_spiky1
	IPB			2, 0, 0
	celltype gfx_crumbly0
	IPB_LAST	4, 0, 0
	celltype gfx_conveyor0

	db 4 ; brush and flags
	TILE		11, 0
	TILELAST	16, 0

	db 3 ; brush and flags
	TILE		23, 4
	TILE		27, 4
	TILE		21, 8
	TILELAST	12, 12
	
	; platforms
	db 2|HORZ_REPEAT ; brush and flags
	TILERPT			1, 5, 30
	TILERPT			1, 7, 3
	TILERPT			1, 9, 4
	TILERPT			29, 10, 2
	TILERPT			29, 10, 2
	TILERPT			28, 12, 3
	TILERPT			5, 13, 15
	TILERPT_LAST	1, 15, 30

	; crumbly platforms
	db 5|HORZ_REPEAT ; brush and flags
	TILERPT			14, 5, 4
	TILERPT			19, 5, 4
	TILERPT_LAST	23, 12, 5

	; walls
	db 1|HORZ_REPEAT ; brush and flags
	TILERPT			17, 8, 3
	TILERPT_LAST	20, 12, 3

	; conveyor
	db 6|HORZ_REPEAT ; brush and flags
	TILERPT_LAST	8, 9, 20

	db #FF ; end of level data

	DISPLAY "Central Cavern (unpacked) size: ", /A, $-Central_Cavern

The_Cold_Room:
	db 0 ; last in list
	db 5;|(1<<4)	; number of keys/paper colour

	IF !PACKED
	XY 2, 13 ; willy start pos/facing
	db 29, 13 ; exit position
	IPB 3, 1, 1 ; exit colour
	db 2 ; border colour
	db 0;1 ; paper colour
	db TOKEN_LEADINGTHE, "Cold Room"C
	ENDIF

	EXITANDGUARDIANS gfx_exit1, TRUE, FALSE

	HGUARDIANTYPE gfx_penguin0
	;IPB				6, 1, 0
	IPB					7, 0, 0;1, 0
	HG8FRAME_XY_FLAGS 18, 3, 7, FALSE
	db 1, 18
	;IPB_LAST		5, 0, 0;1, 0
	IPB_LAST		7, 0, 1
	HG8FRAME_XY_FLAGS 29, 13, 7, FALSE
	db 12, 29

	; keytype
	keytype gfx_key2

	IF !PACKED
	XY 7, 1
	XY 25, 1
	XY 26, 7
	XY 3, 9
	XY 19, 12
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 3, 4, 0, 5, 6

	IPB			6, 2, 0
	celltype gfx_wall0 ; wall is always first tile in list. tile1
	;IPB			3, 0, 1;1, 1
	IPB			5, 1, 1
	celltype gfx_platform0 ; tile2
	IPB			5, 0, 0;1, 0
	celltype gfx_spiky1 ; tile3
	IPB			7,1,0;3, 0, 0;1, 0
	celltype gfx_crumbly0 ; tile4
	IPB_LAST	6, 0, 0;1, 0
	celltype gfx_conveyor0 ; tile5

	; single blocks
	db 3 ; brush and flags
	TILELAST	30, 1

	db 2
	TILE		25, 3
	TILELAST	1, 7

	db 2|HORZ_REPEAT
	TILERPT			1, 5, 19
	TILERPT			21, 6, 4
	TILERPT			9, 9, 7
	TILERPT			14, 12, 4
	TILERPT_LAST	1, 15, 30

	db 1|HORZ_REPEAT
	TILERPT_LAST	19, 0, 12

	db 4|HORZ_REPEAT
	TILERPT			21, 3, 4
	TILERPT			2, 7, 5
	TILERPT			19, 10, 4
	TILERPT_LAST	8, 13, 4

	db 5|HORZ_REPEAT
	TILERPT_LAST	3, 11, 4

	db 1|VERT_REPEAT
	TILERPT			25, 6, 7
	TILERPT_LAST	28, 5, 8

	db 4|VERT_REPEAT|HORZ_REPEAT
	TILEHVRPT_LAST	26, 6, 2, 7

	db 0|HORZ_REPEAT ; brush and flags
	TILERPT_LAST	26, 7, 2

	db #FF ; end of level data

	DISPLAY "The Cold Room (unpacked) size: ", /A, $-The_Cold_Room

The_Menagerie:
	db 0 ; last in list
	db 5 ; number of keys/paper colour

	IF !PACKED
	XY 2, 13 ; willy start pos/facing
	db 29, 11 ; exit position
	IPB 6, 1, 0 ; exit colour
	db 2 ; border colour
	db 0 ; paper colour
	db TOKEN_LEADINGTHE, "Menagerie"C
	ENDIF

	EXITANDGUARDIANS gfx_exit2, TRUE, FALSE
	HGUARDIANTYPE gfx_flamingo0
	IPB				3, 0, 1
	HG8FRAME_XY_FLAGS 16, 3, 7, FALSE
	db 1, 16
	IPB				2, 0, 1
	HG8FRAME_XY_FLAGS 18, 3, 0, FALSE
	db 18, 29
	IPB_LAST		4, 0, 1
	HG8FRAME_XY_FLAGS 19, 13, 7, FALSE
	db 1, 19

	; keytype
	keytype gfx_key0

	IF !PACKED
	XY 6, 0
	XY 15, 0
	XY 23, 0
	XY 21, 6
	XY 30, 6
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 4, 5, 6, 0, 7

	; cell graphics
	IPB 5, 1, 0
	celltype gfx_wall2 ; tile1
	IPB 5, 0, 1
	celltype gfx_platform2 ; tile2
	IPB 3, 0, 0 
	celltype gfx_platform3 ; tile3
	IPB 3, 0, 1
	celltype gfx_spiky5 ; tile4
	IPB 5, 0, 0
	celltype gfx_crumbly2 ; tile5
	IPB_LAST 2, 0, 0
	celltype gfx_conveyor2 ; tile6

	; single blocks
	db 4
	TILE		10, 0
	TILE		18, 1
	TILE		27, 0
	TILELAST	1, 11

	db 3
	TILELAST 18, 0

	; repeat blocks
	db 2|HORZ_REPEAT
	TILERPT			1, 15, 30
	TILERPT			1, 5, 4
	TILERPT			1, 7, 6
	TILERPT			27, 7, 4
	TILERPT			25, 10, 6
	TILERPT			21, 13, 10
	TILERPT			5, 12, 6
	TILERPT			14, 11, 5
	TILERPT_LAST	27, 7, 4

	db 5|HORZ_REPEAT
	TILERPT_LAST	5, 5, 26

	db 6|HORZ_REPEAT
	TILERPT_LAST	6, 9, 6

	db 3|VERT_REPEAT
	TILERPT_LAST	1, 8, 3

	db #ff
	DISPLAY "The Menagerie (unpacked) size: ", /A, $-The_Menagerie

Abandoned_Uranium_Workings:
	db 0 ; last in list
	db 5	; number of keys/paper colour

	IF !PACKED
	XY 28, 13+16 ; willy start pos/facing
	db 29, 1 ; exit position
	IPB 6, 1, 0 ; exit colour
	db 2 ; border colour
	db 0 ; paper colour
	db "Abandoned Uranium Workings"C
	ENDIF

	EXITANDGUARDIANS gfx_exit3, TRUE, FALSE
	HGUARDIANTYPE gfx_seal0
	IPB				2, 0, 1
	HG8FRAME_XY_FLAGS 1, 13, 0, FALSE
	db 1, 10
	IPB_LAST		4, 0, 1
	HG8FRAME_XY_FLAGS 7, 13, 0, FALSE
	db 6, 15

	; keytype
	keytype gfx_key0

	IF !PACKED
	XY 1, 0
	XY 12, 1
	XY 25, 1
	XY 16, 6
	XY 30, 6	
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 3, 4, 0, 5, 6

	; cell graphics
	IPB 1, 5, 0
	celltype gfx_wall0		; tile1
	IPB 6, 0, 1
	celltype gfx_platform0	; tile2
	IPB 5, 0, 0
	celltype gfx_spiky6		; tile3
	IPB 6, 0, 0
	celltype gfx_crumbly0	; tile4
	IPB_LAST 3, 0, 1
	celltype gfx_conveyor0	; tile5

	; single blocks
	db 3
	TILE		7, 0
	TILELAST	23, 12

	db 2
	TILE		1, 5
	TILE		7, 5
	TILE		17, 5
	TILELAST	30, 10

	; repeat blocks
	db 1|HORZ_REPEAT
	TILERPT_LAST	14, 0, 17

	db 2|HORZ_REPEAT
	TILERPT			1, 15, 30	
	TILERPT			19, 3, 6	
	TILERPT			27, 4, 4	
	TILERPT			12, 6, 2	
	TILERPT			21, 6, 3	
	TILERPT			7, 8, 2	
	TILERPT			26, 8, 3	
	TILERPT			18, 9, 3	
	TILERPT			12, 11, 3	
	TILERPT			22, 11, 3	
	TILERPT			6, 12, 2	
	TILERPT			28, 12, 3	
	TILERPT_LAST	18, 13, 2

	db 4|HORZ_REPEAT
	TILERPT_LAST	1, 7, 3

	db 5|HORZ_REPEAT
	TILERPT_LAST	1, 10, 3

	db #ff
	DISPLAY "Abandoned Uranium Workings (unpacked) size: ", /A, $-Abandoned_Uranium_Workings

Eugenes_Lair:
	db 0 ; last in list
	db 5|(2<<4)	; number of keys/paper colour

	IF !PACKED
	XY 1, 3 ; willy start pos/facing
	db 15, 13 ; exit position
	IPB 7, 2, 0 ; exit colour
	db 1 ; border colour
	db 2 ; paper colour
	db "Eugene", TOKEN_APOSTROPHE, "s Lair"C
	ENDIF

	EXITANDGUARDIANS gfx_exit4, TRUE, TRUE

	VGUARDIANTYPE gfx_eugene
	IPB_LAST	7, 2, 0
	VGUARDIAN	15, 0, 0
	db 0, 88
	VGUARDIANVELFLAGS 1, VGEUGENE

	HGUARDIANTYPE gfx_toilet0
	IPB				6, 2, 0
	HG8FRAME_XY_FLAGS 12, 3, 7, FALSE
	db 1, 12
	IPB_LAST		0, 2, 0
	HG8FRAME_XY_FLAGS 4, 7, 0, FALSE
	db 4, 12

	; keytype
	keytype gfx_key3

	IF !PACKED
	XY 30, 1
	XY 10, 6
	XY 29, 7
	XY 7, 12
	XY 9, 12
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 3, 5, 6, 0, 7

	; cell graphics
	IPB 7, 4, 0
	celltype gfx_wall0		; tile1
	IPB 5, 2, 0
	celltype gfx_platform0	; tile2
	IPB 6, 2, 0
	celltype gfx_spiky0		; tile3
	IPB 3, 2, 0
	celltype gfx_spiky7		; tile4
	IPB 4, 2, 0
	celltype gfx_crumbly0	; tile5
	IPB_LAST 6, 2, 1
	celltype gfx_conveyor3	; tile6

	; single blocks
	db 4
	TILELAST	20,0

	db 3
	TILE		24, 4
	TILE		21, 7
	TILELAST	5, 14

	db 2
	TILELAST	30, 11

	; repeat blocks
	db 3|HORZ_REPEAT
	TILERPT_LAST	24, 14, 2

	db 2|HORZ_REPEAT
	TILERPT			1, 15, 30
	TILERPT			1, 5, 13
	TILERPT			22, 5, 6
	TILERPT			29, 6, 2
	TILERPT			4, 9, 10
	TILERPT			3, 11, 11
	TILERPT			18, 11, 7
	TILERPT_LAST	1, 13, 2

	db 5|HORZ_REPEAT
	TILERPT			18, 5, 4
	TILERPT_LAST	1, 11, 2

	db 6|HORZ_REPEAT
	TILERPT_LAST	18, 8, 10

	db 1|HORZ_REPEAT
	TILERPT			14, 13, 4
	TILERPT			14, 14, 10
	TILERPT_LAST	8, 15, 16

	db 1|VERT_REPEAT
	;TILERPT			14, 13, 2
	TILERPT_LAST	8, 12, 3

	db #ff
	DISPLAY "Eugene's Lair (unpacked) size: ", /A, $-Eugenes_Lair

Processing_Plant:
	db 0 ; last in list
	db 5 ; num keys, bright<<7, paper<<4 for key

	IF !PACKED
	XY 15, 3+16 ; willy start pos/facing
	db 29, 0 ; exit position
	IPB 6, 1, 0 ; exit colour
	db 2 ; border colour
	db 0 ; paper colour
	db "Processing Plant"C
	ENDIF

	EXITANDGUARDIANS gfx_exit5, TRUE, FALSE
	HGUARDIANTYPE gfx_pacman0
	IPB				6, 0, 1
	HG8FRAME_XY_FLAGS 6, 8, 0, FALSE
	db 6, 13
	IPB				3, 0, 1
	HG8FRAME_XY_FLAGS 14, 8, 1, FALSE
	db 14, 21
	IPB				5, 0, 1
	HG8FRAME_XY_FLAGS 8, 13, 2, FALSE
	db 8, 20
	IPB_LAST		6, 0, 0
	HG8FRAME_XY_FLAGS 24, 13, 3, FALSE
	db 24, 29

	; keytype
	keytype gfx_key0

	IF !PACKED
	XY 15, 6
	XY 17, 6
	XY 30, 7
	XY 1, 10
	XY 13, 11
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 3, 0, 5, 0, 6

	; cell graphics
	IPB 6, 2, 0
	celltype gfx_wall3		; tile1
	IPB 4, 0, 1
	celltype gfx_platform4	; tile2
	IPB 3, 0, 1
	celltype gfx_spiky0		; tile3
	IPB 6, 0, 0
	celltype gfx_spiky8		; tile4
	IPB_LAST 5, 0, 0
	celltype gfx_conveyor0	; tile5

	; single blocks
	db 4
	TILE		21, 4
	TILELAST	17, 11

	db 3
	TILELAST	3, 12

	; repeat blocks
	db 2|HORZ_REPEAT
	TILERPT			1, 15, 30
	TILERPT			8, 5, 3
	TILERPT			15, 5, 2
	TILERPT			21, 5, 5
	TILERPT			3, 6, 2
	TILERPT			28, 6, 3
	TILERPT			23, 8, 5
	TILERPT			1, 9, 2
	TILERPT			7, 10, 19
	TILERPT			28, 12, 3
	TILERPT_LAST	22, 13, 2

	; repeat blocks
	db 5|HORZ_REPEAT
	TILERPT_LAST	3, 13, 4

	IF 1 ; can make this better if horz guardians clear their path. Although it would only save memory on this level and code might be longer. 
	db 1|VERT_REPEAT
	TILERPT			16, 6, 2
	TILERPT_LAST	16, 10, 3
	ELSE
	db 1|VERT_REPEAT
	TILERPT_LAST	16, 6, 7
	ENDIF

	db #ff
	DISPLAY "Processing Plant (unpacked) size: ", /A, $-Processing_Plant

The_Vat:
	db 0 ; last in list
	db 5|(2<<4)	; number of keys/key paper ; change of key paper does not work!!!

	IF !PACKED
	XY 2, 13 ; willy start pos/facing
	db 15, 13 ; exit position
	IPB 3, 1, 0 ; exit colour
	db 4 ; border colour
	db 0 ; paper colour
	db TOKEN_LEADINGTHE, "Vat"C
	ENDIF

	EXITANDGUARDIANS gfx_exit6, TRUE, FALSE
	HGUARDIANTYPE gfx_kangaroo0
	IPB				5, 0, 1
	HG8FRAME_XY_FLAGS 15, 1, 0, FALSE
	db 15, 29
	IPB				3, 0, 1
	HG8FRAME_XY_FLAGS 10, 8, 7, FALSE
	db 2, 10
	IPB_LAST		6, 0, 0
	HG8FRAME_XY_FLAGS 17, 13, 0, FALSE
	db 17, 29

	; keytype
	keytype gfx_key0

	IF !PACKED
	XY 30, 3
	XY 20, 6
	XY 27, 7
	XY 19, 10
	XY 30, 11
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 3, 4, 5, 0, 6

	; cell graphics
	IPB 5, 1, 1
	celltype gfx_wall0		; tile1
	IPB 6, 0, 1
	celltype gfx_platform0	; tile2
	IPB 6, 2, 0
	celltype gfx_spiky9		; tile3
	IPB 2, 0, 0
	celltype gfx_crumbly3	; tile4
	IPB_LAST 4, 0, 0
	celltype gfx_conveyor4	; tile5

	; single blocks
	db 2
	TILELAST	1, 8

	db 2|HORZ_REPEAT
	TILERPT			1, 15, 13
	TILERPT			15, 3, 2
	TILERPT			14, 5, 3
	TILERPT			1, 6, 3
	TILERPT			14, 9, 3
	TILERPT			1, 10, 11
	TILERPT_LAST	9, 13, 3

	; can make these walls cheaper with HORZ+VERT REPEAT too
	db 1|HORZ_REPEAT
	TILERPT			14, 0, 17
	TILERPT_LAST	14, 15, 17

	db 1|VERT_REPEAT
	TILERPT_LAST	17, 3, 10

	db 1|HORZ_REPEAT|VERT_REPEAT
	TILEHVRPT_LAST	14, 12, 3, 3

	db 5|HORZ_REPEAT
	TILERPT_LAST	7, 5, 5

	db 4|HORZ_REPEAT|VERT_REPEAT
	TILEHVRPT_LAST 18, 3, 13, 10

	; overwrite some crumbly platforms with spikys
	db 3
	TILE		28, 5		
	TILE		22, 8		
	TILE		28, 10		
	TILE		22, 12
	TILELAST	22, 12

	db #ff
	DISPLAY "The Vat (unpacked) size: ", /A, $-The_Vat

Miner_Willy_meets_the_Kong_Beast:
	db 0 ; last in list
	db 4 ; num keys, bright<<7, paper<<4 for key

	IF !PACKED
	XY 2, 13 ; willy start pos/facing
	db 15, 13 ; exit position
	IPB 6, 1, 0 ; exit colour
	db 2 ; border colour
	db 0 ; paper colour
	db "Miner Willy", TOKEN_MEETSTHE, "Kong Beast"C
	ENDIF

	EXITANDGUARDIANS gfx_exit7, TRUE, TRUE

	VGUARDIANTYPE gfx_kong0
	IPB_LAST	4, 0, 1
	VGUARDIAN	15, 0, 0
	db 0, 0
	VGUARDIANVELFLAGS 0, VGKONG

	HGUARDIANTYPE gfx_barrel0
	IPB				3, 0, 1
	HG4FRAME_XY_FLAGS 11, 11, 0, TRUE
	db 11, 15
	IPB				4, 0, 1
	HG4FRAME_XY_FLAGS 9, 13, 7, FALSE
	db 1, 9
	IPB_LAST		5, 0, 0
	HG4FRAME_XY_FLAGS 18, 7, 0, FALSE
	db 18, 21

	; keytype
	keytype gfx_key4

	IF !PACKED
	XY 13, 2
	XY 14, 6
	XY 2, 8
	XY 29, 13
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 5, 0, 0, 7, 8

	; cell graphics
	IPB 2, 6, 1
	celltype gfx_wall0		; tile1
	IPB 2, 0, 0
	celltype gfx_platform0	; tile2
	IPB 6, 0, 0 
	celltype gfx_switch0	; tile3
	IPB 6, 0, 0 
	celltype gfx_switch1	; tile4
	IPB 4, 0, 0
	celltype gfx_spiky0		; tile5
	IPB 5, 0, 0
	celltype gfx_spiky7		; tile6
	IPB_LAST 4, 0, 1
	celltype gfx_conveyor5	; tile7

	db 6
	TILE		2, 0
	TILELAST	10, 0

	db 5
	TILELAST	23, 14

	db 3
	TILE		6, 0	; SWITCH
	TILELAST	18, 0	; SWITCH

	db 2
	TILE		30, 5
	TILE		27, 7
	TILELAST	1, 10

	db 2|HORZ_REPEAT
	TILERPT			1, 15, 30
	TILERPT			15, 2, 2
	TILERPT			29, 2, 2
	TILERPT			1, 5, 3
	TILERPT			9, 5, 6
	TILERPT			18, 5, 2
	TILERPT			21, 6, 4
	TILERPT			2, 7, 3
	TILERPT			8, 8, 3
	TILERPT			18, 9, 5
	TILERPT			12, 10, 3
	TILERPT			27, 10, 4
	TILERPT			9, 11, 2
	TILERPT			4, 12, 2
	TILERPT			22, 12, 5
	TILERPT_LAST	18, 13, 2

	db 7|HORZ_REPEAT
	TILERPT_LAST	11, 13, 3

	db 1|VERT_REPEAT
	TILERPT			14, 13, 2
	TILERPT			17, 0, 15
	TILERPT_LAST	20, 0, 2

	db #ff
	DISPLAY "Miner Willy meets the Kong Beast (unpacked) size: ", /A, $-Miner_Willy_meets_the_Kong_Beast

Wacky_Amoebatrons:
	db 0 ; last in list
	db 1 ; num keys, bright<<7, paper<<4 for key

	IF !PACKED
	XY 1, 13 ; willy start pos/facing
	db 1, 0 ; exit position
	IPB 6, 1, 0 ; exit colour
	db 1 ; border colour
	db 0 ; paper colour
	db "Wacky Amoebatrons"C
	ENDIF

	EXITANDGUARDIANS gfx_exit8, TRUE, TRUE

	VGUARDIANTYPE gfx_amoeba0
	IPB				3, 0, 1
	VGUARDIAN	5, 8, 0
	db 5, 100
	VGUARDIANVELFLAGS 1, 0

	IPB				4, 0, 0
	VGUARDIAN	10, 8, 1
	db 5, 100
	VGUARDIANVELFLAGS 2, 0

	IPB				5, 0, 0
	VGUARDIAN	20, 8, 2
	db 5, 100
	VGUARDIANVELFLAGS 1, 0

	IPB_LAST		2, 0, 1
	VGUARDIAN	25, 8, 0
	db 5, 100
	VGUARDIANVELFLAGS 2, 0

	HGUARDIANTYPE gfx_pole0
	IPB				4, 0, 1
	HG4FRAME_XY_FLAGS 12, 3, 0, FALSE
	db 12, 18
	IPB_LAST		5, 0, 0
	HG4FRAME_XY_FLAGS 16, 10, 0, TRUE
	db 12, 18

	; keytype
	keytype gfx_key0

	IF !PACKED
	XY 16, 1
	ENDIF


	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 0, 0, 0, 3, 4

	IPB 6, 2, 0
	celltype gfx_wall4		; tile1
	IPB 6, 0, 0
	celltype gfx_platform0	; tile2
	IPB_LAST 4, 0, 0
	celltype gfx_conveyor6	; tile3

	;db EVENT_SOLAR
	;db 25 ; start position
	;IPB 7, 0, 0 ; background colour
	;IPB 6, 5, 1 ; beam colour

	db 1
	TILELAST	3, 0

	db 2|HORZ_REPEAT
	TILERPT			1, 15, 30
	TILERPT			1, 5, 4
	TILERPT			7, 5, 3
	TILERPT			12, 5, 8
	TILERPT			22, 5, 3
	TILERPT			27, 5, 2
	TILERPT			29, 7, 2
	TILERPT			3, 8, 2
	TILERPT			7, 8, 3
	TILERPT			22, 9, 3
	TILERPT			27, 9, 2
	TILERPT			1, 10, 2
	TILERPT			3, 12, 2
	TILERPT			7, 12, 3
	TILERPT			12, 12, 8
	TILERPT			22, 12, 3
	TILERPT			27, 12, 2
	TILERPT_LAST	29, 13, 2

	db 3|HORZ_REPEAT
	TILERPT_LAST	12, 8, 8

	db #ff
	DISPLAY "Wacky Amoebatrons (unpacked) size: ", /A, $-Wacky_Amoebatrons

The_Endorian_Forest:
	db 0 ; last in list
	db 5 ; num keys, bright<<7, paper<<4 for key

	IF !PACKED
	XY 1, 4 ; willy start pos/facing
	db 12, 13 ; exit position
	IPB 6, 3, 0 ; exit colour
	db 2 ; border colour
	db 0 ; paper colour
	db TOKEN_LEADINGTHE, "Endorian Forest"C
	ENDIF

	EXITANDGUARDIANS gfx_exit9, TRUE, FALSE
	HGUARDIANTYPE gfx_ewok0
	IPB				6, 0, 1
	HG8FRAME_XY_FLAGS 9, 7, 0, FALSE
	db 9, 14
	IPB				2, 0, 1
	HG8FRAME_XY_FLAGS 12, 10, 0, TRUE
	db 8, 14
	IPB				3, 0, 1
	HG8FRAME_XY_FLAGS 8, 13, 0, FALSE
	db 4, 26

	IF 0 ; Test 8 horizontal guardian speed
	IPB				1, 0, 1
	HG8FRAME_XY_FLAGS 1, 0, 0, TRUE
	db 1, 5
	IPB				7, 0, 0
	HG8FRAME_XY_FLAGS 1, 11, 0, TRUE
	db 1, 2
	IPB				6, 0, 0
	HG8FRAME_XY_FLAGS 17, 2, 0, TRUE
	db 17, 19
	IPB				7, 0, 1
	HG8FRAME_XY_FLAGS 23, 3, 0, FALSE
	db 23, 29
	ENDIF

	IPB_LAST		5, 0, 0
	HG8FRAME_XY_FLAGS 18, 5, 0, FALSE
	db 17, 21

	; keytype
	keytype gfx_key5

	IF !PACKED
	XY 14, 1
	XY 30, 1
	XY 21, 2
	XY 12, 6
	XY 18, 8
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 4, 5, 0, 0, 6

	; cell graphics
	IPB 6, 2, 0
	celltype gfx_wall5		; tile1
	IPB 4, 0, 1
	celltype gfx_platform5	; tile2
	IPB 5, 0, 0 
	celltype gfx_platform6	; tile3
	IPB 4, 0, 0
	celltype gfx_spiky10	; tile4
	IPB_LAST 2, 0, 0
	celltype gfx_crumbly4	; tile5

	db 4
	TILE		11, 0
	TILE		18, 0
	TILE		20, 0
	TILE		21, 1
	TILE		3, 3
	TILE		1, 9
	TILE		30, 10
	TILELAST	23, 11

	db 2
	TILELAST	8, 5

	db 3|HORZ_REPEAT
	TILERPT			0, 15, 32
	TILERPT_LAST	8, 12, 10

	db 2|HORZ_REPEAT
	TILERPT			13, 0, 3
	TILERPT			21, 0, 10
	TILERPT			1, 2, 6
	TILERPT			27, 2, 4
	TILERPT			17, 4, 4
	TILERPT			23, 5, 8
	TILERPT			1, 6, 4
	TILERPT			17, 7, 7
	TILERPT			1, 8, 5
	TILERPT			9, 9, 7
	TILERPT			29, 9, 2
	TILERPT			1, 10, 4
	TILERPT			17, 10, 7
	TILERPT			1, 13, 3
	TILERPT_LAST	28, 13, 3

	db 5|HORZ_REPEAT
	TILERPT			9, 5, 7
	TILERPT			24, 7, 3
	TILERPT			5, 10, 2
	TILERPT_LAST	24, 11, 3

	db 1|VERT_REPEAT
	TILERPT_LAST	16, 0, 12

	db #ff
	DISPLAY "The Endorian Forest (unpacked) size: ", /A, $-The_Endorian_Forest

Attack_of_the_Mutant_Telephones:
	db 0 ; last in list
	db 5 ; num keys, bright<<7, paper<<4 for key

	IF !PACKED
	XY 3, 1 ; willy start pos/facing
	db 1, 1 ; exit position
	IPB 6, 2, 0 ; exit colour
	db 2 ; border colour
	db 0 ; paper colour
	db "Attack", TOKEN_OFTHE, "Mutant Telephones"C
	ENDIF

	EXITANDGUARDIANS gfx_exit10, TRUE, TRUE

	VGUARDIANTYPE gfx_telephone0

	IPB					3, 0, 1
	VGUARDIAN 12, 8, 0
	db 2, 56
	VGUARDIANVELFLAGS 2, 0 ; speed and flags

	IPB					4, 0, 0
	VGUARDIAN 3, 32, 1
	db 32, 100
	VGUARDIANVELFLAGS 1, 0 ; speed and flags

	IPB					6, 0, 0
	VGUARDIAN 21, 48, 2
	db 48, 100
	VGUARDIANVELFLAGS 1, 0 ; speed and flags

	IPB_LAST			2, 0, 1
	VGUARDIAN 26, 48, 3
	db 4, 100
	VGUARDIANVELFLAGS -3, 0 ; speed and flags

	HGUARDIANTYPE gfx_satellitedish0
	IPB				6, 0, 1
	HG4FRAME_XY_FLAGS 15, 3, 0, FALSE
	db 15, 24
	IPB				4, 0, 1
	HG4FRAME_XY_FLAGS 14, 7, 0, TRUE
	db 14, 18
	IPB_LAST		2, 0, 1
	HG4FRAME_XY_FLAGS 15, 13, 7, FALSE
	db 5, 19

	; keytype
	keytype gfx_key6

	IF !PACKED
	XY 24, 0
	XY 30, 1
	XY 1, 4
	XY 19, 6
	XY 30, 13
	ENDIF
	
	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 5, 6, 7, 0, 8

	; cell graphics
	IPB 6, 1, 0
	celltype gfx_wall6		; tile1
	IPB 1, 0, 1
	celltype gfx_platform0	; tile2
	IPB 5, 0, 1 
	celltype gfx_platform7	; tile3
	IPB 2, 0, 1 
	celltype gfx_platform3	; tile4
	IPB 6, 0, 1
	celltype gfx_spiky11	; tile5
	IPB 1, 0, 0
	celltype gfx_crumbly0	; tile6
	IPB_LAST 6, 0, 0
	celltype gfx_conveyor7	; tile7

	db 4
	TILE		19, 0
	TILELAST	19, 10

	db 2
	TILE		28, 10
	TILELAST	9, 11

	db 5
	TILE		19, 1
	TILE		24, 9
	TILE		19, 11
	TILELAST	12, 12

	db 2|HORZ_REPEAT
	TILERPT			1, 15, 30
	TILERPT			1, 3, 4
	TILERPT			5, 5, 6
	TILERPT			15, 5, 11
	TILERPT			29, 6, 2
	TILERPT			1, 8, 2
	TILERPT			29, 8, 2
	TILERPT			11, 9, 9
	TILERPT			28, 12, 3
	TILERPT			1, 13, 2
	TILERPT_LAST	23, 13, 3

	db 3|HORZ_REPEAT
	TILERPT_LAST	17, 5, 7

	db 6|HORZ_REPEAT
	TILERPT_LAST	6, 11, 3

	db 1|HORZ_REPEAT
	TILERPT_LAST	1, 0, 6

	db 7|HORZ_REPEAT
	TILERPT_LAST	5, 8, 2

	db 4|VERT_REPEAT
	TILERPT			12, 10, 2
	TILERPT_LAST	24, 6, 3

	db #ff
	DISPLAY "Attack of the Mutant Telephones (unpacked) size: ", /A, $-Attack_of_the_Mutant_Telephones

Return_of_the_Alien_Kong_Beast:
	db 0 ; last in list
	db 5 ; num keys, bright<<7, paper<<4 for key

	IF !PACKED
	XY 2, 13 ; willy start pos/facing
	db 15, 13 ; exit position
	IPB 6, 3, 1 ; exit colour
	db 2 ; border colour
	db 0 ; paper colour
	db "Return", TOKEN_OFTHE, "Alien Kong Beast"C
	ENDIF

	EXITANDGUARDIANS gfx_exit11, TRUE, TRUE

	VGUARDIANTYPE gfx_kong0
	IPB_LAST	4, 0, 1
	VGUARDIAN	15, 0, 0
	db 0, 0
	VGUARDIANVELFLAGS 0, VGKONG

	HGUARDIANTYPE gfx_barrel0
	IPB				5, 0, 0
	HG4FRAME_XY_FLAGS 25, 6, 0, FALSE
	db 25, 28
	IPB				6, 0, 1
	HG4FRAME_XY_FLAGS 11, 11, 0, TRUE
	db 11, 15
	IPB_LAST		4, 0, 1
	HG4FRAME_XY_FLAGS 9, 13, 7, FALSE
	db 1, 9

	; keytype
	keytype gfx_key4

	IF !PACKED
	XY 15, 3
	XY 26, 5
	XY 2, 6
	XY 16, 7
	XY 29, 13
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 5, 7, 0, 8, 9

	; cell graphics
	IPB 5, 4, 1
	celltype gfx_wall0		; tile1
	IPB 3, 0, 1
	celltype gfx_platform0	; tile2
	IPB 6, 0, 0 
	celltype gfx_switch0	; tile3
	IPB 6, 0, 0 
	celltype gfx_switch1	; tile4
	IPB 4, 0, 0
	celltype gfx_spiky0		; tile5
	IPB 5, 0, 0
	celltype gfx_spiky7		; tile6
	IPB 3, 0, 0
	celltype gfx_crumbly0	; tile7
	IPB_LAST 6, 0, 1
	celltype gfx_conveyor5	; tile8

	db 6
	TILE		2, 0
	TILELAST	10, 0

	db 5
	TILE		23, 12
	TILELAST	28, 12

	db 1
	TILE		17, 0
	TILELAST	21, 0

	db 2
	TILE		30, 6
	TILE		3, 8
	TILELAST	6, 10

	db 3
	TILE		6, 0	; SWITCH
	TILELAST	18, 0	; SWITCH

	db 2|HORZ_REPEAT
	TILERPT			1, 15, 30
	TILERPT			1, 5, 3
	TILERPT			24, 5, 2
	TILERPT			6, 7, 2
	TILERPT			25, 8, 6
	TILERPT			10, 9, 4
	TILERPT			18, 10, 3
	TILERPT			25, 11, 2
	TILERPT			1, 12, 6
	TILERPT_LAST	11, 13, 3

	db 7|HORZ_REPEAT
	TILERPT			15, 2, 2
	TILERPT			9, 5, 5
	TILERPT_LAST	18, 5, 6

	db 8|HORZ_REPEAT
	TILERPT_LAST	18, 13, 11

	;db 1|HORZ_REPEAT
	;TILERPT_LAST	14, 15, 4

	db 1|VERT_REPEAT
	TILERPT			14, 5, 5
	;TILERPT			14, 13, 2
	TILERPT_LAST	17, 5, 8;10

	db 1|VERT_REPEAT|HORZ_REPEAT
	TILEHVRPT_LAST	14, 13, 4, 3

	db #ff
	DISPLAY "Return of the Alien Kong Beast (unpacked) size: ", /A, $-Return_of_the_Alien_Kong_Beast

Ore_Refinery:
	db 0 ; last in list
	db 5 ; num keys, bright<<7, paper<<4 for key

	IF !PACKED
	XY 29, 13+16 ; willy start pos/facing
	db 1, 13 ; exit position
	IPB 7, 1, 1 ; exit colour
	db 1 ; border colour
	db 0 ; paper colour
	db "Ore Refinery"C
	ENDIF

	EXITANDGUARDIANS gfx_exit12, TRUE, TRUE

	VGUARDIANTYPE gfx_eye0
	IPB_LAST	7, 0, 1
	VGUARDIAN	5, 8, 0
	db 8, 100
	VGUARDIANVELFLAGS 2, 0

	HGUARDIANTYPE gfx_oremachine0
	IPB				3, 0, 1
	HG4FRAME_XY_FLAGS 7, 1, 0, FALSE
	db 7, 29
	IPB				4, 0, 1
	HG4FRAME_XY_FLAGS 16, 4, 0, TRUE
	db 7, 29
	IPB				6, 0, 1
	HG4FRAME_XY_FLAGS 20, 7, 7, FALSE
	db 10, 26
	IPB_LAST		2, 0, 1
	HG4FRAME_XY_FLAGS 18, 10, 0, TRUE
	db 7, 29

	; keytype
	keytype gfx_key7

	IF !PACKED
	XY 26, 3
	XY 10, 6
	XY 19, 9
	XY 26, 9
	XY 11, 12
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 3, 0, 0, 4, 5

	; cell graphics
	IPB 6, 2, 0
	celltype gfx_wall4		; tile1
	IPB 5, 0, 0
	celltype gfx_platform8	; tile2
	IPB 6, 0, 0 
	celltype gfx_platform9	; tile3
	IPB_LAST 4, 0, 0
	celltype gfx_conveyor6	; tile4

	db 2
	TILELAST	30, 6

	db 2|HORZ_REPEAT
	TILERPT			1, 15, 30
	TILERPT			7, 3, 18
	TILERPT			27, 3, 4
	TILERPT			7, 6, 2
	TILERPT			11, 6, 4
	TILERPT			17, 6, 5
	TILERPT			24, 6, 4
	TILERPT			7, 9, 5
	TILERPT			14, 9, 3
	TILERPT			20, 9, 5
	TILERPT			27, 9, 4
	TILERPT			7, 12, 3
	TILERPT			12, 12, 3
	TILERPT			17, 12, 4
	TILERPT			23, 12, 4
	TILERPT_LAST	29, 12, 2

	db 1|HORZ_REPEAT
	TILERPT_LAST	1, 0, 30

	db 4|HORZ_REPEAT
	TILERPT_LAST	3, 15, 26

	db 3|VERT_REPEAT
	TILERPT_LAST	3, 1, 14

	db #ff
	DISPLAY "Ore Refinery (unpacked) size: ", /A, $-Ore_Refinery

Skylab_Landing_Bay:
	db 0 ; last in list
	db 4|(1<<4)	; number of keys/paper colour

	IF !PACKED
	XY 29, 13 ; willy start pos/facing
	db 15, 0 ; exit position
	IPB 6, 3, 0 ; exit colour
	db 6 ; border colour
	db 1 ; paper colour
	db "Skylab Landing Bay"C
	ENDIF

	EXITANDGUARDIANS gfx_exit13, FALSE, TRUE

	VGUARDIANTYPE gfx_skylab0

	IPB			7, 1, 0
	VGUARDIAN	1, 0, 0
	db 0, 72
	VGUARDIANVELFLAGS 4, VGSKYLAB

	IPB			5, 1, 0
	VGUARDIAN	11, 0, 0
	db 0, 32
	VGUARDIANVELFLAGS 1, VGSKYLAB

	IPB_LAST	6, 1, 0
	VGUARDIAN	21, 2, 0
	db 2, 56
	VGUARDIANVELFLAGS 3, VGSKYLAB

	; keytype
	keytype gfx_key8

	IF !PACKED
	XY 23, 2
	XY 16, 7
	XY 27, 7 ; missed this key last time LOL
	XY 3, 8
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 0, 0, 4, 0, 5

	; cell graphics
	IPB 0, 5, 1
	celltype gfx_wall7		; tile1
	IPB 4, 1, 1
	celltype gfx_platform10	; tile2
	IPB 4, 1, 0 
	celltype gfx_platform11	; tile3
	IPB_LAST 3, 1, 1
	celltype gfx_conveyor6	; tile4

	db 2
	TILE		15, 5
	TILE		3, 6
	TILE		11, 6
	TILE		19, 6
	TILE		27, 6
	TILE		7, 7
	TILE		23, 7
	TILE		5, 9
	TILE		13, 9
	TILE		21, 9
	TILE		29, 9
	TILE		1, 11
	TILE		9, 11
	TILE		25, 11
	TILELAST	7, 13

	db 3
	TILE		16, 5
	TILE		4, 6
	TILE		12, 6
	TILE		20, 6
	TILE		28, 6
	TILE		8, 7
	TILE		24, 7
	TILE		6, 9
	TILE		14, 9
	TILE		22, 9
	TILE		30, 9
	TILE		2, 11
	TILE		10, 11
	TILE		26, 11
	TILELAST	8, 13

	db 1|HORZ_REPEAT
	TILERPT_LAST 1, 15, 30

	db 4|HORZ_REPEAT
	TILERPT_LAST 15, 11, 6

	db #ff
	DISPLAY "Skylab Landing Bay (unpacked) size: ", /A, $-Skylab_Landing_Bay

The_Bank:
	db 0 ; last in list
	db 3	; number of keys. This was wrong in 2010 version! Had an extra key

	IF !PACKED
	XY 2, 13 ; willy start pos/facing
	db 1, 3 ; exit position
	IPB 6, 2, 1 ; exit colour
	db 2 ; border colour
	db 0 ; paper colour
	db TOKEN_LEADINGTHE, "Bank"C
	ENDIF

	EXITANDGUARDIANS gfx_exit14, TRUE, TRUE

	VGUARDIANTYPE gfx_safe0

	IPB			6, 0, 0
	VGUARDIAN	9, 40, 0
	db 36, 102
	VGUARDIANVELFLAGS 2, 0

	IPB			7, 0, 0
	VGUARDIAN	15, 64, 1
	db 36, 102
	VGUARDIANVELFLAGS 1, 0

	IPB_LAST	4, 0, 1
	VGUARDIAN	21, 80, 3
	db 32, 104
	VGUARDIANVELFLAGS -3, 0

	HGUARDIANTYPE gfx_cheque0
	IPB_LAST		5, 0, 1
	HG4FRAME_XY_FLAGS 17, 13, 0, FALSE
	db 17, 19

	; keytype
	keytype gfx_key9

	IF !PACKED
	XY 25, 2
	XY 12, 6
	XY 26, 14
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 5, 6, 7, 0, 8

	; cell graphics
	IPB 6, 1, 0
	celltype gfx_wall6		; tile1
	IPB 1, 0, 1
	celltype gfx_platform12	; tile2
	IPB 6, 0, 0 
	celltype gfx_platform13	; tile3
	IPB 2, 0, 1
	celltype gfx_platform3	; tile4
	IPB 6, 0, 1
	celltype gfx_spiky11	; tile5
	IPB 1, 0, 0
	celltype gfx_crumbly0	; tile6
	IPB_LAST 5, 0, 1
	celltype gfx_conveyor7	; tile7

	db 4
	TILELAST	8, 4

	db 5
	TILE		8, 5
	TILELAST	28, 10

	db 6
	TILELAST	7, 7

	db 2|HORZ_REPEAT
	TILERPT			1, 5, 5
	TILERPT			24, 3, 5
	TILERPT			24, 6, 2 ; last version had a typo here
	TILERPT			12, 7, 2
	TILERPT			3, 8, 2
	TILERPT			18, 8, 2
	TILERPT			25, 9, 2
	TILERPT			1, 10, 2
	TILERPT			12, 10, 2
	TILERPT			18, 11, 2
	TILERPT			5, 12, 3
	TILERPT			23, 12, 2
	TILERPT			12, 13, 2
	TILERPT_LAST	1, 15, 30

	db 1|HORZ_REPEAT
	TILERPT_LAST	6, 0, 25

	db 7|HORZ_REPEAT
	TILERPT_LAST	8, 3, 16

	db 4|VERT_REPEAT
	TILERPT_LAST	28, 4, 6

	db 3|VERT_REPEAT|HORZ_REPEAT
	TILEHVRPT_LAST	29, 1, 2, 14

	db #ff
	DISPLAY "The Bank (unpacked) size: ", /A, $-The_Bank

The_Sixteenth_Cavern:
	db 0 ; last in list
	db 4	; number of keys

	IF !PACKED
	XY 2, 13 ; willy start pos/facing
	db 12, 5 ; exit position
	IPB 6, 3, 1 ; exit colour
	db 2 ; border colour
	db 0 ; paper colour
	db TOKEN_LEADINGTHE, "Sixteenth Cavern"C
	ENDIF

	EXITANDGUARDIANS gfx_exit15, TRUE, FALSE
	HGUARDIANTYPE gfx_flagbug0
	IPB				5, 0, 0
	HG8FRAME_XY_FLAGS 26, 5, 0, TRUE
	db 25, 29
	IPB				3, 0, 1
	HG8FRAME_XY_FLAGS 18, 7, 0, FALSE
	db 18, 23
	IPB				6, 0, 0
	HG8FRAME_XY_FLAGS 1, 10, 0, FALSE
	db 1, 7
	IPB_LAST		4, 0, 1
	HG8FRAME_XY_FLAGS 9, 13, 0, FALSE
	db 1, 18

	; keytype
	keytype gfx_key10

	IF !PACKED
	XY 1, 0
	XY 30, 2
	XY 13, 7
	XY 17, 10
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 3, 4, 5, 0, 6

	; cell graphics
	IPB 5, 4, 1
	celltype gfx_wall8		; tile1
	IPB 2, 0, 1
	celltype gfx_platform0	; tile2
	IPB 4, 0, 0
	celltype gfx_spiky12	; tile3
	IPB 2, 0, 0
	celltype gfx_crumbly0	; tile4
	IPB_LAST 6, 0, 1
	celltype gfx_conveyor5	;tile5

	db 2
	TILE		1, 5	
	TILE		6, 5
	TILE		4, 7
	TILE		26, 11
	TILE		20, 13
	TILELAST	26, 13

	db 1
	TILELAST	14, 5

	db 2|HORZ_REPEAT
	TILERPT			1, 15, 30
	TILERPT			22, 5, 3
	TILERPT			25, 7, 6
	TILERPT			12, 11, 2
	TILERPT_LAST	1, 12, 9

	db 1|HORZ_REPEAT
	TILERPT			14, 6, 2
	TILERPT			14, 7, 3
	TILERPT			14, 8, 4
	TILERPT_LAST	10, 11, 2

	db 4|HORZ_REPEAT
	TILERPT_LAST	1, 9, 2

	db 3|HORZ_REPEAT
	TILERPT_LAST	23, 14, 3

	db 5|HORZ_REPEAT
	TILERPT_LAST	3, 9, 24

	db 1|VERT_REPEAT
	TILERPT_LAST	11, 5, 4

	db #ff
	DISPLAY "The Sixteenth Cavern (unpacked) size: ", /A, $-The_Sixteenth_Cavern

The_Warehouse:
	db 0 ; last in list
	db 5|(4<<4)	; number of keys/key paper

	IF !PACKED
	XY 2, 3+16 ; willy start pos/facing
	db 29, 1 ; exit position
	IPB 4, 1, 1 ; exit colour
	db 2 ; border colour
	db 0 ; paper colour
	db "Roger Penrose", TOKEN_APOSTROPHE, "s Mathematical Warehouse"C
	ENDIF

	EXITANDGUARDIANS gfx_exit16, TRUE, TRUE

	VGUARDIANTYPE gfx_penrosetriangle0

	IPB			1, 0, 1
	VGUARDIAN	3, 64, 0
	db 64, 102
	VGUARDIANVELFLAGS 2, 0

	IPB			6, 0, 0
	VGUARDIAN	10, 64, 1
	db 3, 96
	VGUARDIANVELFLAGS -3, 0

	IPB			7, 0, 1
	VGUARDIAN	19, 48, 2
	db 0, 64
	VGUARDIANVELFLAGS 1, 0

	IPB_LAST	3, 0, 1
	VGUARDIAN	27, 0, 3
	db 4, 96
	VGUARDIANVELFLAGS 4, 0

	HGUARDIANTYPE gfx_warehouse0
	IPB				2, 0, 1
	HG4FRAME_XY_FLAGS 5, 13, 0, TRUE
	db 5, 8
	IPB_LAST		5, 0, 0
	HG4FRAME_XY_FLAGS 12, 13, 0, FALSE
	db 12, 25

	; keytype
	keytype gfx_key0

	IF !PACKED
	XY 24, 5
	XY 15, 7
	XY 1, 9
	XY 19, 10
	XY 26, 11
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 3, 5, 0, 6, 7

	; cell graphics
	IPB 6, 2, 0
	celltype gfx_wall9		; tile1
	IPB 4, 0, 0
	celltype gfx_platform0	; tile2
	IPB 6, 0, 0
	celltype gfx_spiky0		; tile3
	IPB 4, 1, 0
	celltype gfx_spiky13	; tile4
	IPB 4, 0, 1
	celltype gfx_crumbly3	; tile5
	IPB_LAST 0, 4, 0
	celltype gfx_conveyor6	; tile6

	db 3
	TILE		6, 4
	TILE		9, 4
	TILE		13, 4
	TILE		16, 4
	TILE		22, 4
	TILELAST	24, 4

	db 2|HORZ_REPEAT
	TILERPT			1, 15, 30
	TILERPT_LAST	27, 14, 4			

	db 1|HORZ_REPEAT
	TILERPT_LAST	29, 0, 2			

	db 5|HORZ_REPEAT|VERT_REPEAT
	TILEHVRPT_LAST	1, 5, 30, 8

	db 2|HORZ_REPEAT
	TILERPT			1, 5, 2
	TILERPT_LAST	29, 5, 2

	; don't need these if vert guardians carve their own paths
	db 0|HORZ_REPEAT|VERT_REPEAT
	TILEHVRPT		3, 8, 2, 5
	TILEHVRPT		10, 5, 2, 8
	TILEHVRPT		19, 5, 2, 5
	TILEHVRPT_LAST	27, 5, 2, 8

	db 6|HORZ_REPEAT
	TILERPT_LAST	14, 8, 5

	db 4
	TILE		3, 6
	TILE		26, 7
	TILE		22, 10
	TILELAST	9, 11

	db #ff
	DISPLAY "The Warehouse (unpacked) size: ", /A, $-The_Warehouse
	
Amoebatrons_Revenge:	
	db 0 ; last in list
	db 1	; number of keys

	IF !PACKED
	XY 30, 13+16 ; willy start pos/facing
	db 29, 0 ; exit position
	IPB 6, 1, 0 ; exit colour
	db 1 ; border colour
	db 0 ; paper colour
	db "Amoebatrons", TOKEN_APOSTROPHE, " Revenge"C
	ENDIF

	EXITANDGUARDIANS gfx_exit17, TRUE, TRUE

	VGUARDIANTYPE gfx_amoeba_b0
	IPB					3, 0, 1
	VGUARDIAN 5, 8, 0
	db 5, 104
	VGUARDIANVELFLAGS 3, 0 ; speed and flags

	IPB					4, 0, 0
	VGUARDIAN 10, 8, 1
	db 5, 104
	VGUARDIANVELFLAGS 2, 0 ; speed and flags

	IPB					5, 0, 0
	VGUARDIAN 20, 8, 2
	db 5, 104
	VGUARDIANVELFLAGS 4, 0 ; speed and flags

	IF 0
	IPB					3, 0, 1
	VGUARDIAN 7, 8, 0
	db 5, 104
	VGUARDIANVELFLAGS 3, 0 ; speed and flags

	IPB					4, 0, 0
	VGUARDIAN 12, 8, 1
	db 5, 104
	VGUARDIANVELFLAGS 2, 0 ; speed and flags

	IPB					5, 0, 0
	VGUARDIAN 22, 8, 2
	db 5, 104
	VGUARDIANVELFLAGS 4, 0 ; speed and flags

	IPB			6, 0, 1
	VGUARDIAN 27, 8, 3
	db 5, 104
	VGUARDIANVELFLAGS 1, 0 ; speed and flags
	ENDIF

	IPB_LAST			6, 0, 1
	IF BEAMFLICKERTEST
	VGUARDIAN 25, 0, 3
	ELSE
	VGUARDIAN 25, 8, 3
	ENDIF
	db 5, 104
	IF BEAMFLICKERTEST
	VGUARDIANVELFLAGS 0, 0 ; speed and flags
	ELSE
	VGUARDIANVELFLAGS 1, 0 ; speed and flags
	ENDIF

	HGUARDIANTYPE gfx_pole0
	IPB				4, 0, 1
	HG4FRAME_XY_FLAGS 12, 3, 0, TRUE
	db 12, 18
	IPB				5, 0, 1
	HG4FRAME_XY_FLAGS 16, 10, 0, TRUE
	db 12, 17
	IPB				3, 0, 1
	HG4FRAME_XY_FLAGS 16, 6, 0, FALSE
	db 12, 17
	IPB_LAST		6, 0, 0
	HG4FRAME_XY_FLAGS 16, 13, 7, FALSE
	db 12, 18

	; keys. Can obvs compress these better (9bits/xy pos)
	keytype gfx_key0
	IF !PACKED
	XY 16, 1
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 0, 0, 0, 0, 3

	; cell graphics
	IPB			6, 2, 0
	celltype gfx_wall10
	IPB_LAST	2, 0, 1
	celltype gfx_platform0

	IF BEAMFLICKERTEST
	db EVENT_SOLAR
	db 25 ; start position
	IPB 7, 0, 0 ; background colour
	IPB 7, 6, 1 ; beam colour
	ENDIF

	; single blocks
	db 1 ; brush and flags
	TILELAST	28, 0

	db 2|HORZ_REPEAT
	; repeat blocks
	TILERPT			0, 15, 32
	TILERPT			3, 5, 2
	TILERPT			7, 5, 3
	TILERPT			12, 5, 8
	TILERPT			22, 5, 3
	TILERPT			27, 5, 4
	TILERPT			1, 7, 2
	TILERPT			12, 8, 8
	TILERPT			22, 8, 3
	TILERPT			27, 8, 2
	TILERPT			3, 9, 2
	TILERPT			7, 9, 3
	TILERPT			29, 10, 2
	TILERPT			3, 12, 2
	TILERPT			7, 12, 3
	TILERPT			12, 12, 8
	TILERPT			22, 12, 3
	TILERPT			27, 12, 2
	TILERPT_LAST	1, 13, 2

	db #FF ; end of level data
	DISPLAY "Amoebatrons' Revenge (unpacked) size: ", /A, $-Amoebatrons_Revenge

Solar_Power_Generator:
	db 0 ; last in list
	db 3|(4<<4)	; number of keys/paper colour

	IF !PACKED
	XY 14, 10 ; willy start pos/facing
	db 1, 1 ; exit position
	IPB 6, 1, 1 ; exit colour
	db 3 ; border colour
	db 4 ; paper colour
	db "Solar Power Generator"C
	ENDIF

	EXITANDGUARDIANS gfx_exit18, TRUE, TRUE

	VGUARDIANTYPE gfx_prism0

	IPB			6, 4, 0
	VGUARDIAN	5, 64, 0
	db 2, 102
	VGUARDIANVELFLAGS 3, 0

	IPB			2, 4, 0
	VGUARDIAN	11, 56, 1
	db 48, 102
	VGUARDIANVELFLAGS -2, 0

	IPB_LAST	1, 4, 0
	VGUARDIAN	16, 80, 2
	db 4, 80
	VGUARDIANVELFLAGS 1, 0

	HGUARDIANTYPE gfx_reflector0
	IPB				6, 4, 0
	HG4FRAME_XY_FLAGS 24, 3, 0, FALSE
	db 23, 29
	IPB				1, 4, 0
	HG4FRAME_XY_FLAGS 28, 6, 0, FALSE
	db 22, 29
	IPB				2, 4, 0
	HG4FRAME_XY_FLAGS 29, 9, 7, TRUE
	db 23, 29
	IPB_LAST		6, 4, 0
	HG4FRAME_XY_FLAGS 16, 13, 0, FALSE
	db 13, 29

	; keytype
	keytype gfx_key0

	IF !PACKED
	XY 30, 1
	XY 1, 5
	XY 30, 12
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 0, 0, 3, 0, 4

	IPB 6, 2, 0
	celltype gfx_wall0		; tile1
	IPB 0, 4, 0
	celltype gfx_platform0	; tile2
	IPB_LAST 6, 4, 0
	celltype gfx_conveyor0	; tile3

	db EVENT_SOLAR
	db 23 ; start position
	IPB 7, 4, 0 ; background colour
	IPB 7, 6, 1 ; beam colour

	db 2|HORZ_REPEAT
	TILERPT			3, 15, 28
	TILERPT			3, 5, 2
	TILERPT			9, 5, 6
	TILERPT			24, 5, 7
	TILERPT			19, 7, 3
	TILERPT			1, 8, 2
	TILERPT			13, 8, 3
	TILERPT			24, 8, 7
	TILERPT			19, 10, 2
	TILERPT			1, 11, 4
	TILERPT			24, 11, 7
	TILERPT_LAST	14, 12, 5

	IF 0
	db 1|HORZ_REPEAT
	TILERPT			1, 0, 2
	TILERPT			1, 14, 2
	TILERPT_LAST	1, 15, 2
	ELSE
	db 1|HORZ_REPEAT
	TILERPT_LAST	1, 0, 2
	db 1|HORZ_REPEAT|VERT_REPEAT
	TILEHVRPT_LAST	1, 14, 2, 2
	ENDIF

	db 3|HORZ_REPEAT
	TILERPT_LAST	7, 12, 4

	; single blocks
	db 1
	TILELAST		23, 15

	db #FF ; end of level data
	DISPLAY "Solar Power Generator (unpacked) size: ", /A, $-Solar_Power_Generator

The_Final_Barrier:
	db 0 ; last in list
	db 5 ; num keys, bright<<7, paper<<4 for key

	IF !PACKED
	XY 26, 13+16 ; willy start pos/facing
	db 19, 5 ; exit position
	IPB 6, 3, 1 ; exit colour
	db 2 ; border colour
	db 0 ; paper colour
	db TOKEN_LEADINGTHE, "Final Barrier", TOKEN_QUESTIONMARK|128
	ENDIF

	EXITANDGUARDIANS gfx_exit19, TRUE, TRUE

	VGUARDIANTYPE gfx_eye0

	IPB_LAST	7, 0, 0
	VGUARDIAN	24, 48, 0
	db 40, 103
	VGUARDIANVELFLAGS 1, 0

	HGUARDIANTYPE gfx_thresher0
	IPB_LAST		6, 0, 1
	HG4FRAME_XY_FLAGS 7, 13, 0, FALSE
	db 7, 22

	
	; keytype
	keytype gfx_key0

	IF !PACKED
	XY 23, 5
	XY 30, 6
	XY 10, 11
	XY 14, 11
	XY 19, 11
	ENDIF

	;TILEDIST	_platforms_idx_, _spikies_idx_, _crumblies_idx_, _conveyorl_idx_, _conveyorr_idx_, _bg_idx_
	TILEDIST	2, 3, 4, 0, 5, 6

	; cell graphics
	IPB 6, 4, 0
	celltype gfx_wall0		; tile1
	IPB 2, 0, 1
	celltype gfx_platform0	; tile2
	IPB 4, 0, 1
	celltype gfx_spiky11	; tile3
	IPB 2, 0, 0
	celltype gfx_crumbly0	; tile4
	IPB_LAST 5, 0, 0
	celltype gfx_conveyor0	; tile5

	db 3
	TILE		9, 11
	TILE		12, 11
	TILE		17, 11
	TILELAST	21, 11

	db 2
	TILELAST	28, 11

	db 4
	TILELAST	26, 10

	db 2|HORZ_REPEAT
	TILERPT			1, 15, 30
	TILERPT			29, 8, 2
	TILERPT			1, 12, 2
	TILERPT_LAST	5, 13, 2

	db 1|HORZ_REPEAT
	TILERPT			18, 4, 13
	TILERPT_LAST	1, 7, 18

	db 5|HORZ_REPEAT
	TILERPT_LAST	1, 10, 22

	db 1|VERT_REPEAT
	TILERPT			18, 5, 2
	TILERPT_LAST	21, 5, 3

	db EVENT_GFXCOPY
	db Final_Barrier_ImageData/256
	dw SCRBASE
	dw 2048
	db (Final_Barrier_ImageData+2048)/256
	dw ATTRIBS
	dw 256

	db #ff
	DISPLAY "The Final Barrier (unpacked) size: ", /A, $-The_Final_Barrier

	db #ff ; level list terminator

	DISPLAY "Unpacked levels size: ", /A, $-Central_Cavern
	DISPLAY "End of code and data: ", /A, $

	DEVICE ZXSPECTRUM48

	EMPTYTAP "slickwilly.tap"
    SAVETAP "slickwilly.tap", CODE, "splash", collision_map, 224
    SAVETAP "slickwilly.tap", CODE, "code", main, $-main
