Slick Willy Rolling Demo code WIP

For ZX Spectrum 48K

Compile with sjasmplus v1.20.3

sjasmplus.exe --sym=out.sym --syntax=f --raw=out.bin swedit.asm

To run on a speccy emulator:

CLEAR 24575

<upload out.bin to address 24576 in emulator>

RANDOMIZE USR 32768

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
