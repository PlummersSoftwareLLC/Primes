/*
C64 1 million prime sieve
==============================================
Rasmus Wernersson, AKA. Raz/Camelot
Summer vacation project, 2023 (core algo dev)
Wrapped up, polished, documented, April 2026
==============================================

==> IMPORTANT: Please read "README.md" first. It contains a detailed walk-through 
    of the problem solving steps, theoretical/technical considerations for the
    C64 implementation, complexity analysis and follow-up optimizations.
    
    The comments here will mostly be about the actual code.


Inspired by Dave's Garage PrimeSieve implementation challenge/project
(originally: implement the exact same algorithm in as many langues as possible
and benchmark the implementatins agaist eachother on standard hardware).

The basic algorith is the "Sieve of Eratosthenes":
https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

Works in two steps:
1) Caluclate all primes below 1,000,000 as fast as possible using a bit-field 
   to keep track of results
2) Validate (count primes)

C64 notes:
In the original benchmark study, the number of completions of both steps with-in
5 secs is counted. Here we just keep track of the time spend, and allow the user
to relaunch the calculation as many time as wanted.

A bit field of length 1M would require 125.000 bytes. By using the trick of only
evaluating the odd numbers (and treating "2" as a special case) we can reduced 
the bitfield to 62.500 bytes ($f424).

This means the we run in i=0..499999 coordinate system, where each i translate 
to (2*i)+1 in nomal space.

IMPORTANT: In this implementation I utilize the fact that I know the following
facts up front: 
1) That 1,000,000 is the max (and fixed) upper bound. I do calculate
   sqroot(1,000,000) as in the original implementation for completeness,
   but the bitfield size and code optimizations are fixed.
2) That I need to iterate through the numbers 1..499 (in odd-space) which
   will fit into a 9-bit numbers. This allows for the trick of having the
   core routine run in two modes: with "0" as the implicit 9-bit and with
   "1" as the implicit 9-bit.

Furthermore, since I'll never need to calculate n^2 beyond 500 I can get a
speed up from pre-calculating lookup tables, set up in the following way:

All positions of the bitfield can be covered by a 19-bit number (odd space).
Since the start of the bitfield is byte aligned, we can always view the
lower 3 bits as the bit-position within a specific byte.

I pack the lower 3 bytes into a table of its own, and the higher 16-bits
(bit 4-19) are conveniently the byte offset from the start of the bitfield.

Finally, the concrete byte address of baked into the 16-bit offset and I
generated high + low byte tables for quick look-up.

*/

// ===== FLAGS =====
//#define BATCHMODE

// =====  CONSTs   ====

// Ordinary ZP consts ($02-$0f)
.const zp1     = $02
.const zp2     = $04
.const zp_tmp1 = $06
.const zp_tmp2 = $07

// Counting primes + sqroot: ad-hoc 24 bit accumulator
.const zp_a1 = $08
.const zp_a2 = $09
.const zp_a3 = $0a

.const zp_rootL = $0b
.const zp_rootH = $0c
.const zp_remL  = $0d
.const zp_remH  = $0e

// BIT FIELD definitions
.const bitfield_len   = $f424                 // 500.000 / 8 = 62.500 bytes
.const bitfield_start = $10000-bitfield_len   // $0bdc   => $0bdc..$ffff (yes, including $fffe and $ffff)

// LAZY init definitions
.const lazy_init_len = 1*3*5*7                           // 105 bytes - buffer length required to capture repeating pattern
.const lz_num_blocks = round(bitfield_len/lazy_init_len) // # 595 - complete 105 blocks in bitfield
.const lz_last_block = bitfield_start+((lz_num_blocks-1)*lazy_init_len)

// STATUS line location
.const status_line_loc = $0800+(24*40)        // line starts at $0bc0 - we can use $0bc0..$0bdb (28 chars)
.const status_line_col = $d000+status_line_loc
.const status_line_len = bitfield_start - status_line_loc

// MAX mem (incl)
.const MAX_MEM = status_line_loc-1 // $0bbf

// N squared + other page long tables - final placement after relocation:
.const tab_nsquared_low   = $0800  // 2 pages. Is also shows on screen, and pos 0 can be hijacked as indicator
.const tab_nsquared_high  = $0200  // 2 pages.
.const tab_nsquared_3bits = $0400  // 1 page. Reason: Ends up repeating the pattern $00,$04,$04,$00 forever. We don't need multiple pages of this.
.const tab_num_bits_set   = $0500  // 1 page.
.const tab_mask8_pagelong = $0600  // 1 page.
.const tab_div8_pagelong  = $0700  // 1 page.

// Samller helper tables - final placement:
.const tab_hex2char       = $01e0  // 16 bytes
.const tab_frac_to_patt   = tab_nsquared_low+$0200-$08 // 8 bytes - in unused space at 
                                                       // the end of the n^2 tables

// ======= SEGMENTS set up =======
.file [name="%o.prg", segments="Upstart,Code,Data,StatusLine,CodeInit,DataReloc,DataShowOnce"]

.segmentdef Upstart    [min=$0801]
.segmentdef Code       [start=$0a00, max=MAX_MEM]
.segmentdef Data       [startAfter="Code", max=MAX_MEM]
.segmentdef StatusLine [start=status_line_loc, max=bitfield_start-1]
.segmentdef CodeInit   [start = $0c00]
.segmentdef DataReloc  [startAfter="CodeInit", align=$0100]
.segmentdef DataShowOnce [startAfter="DataReloc",align=$0100]

// ========

.segment Upstart
		*=$0801 "Basic"
		BasicUpstart(showsplashscreen)
.segment StatusLine
status_line: // (32 bytes max)
.text "m:01 s:x p:$000000 t:00:00.0"
//     0123456789012345678901234567

#if BATCHMODE
.segment Code "Batch run hook for data dump"
batchrunhook:
		lda $0800
		adc #$08
		sta $0800
		jmp batchrunhook
#endif

.segment Code "Main control loop"
main:
		jsr SystemSetup
		// All IRQs + NMIs are blocked, and we have the machine for ourselves now...
		// .. set stack point to $ff -> we only use a few bytes at the top of the stack, so
		// a lot of page 1 can be used for code
		ldx #$ff
		txs        // we current don't use stack below $01fa (3 levels jsr)
		jsr trans_low_code
		jsr relocateTables
//		jsr setup_colors
restart:		
		jsr reset_and_start_clock
		jsr readout_clock
		
		lda #'i'
		sta status_line+07
		jsr refresh_colors
		jsr run_sqroot
				
mode_shift:
		bit normal_flow		// gets changed to jmp if mode 2 is selected
//		jmp normal_flow

		// Lazy init - run + prep main loop
		jsr lazy_init_and_main_loop_setup
		jmp !skip+
normal_flow:
		jsr clearbitfield_zp
!skip:
		lda #'c'
		sta status_line+07
		jsr readout_clock
		jsr main_prime_sieve_loop
		jsr setup_for_500_loop
//		jsr readout_clock
		jsr main_prime_sieve_loop
		lda #'v'
		sta status_line+07
		jsr readout_clock
		jsr count_primes      // also prints prime count
		jsr check_prime_count // also sets status
		jsr readout_clock
		jsr setup_for_256_loop
		
#if BATCHMODE
		jmp batchrunhook
#else
		lda #$35
		sta $01

!loop:
		lda $dc01
		cmp #$ef    // check for space ... in a bit primitive fashion
		bne !loop-		
		// no need to set $01 - "reset_and_start_clock"	do that as a 
		//  side effect
		// lda #$30; sta $01	
		jmp restart		
#endif

// -------------------------------------------------
.segment CodeInit "Setup init/setup"
SystemSetup:
		sei
		lda #$7f   // Disable timer interrupts
		sta $dc0d
		sta $dd0d
		lda $dc0d  // Clear pending timer interrupts
		lda $dd0d
		inc $d019  // ACK any pending $d012 IRQs
		
		lda #$00
		sta $d020
		sta $d021

		jsr calibrateTODclock // Calibrate the TOD clock before we hide the IO area

		lda #%00100100  // SCR at $0800, CHAR at $1000 (ROM mirror image)
		sta $d018

		lda #$30   // Disable everything + expose ram under $d000-$dfff
		sta $01
		rts
		
		
.segment CodeInit "ZP code before relocation"
// ======  ZP embedded code starts here =====
zpCode: .segmentout [segments="ZeroPage_Code"]
.label zpCodeSize = *-zpCode
// Zeropage code
//.segment ZeroPage_Code [start=$10,max=$ff]
.segment ZeroPage_Code [start=$10,max=$ff]

// ##################################################################
// Inner and outer loop. MUST be in ZP! 
// ... don't change order of inner and outer - technically it will 
//     work, but it's so much easier to inspect/debug with 
//     inner loop starting at $0010
// ##################################################################


.segment ZeroPage_Code "Inner loop"
zpStart:
zpBittPatt_XYsave:
		// Bit-pattern in A on entry. zpp + add low/high must be directly set beforehand
		stx.z savex+1
		tax
		sty.z savey+1
		ldy #$00
		clc
zpBitPatt:
!loop:
		lda (zpp_b+1),y  // 5c (y==0, no page crossing)
.label zpp = *+1
zpp_b:
		sax $ffff        // 4c
		lda zpp          // 3c
zpadd_low:
		adc #$00         // 2c
		sta zpp          // 3c

                         // ... optimize for add_high == 0 (BCC replaced with NOP_IMM when add_high > 0)
                         //     95+% of the time is spent in loops with add_high == 0.
zp_bcc:
		bcc !loop-       // 3/2c .. also 2c when NOP_IMM gets stored in
		lda zpp+1
zpadd_high:
		adc #$00
		sta zpp+1
		bcc !loop-
		// rts   	// add RTS here to revert to bare-bones version (entry via zpBitPatt)
savex:
		ldx #$ff
savey:
		ldy #$ff
		rts

.segment ZeroPage_Code "Outer loop"
main_prime_sieve_loop:
//test_256:
start_x:
		ldx #$01   // Start at $01 for the 00..255 loop to avoid setting the color at $d800
		           // (the math works fine with starting at $00 since the first byte of the bitfield
		           // is set to %01111111).		 
!loop:
/* ===> version of bitfield look-up not using long tables
		txa
		and #$07
		tay
		lda tab_mask8_small,y
		sta zp_tmp1
		
		txa
		lsr
		lsr
		lsr
		tay

read_bitfield:
		lda bitfield_start,y
		and zp_tmp1
		beq !next+
 <=== */		

        // STEP 1: Read bitfield to see if this number is prime
		ldy tab_div8_pagelong,x
read_bitfield:
		lda bitfield_start,y
 		and tab_mask8_pagelong,x
 		beq !next+               // not prime .. move on
 		
 		
 		// STEP 2: prepare inner loop for bit-pattern roll-put
 		//
		// Calc bit-distance for the pattern to roll out...
		// ... since we work in 8 steps, it will also be the
		// byte distance for each interation.
		// The calculation is (x*2)+1 (conversion from odd-space to norm-space)

		// CALC: (x*2)+1 low-byte
		txa
		sec // rolls into bit 0 => works as +1
		rol // *2
		sta zpadd_low+1

		and #$07
		sta zp_tmp1 // Small side step, calc lower 3 bits for later ...
		            // -> saves some time to do not, and does not affect carry

rol_init_val:
		// CALC: (x*2)+1 high-byte
		lda #$00     // We sneak in a set bit 9 ($01 val) here when we're in the loop of 256..499
		rol          // *2 ... bit 8+9 (8 from X, 9 injected)
		sta zpadd_high+1
		
		beq !skip+   // Check if high-byte add in inner loop is still #$00 ..
		lda #NOP_IMM // Opcode for NOP #xx (undoc opcode)
		sta zp_bcc   // .. otherwise elimate the early BCC (will always cost two cycles)
!skip:		
		txa
		lsr
		lsr          // quick way of doing >>3 of the 16 bit value
bit9x:
		ora #%00000000  // ... if we're on loop 256..499 we'll need to set bit 6 (%01000000) corresponding to the injected bit 9
		sta.z add_direct+1
		
		lda #$01
		sta zp_tmp2  // for counting to 8 by shifting
		
		// ** Save values to zp2 - zpp gets altered **
read_nsquard_low:
		lda tab_nsquared_low,x  // will point to page 0 or page 1 of table
		sta zp2
		sta zpp
read_nsquard_high:
		lda tab_nsquared_high,x // will point to page 0 or page 1 of table
		sta zp2+1
		sta zpp+1
		ldy tab_nsquared_3bits,x // Contains the repeating pattern $00,$04,$04,$00
		                         // does not need to change between the two loops

!inner_loop:
		jsr flash_col
		lda tab_frac_to_patt,y   // 3-bit to pattern
		jsr zpBittPatt_XYsave    // Perform ACTUAL inner loop / bit roll-out
		asl zp_tmp2              // 8-step counter
		bcs !skip+               // ... this was loop #8 -> exit
		tya
		//clc         // not needed asl has shiftet 0 into carry if we're here
		adc zp_tmp1   // add lower 3-bit step factor
		cmp #$08      // set carry if bit 4 is set -> used for adc#00 in a sec
		and #$07      // get back to 3-bit 
		tay
		lda zp2
add_direct:
		adc #$00      // add byte step + carry (bf low-byte pointer)
		sta zp2
		sta zpp
		lda zp2+1     // update bf high-byte pointer
		adc #$00
		sta zp2+1
		sta zpp+1
		bcc !inner_loop-
!skip:
		jsr readout_clock // does not change X
		lda #$01		  
!next:
		ldy #$35
		sty $01
col_store_done:
		sta $d800,x       // done - set color to white ($d800,x or $d900,x)
		ldy #$30
		sty $01
		
		inx
max_x:
		cpx #$00
		bne !loop-
		rts

.segment ZeroPage_Code "Clear bitfield"
clearbitfield_zp:
		// 1 is not prime - add that in as a fact
		lda #%01111111
		sta bitfield_start

		// Mark everything else as potential primes...
		lda #>(bitfield_start+1)
		sta.z storebf+2
		ldy #<(bitfield_start+1)
		lda #$ff
!loop:
storebf:
		sta $ff00,y
		iny
		bne !loop-
		inc storebf+2
		bne !loop-
		rts

.segment ZeroPage_Code "Copy lazy init blocks"
copy_lz_blocks:
copy_lz_blocks_DOWN:
		clc 
		lda #<(lz_last_block-lazy_init_len)
		sta.z dst_store+1
		lda #>(lz_last_block-lazy_init_len)
		sta.z dst_store+2
!loop0:
		ldy #(lazy_init_len-1)  // bit 7 is clear ....  (len=105)
!loop1:
		lda lz_last_block,y
dst_store:
		sta $ffff,y
		dey
		bpl !loop1-             // ... therefore we can do this trick

		lda dst_store+1
		sec
		sbc #lazy_init_len
		sta dst_store+1
		lda dst_store+2
		sbc #$00
		sta dst_store+2
		sta $0800            // Visuals
		cmp #>(bitfield_start-lazy_init_len)
		bne !loop0-
		lda dst_store+1
		cmp #<(bitfield_start-lazy_init_len)
		bne !loop0-
		rts


// April 2026: Seemlessly morph into page 1.		
// ... caution: transfer code excepts this segment to be len < 256		
.segment CodeInit "Page 1 code before relocation"
// ======  ZP embedded code starts here =====
p1Code: .segmentout [segments="PageOne_Code"]
.label p1CodeSize = *-p1Code
// Page 1 code
//.segment PageOne_Code [start=$0100,max=$01e7]
.segment PageOne_Code [startAfter="ZeroPage_Code",max=$01df]
p1Start:

// This one will partly sit in ZP and partly in p1
.segment PageOne_Code "Flash color"
flash_col:
		lda #$35
		sta $01
col_store_flash1:
		lda $d800,x
		eor #$03
col_store_flash2:
		sta $d800,x
		lda #$30
		sta $01
		inc $0800
		rts

.segment PageOne_Code "Count primes"
count_primes:
		lda #>bitfield_start
		sta readbf+2
		lda #$00
		sta zp_a2
		sta zp_a3
add_to_count:
		lda #$01       // count +1 since 2 is prime and is not seen in the bitfield.
//		lda #$00       // debug - triggers ERROR stage when checking the prime count later
		ldy #<bitfield_start
		clc
!loop:
readbf:
		ldx $ff00,y    // Run this page aligned - read will always be 4 cycles
		adc tab_num_bits_set,x
		bcc !next+

		// Preserve A, clear carry manually and use INC for bits 8-15 and 16-23.
		clc
		dec $0800      // visuals - must be before the INC below
		inc zp_a2
		bne !next+
		inc zp_a3
!next:
		iny
		bne !loop-
		// ==== visuals - looks cool, but costs cycles.
		sta zp_a1
		jsr print_num_primes
		lda zp_a1
		clc        // important - carry is altered in the subrouting 
		ldy #$00
		// ==== done with visuals. This block can be taken out without breaking the function.
		inc readbf+2
		bne !loop-
!exit:
		sta zp_a1
		rts

//.segment Code "Flip ZP code to 256..499 mode"
.segment PageOne_Code "Setup for 500 loop"
// Setup outer loop for running the interval 256..500
setup_for_500_loop:

		// $d900 as base for color coding
		lda #$d9
		sta col_store_done+2
		sta.z col_store_flash1+2
		sta.z col_store_flash2+2
		
//		lda #$00
//		sta start_x+1
		ldx #$00
		stx start_x+1
		lda #(500-256)
		sta max_x+1
		
		lda #<(bitfield_start+32) // still with-in the same page ($0bdc+$0020)
		sta read_bitfield+1
		
//		lda #$01
		inx // x = 1
		stx rol_init_val+1
		lda #%01000000
		sta bit9x+1
		
		// Use page 1 of n^2 tables
		lda #>(tab_nsquared_low+$0100)
		sta read_nsquard_low+2
		lda #>(tab_nsquared_high+$0100)
		sta read_nsquard_high+2
		rts

//.segment Code "Reset ZP code to 01..255 mode"
.segment PageOne_Code "Setup for 256 loop"

// (re)set outer loop for running the interval 1..255
setup_for_256_loop:

		// $d800 as base for color coding
		lda #$d8
		sta col_store_done+2
		sta.z col_store_flash1+2
		sta.z col_store_flash2+2
		
//		lda #$01
//		sta start_x+1
//		lda #$00
//		sta max_x+1
		ldx #$01
		stx start_x+1
		dex 
		stx max_x+1  // x = 0
		
		lda #<(bitfield_start)
		sta read_bitfield+1
		
//		lda #$00
//		sta rol_init_val+1
//		lda #%00000000
//		sta bit9x+1
		stx rol_init_val+1  // x = 0
		stx bit9x+1         // x = %00000000

		// Use page 0 of n^2 tables
		lda #>(tab_nsquared_low+$0000)
		sta read_nsquard_low+2
		lda #>(tab_nsquared_high+$0000)
		sta read_nsquard_high+2
		
		// (re)establish early exit BCC in inner loop
		lda #BCC_REL
		sta zp_bcc
		rts

.segment PageOne_Code "Lazy init"
lazy_init_and_main_loop_setup:
		lda #$ff
		ldx #<lz_last_block
!loop:
		sta $ff00,x
		inx
		bne !loop-
		
lz_init_first4:
		lax #$00; tay      // a=x=y=0 ... saves a bytes compared to ldx + ldy
		jsr lz_set_tables

		stx max_x+1 // 4
		lda #%01111111
		sta bitfield_start
		jsr main_prime_sieve_loop

		// Setup main loop to start at index 4
		ldy #$04; sty.z start_x+1
		ldx #$00; stx.z max_x+1
		
		// reestablish n^2 tables (needs x=0, y=4)
		jsr lz_set_tables
		
		lda bitfield_start
		and #%11110000     
		sta zp_tmp1
		jsr copy_lz_blocks
		
		lda bitfield_start 
		and #%00001111     
		ora zp_tmp1
		sta bitfield_start
		
		rts

.segment PageOne_Code "Set lazy init tables"
lz_set_tables:
!loop:
		lda lz_linear_low,y
		sta tab_nsquared_low,x
		lda lz_linear_high,y
		sta tab_nsquared_high,x
		lda lz_linear_3bits,y
		sta tab_nsquared_3bits,x
		
		iny
		inx
		cpx #$04
		bcc !loop-
		rts

// ##################################################################
// Counting + validating primes
// ##################################################################
/*
Original check table:
    primeCounts = { 10 : 4,                 # Historical data for validating our results - the number of primes
                    100 : 25,               # to be found under some limit, such as 168 primes under 1000
                    1000 : 168,
                    10000 : 1229,
                    100000 : 9592,
                    1000000 : 78498,
                    10000000 : 664579,
                    100000000 : 5761455
                  }
=> There are 78498 primes below 1.000.000 ($0132a2)
*/
.segment Code "Check prime count"
check_prime_count:
		lda #$35
		sta $01

		lda zp_a1
		cmp #$a2
		bne !error+
		lda zp_a2
		cmp #$32
		bne !error+
		lda zp_a3
		cmp #$01
		bne !error+
		
		// All checks out
		lda #'*'
		sta status_line+7
		lda #$01
		sta status_line_col+7
		bne !col_count+ // jmp always taken
!error:
		lda #'%'
		sta status_line+7
		lda #$02
		sta status_line_col+7
!col_count:
		ldx #11
!loop:
		sta status_line_col,x
		inx
		cpx #18
		bcc !loop-
		
		lda #$30
		sta $01
		rts

// "r:01 s:x p:$000000 t:00:00.0"
//  0123456789012345678901234567
.segment Code "Print number of primes"
// We only need to print 20 bits
print_num_primes:
.for (var i=0;i<3;i++){
		lax zp_a1+i
		and #$0f
		tay
		lda tab_hex2char,y
		sta status_line+17-(i*2)
		.if (i<2) {
//		txa
//		lsr; lsr; lsr; lsr
		lda tab_div8_pagelong,x
		lsr
		tay
		lda tab_hex2char,y
		sta status_line+16-(i*2)
		}
}
		rts

// ##################################################################
// Various bits of run-once init / setup code
// ##################################################################

.segment CodeInit "Setup colors"
setup_colors:
		ldx $01
		lda #$35
		sta $01

		ldy #$00
!loop:
		lda #$07
		sta $d800,y
		sta $d800+250,y
		//lda #$0b
		lda #$00
		sta $d800+500,y
		sta $d800+750,y
		iny
		cpy #250
		bcc !loop-
		
		lda #%00100100  // SCR at $0800, CHAR at $1000 (ROM mirror image)
		sta $d018

		ldy #$00
		lda #$05
!loop:
		sta $d800+(24*40),y
		iny
		cpy #status_line_len
		bcc !loop-

		stx $01
		rts

.segment Code "Refresh colors"
refresh_colors:
		ldx $01
		lda #$35
		sta $01
		
		ldy #$00
		lda #$07
!loop:
		sta $d800,y
		sta $d800+250,y
		iny
		cpy #250
		bcc !loop-

		lda #$05
		sta $d800       // 1 is not prime and is used for progress indication

		ldy #$00
//		lda #$05
!loop:
		sta $d800+(24*40),y
		iny
		cpy #status_line_len
		bcc !loop-

		stx $01
		rts

.segment CodeInit "ZP + page 1 transfer code"
trans_low_code:
		ldx #0
!loop:
		lda zpCode,x
		sta zpStart,x
		inx
		cpx #zpCodeSize
		bne !loop-

		lda #<p1Start
		sta p1_16bit+1
		lda #>p1Start
		sta p1_16bit+2
		ldx #0
!loop:
		lda p1Code,x
p1_16bit:
		sta $0100,x      // Will cross page 0/1 boundary - thus needs to be 16bit
//		sta.a p1Start,x  // Will cross page 0/1 boundary (works but gives a warning)
		inx
		cpx #p1CodeSize
		bne !loop-

		// Option: fake an error by forgetting that "2" is prime
		lda fake_an_error
		beq !exit+
		lda #$00
		sta add_to_count+1
!exit:
		rts
fake_an_error: .byte $00

.segment CodeInit "Relocate tables"
relocateTables:
		ldx #$00
!loop:
		lda rtab_nsquared_low,x
		sta tab_nsquared_low,x
		lda rtab_nsquared_low+$0100,x
		sta tab_nsquared_low+$0100,x
		lda rtab_nsquared_high,x
		sta tab_nsquared_high,x
		lda rtab_nsquared_high+$0100,x
		sta tab_nsquared_high+$0100,x
		lda rtab_nsquared_3bits,x
		sta tab_nsquared_3bits,x  // Only 1 page needed due to the repeating $00,$04,$04,$00 pattern
		lda rtab_num_bits_set,x
		sta tab_num_bits_set,x
		lda rtab_mask8_pagelong,x
		sta tab_mask8_pagelong,x
		lda rtab_div8_pagelong,x
		sta tab_div8_pagelong,x
		
		inx
		bne !loop-


		// Transfer small tables as the last step
		// ... that way we can utilized a bit of the
		// left over space in the very end of page 2
		// of the n^2 tables
		ldx #$00
!loop:
		lda rtab_frac_to_patt,x
		sta tab_frac_to_patt,x
		inx
		cpx #$08
		bcc !loop-

/*
		ldx #$00
!loop:
		lda rtab_mask8_pagelong,x
		eor #$ff
		sta tab_frac_to_patt,x
		inx
		cpx #$08
		bcc !loop-
*/

		ldx #$00
!loop:
		lda rtab_hex2char,x
		sta tab_hex2char,x
		inx
		cpx #$10
		bcc !loop-
		rts

// ##################################################################
// Everything T-O-D CLOCK related
// ... calibration in run-once, rest in Code
// ##################################################################

.segment Code "Reset and start clock"
// Reset + start - assumes the clock has already been correctly calibrated.
reset_and_start_clock:
		lda #$35
		sta $01
		
		lda #$00
		sta $dc0b // TOD HR.    hours + tens of hours - write to $dc0b also stops the clock
		sta $dc0a // TOD MIN.   mins
		sta $dc09 // TOD SEC.   secs
		sta $dc08 // TOD 10THS. 1/10 of secs
		lda $dc08 // reading from $dc08 starts the clock
		
		lda #$30		
		sta $01		
		rts		

// "m:01 s:x p:$000000 t:00:00.0"
//  0123456789012345678901234567
.segment Code "Readout clock"
readout_clock:
		// Notice - preserve X to make it easier to call from our main ZP loop
		lda #$35
		sta $01
		
		lda $dc0b // stop clock
//		ldx $dc0a // TOD MIN - not needed, we finsh in appx 20 secs .. or less.
		ldy $dc09 // TOD SEC
		lda $dc08 // TOD 10THS - also starts the clock again
		
		// Values are in BCD format
		clc
		adc #'0'
		sta status_line+27  // 1/10 sec
		
		tya
		and #$0f
		adc #'0' // clc not needed here
		sta status_line+25
		tya
		lsr
		lsr
		lsr
		lsr
		clc
		adc #'0'
		sta status_line+24
		
		lda #$30
		sta $01
		rts

// Ensure main loop in the calibrate clock routine does not
//  cross a page boundary. (Putting the .align here is just a trick
//  to have the printed memmap list the correct start address of the
//  calibrate routine).
.segment CodeInit "Padding"
.align $0040
.segment CodeInit "Calibrate clock"		
calibrateTODclock:
// ======> This TOD calibation routing is modified from:  
// http://codebase64.org/doku.php?id=base:initialize_tod_clock_on_all_platforms
// All credits goes to the original author: Devia/Ancients
//
// Devia's orignal comments carried over... I've put in [R] where I had added my own comments
// [Raz] I've taken out the NMI handling code, since that is already taken care of our set-up.
// ======
//.align $0100            // [R] Added - Make sure loop below does not span a page boundary 
		lda	#0
		sta	$d011		// Turn off display to disable badlines
		sta	$dc0e		// Set TOD Clock Frequency to 60Hz
		sta	$dc0f		// Enable Set-TOD-Clock
		sta	$dc0b		// Set TOD-Clock to 0 (hours)
		sta	$dc0a		// - (minutes)
		sta	$dc09		// - (seconds)
		sta	$dc08		// - (deciseconds)
		
		lda	$dc08		// [R] Start clock
		cmp	$dc08		// [R] Wait for 1st decisecond tick 
		beq	*-3			
		
	
		ldx	#0			// Prep X and Y for 16 bit
		ldy	#0			// counter operation
		lda	$dc08		// Read deciseconds
!loop0:	inx				// 2   -+
		bne	!loop1+		// 2/3  | Do 16 bit count up on
		iny				// 2    | X(lo) and Y(hi) regs in a 
		jmp	!loop2+		// 3    | fixed cycle manner
!loop1:	nop				// 2    |
		nop				// 2   -+
!loop2:	cmp	$dc08		// 4 - Did 1 decisecond pass?
		beq	!loop0-		// 3 - If not, loop-di-doop
						// Each loop = 16 cycles
						// If less than 118230 cycles passed, TOD is 
						// clocked at 60Hz. If 118230 or more cycles
						// passed, TOD is clocked at 50Hz.
						// It might be a good idea to account for a bit
						// of slack and since every loop is 16 cycles,
						// 28*256 loops = 114688 cycles, which seems to be
						// acceptable. That means we need to check for
						// a Y value of 28.

		cpy	#28			// Did 114688 cycles or less go by?
		bcc	!skip+		// - Then we already have correct 60Hz $dc0e value
		lda	#$80		// Otherwise, we need to set it to 50Hz
		sta	$dc0e
!skip:
		lda	#$1b		// Enable the display again
		sta	$d011
		rts		

// ##################################################################
// Math
// ##################################################################
//
// =====================================================
// Fast + compact SQROOT on 24 bit input (12 bit output)
// =====================================================
// This is a straight forward extension of the 16 -> 8 bit 
// algorithm described in great details here:
// http://6502org.wikidot.com/software-math-sqrt
// 
// See also the comprehensive sqroot algorithm comparision at:
// https://github.com/TobyLobster/sqrt_test

.segment Code "sqroot 24 bit"
sqroot24:
		lda #$00
		sta zp_rootL
		sta zp_rootH
		sta zp_remL
		sta zp_remH
		ldx #12
!loop:
		sec
		lda zp_a3  // bit 16-23
		sbc #$40
		tay

		lda zp_remL
		sbc zp_rootL
		sta zp_tmp1
		lda zp_remH
		sbc zp_rootH
		bcc !skip+

		sty zp_a3  // bit 16-23
		sta zp_remH
		lda zp_tmp1
		sta zp_remL
!skip:
		rol zp_rootL
		rol zp_rootH
	
		asl zp_a1  // bit  0-7
		rol zp_a2  // bit  8-15
		rol zp_a3  // bit 16-23
		rol zp_remL
		rol zp_remH
	
		asl zp_a1
		rol zp_a2
		rol zp_a3
		rol zp_remL
		rol zp_remH
	
		dex	
		bne !loop-
		rts


.segment Code "Validate sqroot(1000000) == 1000"
run_sqroot:
		// 1000000 = $0f4240	
		lda #$0f
		sta zp_a3
		lda #$42
		sta zp_a2
		lda #$40
		sta zp_a1
		jsr sqroot24
		
		// 1000 = $03e8
		lda zp_rootH
		cmp #$03
		bne !error+
		lda zp_rootL
		cmp #$e8
		bne !error+
		rts
// Oh no! We find ourselves in a world where sqroot(1000000) != 1000
// ... loop forever, while we wait for the universe to reboot ...
!error:
		dec status_line+07
		jmp !error-

// ##################################################################
// Splash screen - only in run-once space
// ##################################################################

.segment CodeInit "Padding"
.align $0100
.segment CodeInit "Splash screen code"
showsplashscreen:
		lda #$00
		sta $d020
		sta $d021
		ldx #$00
!loop:
		lda splashtext+(0*250),x
		sta $0400+(0*250),x
		lda splashtext+(1*250),x
		sta $0400+(1*250),x
		lda splashtext+(2*250),x
		sta $0400+(2*250),x
		lda splashtext+(3*250),x
		sta $0400+(3*250),x
		
		lda #$07
		sta $d800+(0*250),x
		sta $d800+(1*250),x
		sta $d800+(2*250),x
		sta $d800+(3*250),x
		inx
		cpx #250
		bcc !loop-

		ldx #$00
!loop:		
		lda #$01		
		sta $d800+(00*40),x		
		sta $d800+(09*40),x		
		sta $d800+(19*40),x		
		
		lda #$0d
		sta $d800+(06*40),x		
		sta $d800+(07*40),x		

		lda #$0a
		sta $d800+(18*40),x		
		
		inx		
		cpx #40		
		bcc !loop-	
			
// We still have KERNAL IRQ running at this point		
// -> just read $00cb for keypress info (ignoring shift etc)		
//		
// "1" : $38		
// "2" : $3b		
// "3" : $08
// "4" : $0b
// "9" : $20
// "0" : $23
// RET : $01
// SPC : $3c

#if BATCHMODE
		lda #$38
		bne !inject+
#endif
!loop:
		lda $cb
!inject:
		cmp #$38
		beq startM1
		cmp #$3b
		beq startM2
		cmp #$20
		beq startERROR

/*
		pha
		and #$0f
		tay
		lda tab_hex2char,y
		sta $0403
		pla
		lsr
		lsr
		lsr
		lsr
		tay
		lda tab_hex2char,y
		sta $0402
*/
		jmp !loop-
startERROR:
		inc fake_an_error
		lda #'9'
		sta status_line+3
		jmp startM1
startM2:
		lda #'2'
		sta status_line+3
		lda #JMP_ABS
		sta mode_shift
startM1:
		jsr blackout
		jmp main

blackout:
		ldx #$00
		lda #$00
!loop:
		sta $d800+(0*250),x
		sta $d800+(1*250),x
		sta $d800+(2*250),x
		sta $d800+(3*250),x
		inx
		cpx #250
		bcc !loop-
		rts
		
.segment DataShowOnce "Splash screen data"
splashtext:
//     0123456789012345678901234567890123456789
.text "       ** prime sieve 1 million **      "
.text "                                        "
.text "   m:01 fastest  - lazy bitfield init   "
.text "   m:02 slower   - full bitfield init   "
.text "   m:09 test err - forget 2 is prime    "
.text "                                        "
.text "         press 1,2 or 9 to start        "
.text "    space to restart after completion   "
.text "                                        "
.text "-------------- algorithm: --------------"
.text "calculates all primes < 1,000,000       "
.text "validates count (78,498 - $0132a2)      "
.text "                                        "
.text "see readme.md for algorithm details     "
.text "in short: quick roll out of bit patterns"
.text " + lazy init of bitfield with byte seqs "
.text " + stays true to original sieve algo    "
.text "                                        "
.text "v1.0rc2                  raz/cml 04.2026"
.text "---------------- info: -----------------"
.text "bitfield:  $0bdc-$ffff (500,000 bits)   "
.text "bit order: 01234567    (as c64 graphics)"
.text "code:      $0010-$01ef + $0a00-$0bbf    "
.text "tables:    $0200-$09ff                  "
.text "screen:    $0800 (colors on top of data)"

// ##################################################################
// Tables - definitions and roll-outs
// ... most to be transferred to lower memory
// ##################################################################

/*

odd_space
00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15
01,03,05,07,09,11,13,15,17,19,21,23,25,27,29,31
norm_space

bit_space
00,01,02,03,04,05,06,07,00,01,02,03,04,05,06,07


bit_space to bit_pattern:
(Reverse order: mimicing how char/hires grapics is visualized)

00 => %10000000
01 => %01000000
02 => %00100000
03 => %00010000
04 => %00001000
05 => %00000100
06 => %00000010
07 => %00000001

Periode:

00                      : 1 step     EVEN
01,02,03,04,05,06,07,00 : 8 steps    ODD
02,04,06,00             : 4 steps    EVEN
03,06,01,04,07,02,05,00 : 8 steps    ODD
04,00                   : 2 steps    EVEN
05,02,07,04,01,06,03,08 : 8 steps    ODD
06,04,02,00             : 4 steps    EVEN
07,06,05,04,03,02,01,00 : 8 steps    ODD


*/

// Helper table - only exists during assembly
.var renorm_nsquared = List()
.for (var i=0;i<512;i++) {
	.var norm_space_i = (i*2)+1
	.var norm_space_i_sq = pow(norm_space_i,2)
	.var odd_space_i_sq  = round( (norm_space_i_sq / 2) -0.5 )
	.eval renorm_nsquared.add(odd_space_i_sq)
}
//.print renorm_nsquared (debug)

// Actual generated tabels
.segment DataReloc "n squared tables"
rtab_nsquared_low:   .fill 500, <(bitfield_start+(renorm_nsquared.get(i)>>3))
                     .fill 12,0
rtab_nsquared_high:  .fill 500, >(bitfield_start+(renorm_nsquared.get(i)>>3))
                     .fill 12,0
rtab_nsquared_3bits: .fill 500, renorm_nsquared.get(i)&$07 // NOTE: this repeats $00,$04,$04,$00 forever
                     .fill 12,0

// Helper table (compile time)
.var num_bits_set = List()
.for (var i=0;i<256;i++) {
	.var bits = 0
	.var ish  = i
	.for (var j=0;j<8;j++) {
		.eval bits += (ish & $01)
		.eval ish = ish>>1
	}
	.eval num_bits_set.add(bits)
}
//.print num_bits_set
.segment DataReloc "Bit count table"
rtab_num_bits_set: .fill 256, num_bits_set.get(i)

.segment DataReloc "Mask 8 table"
rtab_mask8_pagelong:
.fill 32, [%10000000,%01000000,%00100000,%00010000,%00001000,%00000100,%00000010,%00000001] // 256 bytes
.segment DataReloc "Div 8 table"
rtab_div8_pagelong:
.fill 256, i>>3


.segment Data "Lazy init helper tables"
// Structure: linear vals (4 vals) needed for the lz init ...
//            interleaved with the original nsquared vals
//            for setting and restoring tables using the same code.

lz_linear_low:   .fill 4, <(lz_last_block)
lz_nsquared_low: .fill 4, <(bitfield_start+(renorm_nsquared.get(i)>>3))
lz_linear_high:  .fill 4, >(lz_last_block)
lz_nsquared_high:.fill 4, >(bitfield_start+(renorm_nsquared.get(i)>>3))
lz_linear_3bits: .byte $00,$01,02,$03
lz_nsquared_3bits: .fill 4, renorm_nsquared.get(i)&$07

.segment DataReloc "bitmask table"
.align $08
rtab_frac_to_patt:
//tab_frac_to_patt:
.byte %01111111
.byte %10111111
.byte %11011111
.byte %11101111
.byte %11110111
.byte %11111011
.byte %11111101
.byte %11111110

.segment DataReloc "hex2char tab"
rtab_hex2char:
.text "0123456789abcdef"

