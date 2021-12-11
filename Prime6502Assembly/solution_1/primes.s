; Sieve of Eratosthenes implementation for the Commodore 128
;
; Peculiarities:
; * The sieve size needs to be divisable by 32
; * The square root of the sieve size needs to be divisible by 8, or else rounded up to the nearest multiple of 8

SIEVE_SIZE=1000000
SIEVE_SQRT=1000

; expected prime count is 78498 = $0132a2
CNT_LOW=$a2
CNT_MIDDLE=$32
CNT_HIGH=$01

BUF_BITS=SIEVE_SIZE/2
BUF_BYTES=BUF_BITS/8
HALF_BUF_SIZE=BUF_BYTES/2
HALF_BUF_LAST=HALF_BUF_SIZE-1
SQRT_BYTES=SIEVE_SQRT/8

CURPTR=$fa

MMUCR=$ff00
MMURCR=$d506
RAM_0=%00001110
RAM_1=%01001110
LOGICAL_NUM=$01

SETNAM=$ffbd
SETLFS=$ffba
OPEN=$ffc0
CKOUT=$ffc9
CLOSE=$ffc3
CLRCH=$ffcc
BSOUT=$ffd2
SETTIM=$ffdb
RDTIM=$ffde

EOL=13
SYS=$9e

BASIC_START=$1c01
PROGRAM_START=$1300     ; 4864
BUF_START=$2000
BUF_END=BUF_START+HALF_BUF_LAST

    ; load BASIC program to start
    .org BASIC_START
    .word basic_end             ; address of next BASIC line
    .byte 10,0,SYS," 4864",0    ; 10 SYS 4864
basic_end:
    .word $00                   ; end of program

    .org PROGRAM_START
    jmp start

counter: .byte 0,0,0 
fctr_byte: .byte $00
fctr_bitcnt: .byte $03
curptr_bit: .byte $01
cndptr: .word 0
cndptr_bit: .byte 0
fname: .byte "OUTPUT,S,W"
fname_end: 
clock: .storage 3
time_label: .byte "TIME ",0
valid_label: .byte "VALID ",0
clock_string: .byte 0
    .storage 8
remainder: .storage 1
ram_config: .word 0

start:
    ; select kernal and I/O ROM, RAM 0
    lda MMUCR
    sta ram_config
    lda #RAM_0
    sta MMUCR

    ; make botton 8K RAM shared
    lda MMURCR
    sta ram_config+1
    and #%11110000
    ora #%00000110
    sta MMURCR

    ; set clock to 0
    lda #0
    tax 
    tay
    jsr SETTIM

    lda #"-"
    jsr BSOUT

    ; init first half of buffer (RAM 0)
    jsr init_halfbuf

    ; init second half of buffer (RAM 1)
    lda #RAM_1
    sta MMUCR

    lda #"-"
    jsr BSOUT

    jsr init_halfbuf

    ; select RAM 0
    lda #RAM_0
    sta MMUCR

    ;
    ; main logic
    ;

    ; set pointer to buffer
    lda #<BUF_START
    sta CURPTR
    lda #>BUF_START
    sta CURPTR+1

    ; load first buffer byte
    ldy #0

cnd_loop:
    lda (CURPTR),Y

    ; check if bit at pointer is set
    bit curptr_bit
    bne unset_fctrs

    ; it wasn't, so we increase factor and move pointer
    jsr inc_ptrbit
    jsr inc_fctr            ; this also loads the factor byte number into X

    ; we can stop if we've reached the square root of the sieve size
    cpx #SQRT_BYTES
    bcc cnd_loop
    jmp validate            ; we can't let this label get too far away from us

; we found a factor, let's clear multiples!
unset_fctrs:

    ; save pointer
    lda CURPTR
    sta cndptr
    lda CURPTR+1
    sta cndptr+1
    lda curptr_bit
    sta cndptr_bit

    lda #'.'
    jsr BSOUT

    ; clear multiples in first half of buffer
    jsr unset_halfbuf

    ; select RAM 1
    lda #RAM_1
    sta MMUCR

    ; subtract size of half buffer from pointer
    lda CURPTR
    sec
    sbc #<HALF_BUF_SIZE
    bcs sub_savelow
    
    ; decrease high pointer byte if we borrowed
    dec CURPTR+1
    sec

sub_savelow:
    sta CURPTR

    lda CURPTR+1
    sbc #>HALF_BUF_SIZE
    sta CURPTR+1

    lda #'.'
    jsr BSOUT

    ; clear bit under pointer
    jsr unset_ptrbit

    ; clear multiples in second half of buffer
    jsr unset_halfbuf

    ; select RAM 0
    lda #RAM_0
    sta MMUCR

    ; we're done clearing multiples of our current factor, so increase it
    jsr inc_fctr            ; this also loads the factor byte number into X

    ; we can stop if we've reached the square root of the sieve size
    cpx #SQRT_BYTES  
    bcc next_fctr
    jmp validate

next_fctr:    

    ; restore pointer and find next factor
    lda cndptr
    sta CURPTR
    lda cndptr+1
    sta CURPTR+1
    lda cndptr_bit
    sta curptr_bit
    jsr inc_ptrbit
    jmp cnd_loop

; we're done!
validate:

    ; read and save clock
    jsr RDTIM
    sta clock
    stx clock+1
    sty clock+2

    ; set counter to 1 (2 is also prime but not in the bitmap)
    lda #1
    sta counter
    lda #0
    sta counter+1

    ldy #0

    lda #"+"
    jsr BSOUT

    ; count bits in first half of buffer
    jsr cnt_halfbuf

    ; count bits in second half of buffer
    lda #RAM_1
    sta MMUCR

    lda #"+"
    jsr BSOUT
   
    jsr cnt_halfbuf

    ; switch back to RAM 0
    lda #RAM_0
    sta MMUCR

    lda #"@"
    jsr BSOUT

    ; set output filename
    lda #fname_end-fname
    ldx #<fname
    ldy #>fname
    jsr SETNAM

    ; set logical #, device # and secondary address
    lda #LOGICAL_NUM
    ldx #$08
    ldy #$01
    jsr SETLFS

    ; open file, bail out in case of error
    jsr OPEN
    bcs finish

    ; divert output to file
    ldx #LOGICAL_NUM
    jsr CKOUT
    bcs finish

    ; write valid label
    lda #<valid_label
    sta CURPTR
    lda #>valid_label
    sta CURPTR+1

    jsr write_str

    ; check the number of primes we found
    lda counter
    cmp #CNT_LOW
    bne set_invalid
    lda counter+1
    cmp #CNT_MIDDLE
    bne set_invalid
    lda counter+2
    cmp #CNT_HIGH
    bne set_invalid

    ; prime count is valid
    lda #'Y'
    jmp write_validflag

; prime count is invalid
set_invalid:
    lda #'N'

write_validflag:
    jsr BSOUT

    ; move on to next line
    lda #EOL
    jsr BSOUT

    ; write time label
    lda #<time_label
    sta CURPTR
    lda #>time_label
    sta CURPTR+1

    jsr write_str

    ; convert clock value to decimal string
    jsr clock_to_string

    lda #<clock_string
    sta CURPTR
    lda #>clock_string
    sta CURPTR+1

    ; write the clock tick count
    jsr write_str

    ; move on to next line
    lda #EOL
    jsr BSOUT

    ; restore output to screen
    jsr CLRCH

    ; output is done, so close file
    lda #LOGICAL_NUM
    jsr CLOSE

finish:
    lda #EOL
    jsr BSOUT

    lda ram_config+1
    sta MMURCR

    lda ram_config
    sta MMUCR
    rts

;
; routine: init half of the buffer
;
init_halfbuf:

    ; set pointer to buffer address
    lda #<BUF_START
    sta CURPTR
    lda #>BUF_START
    sta CURPTR+1

    lda #<HALF_BUF_LAST
    sta counter
    lda #>HALF_BUF_LAST
    sta counter+1

    ; setup registers
    lda #$ff
    ldx #0
    ldy #0

init_loop:
    sta (CURPTR),Y

    ; check if we've counted down to 0
    cpx counter
    bne init_deccntr
    cpx counter+1
    beq init_end

; decrease counter
init_deccntr:
    dec counter
    cmp counter             ; use the fact that A contains $ff
    bne init_next
    dec counter+1

; move on to next byte
init_next:
    iny
    bne init_loop           ; if Y has wrapped, increase high byte of pointer
    inc CURPTR+1
    jmp init_loop

init_end:
    rts

;
; routine: increase bit pointer
;
inc_ptrbit:
    asl curptr_bit
    bcc inc_ptrbit_end

    rol curptr_bit

    inc CURPTR
    bne inc_ptrbit_end

    inc CURPTR+1

inc_ptrbit_end:
    rts

;
; routine: add 2 to factor
;
inc_fctr:
    inc fctr_bitcnt
    inc fctr_bitcnt

    lda fctr_bitcnt
    cmp #8
    bcc ld_fctrroot
    sbc #8
    sta fctr_bitcnt

    inc fctr_byte

ld_fctrroot:
    ldx fctr_byte
    rts

;
; routine: clear multiples of factor for half of the buffer
; 
unset_halfbuf:
    ldx fctr_bitcnt
    beq fctr_addbyte

; increase pointer with factor, bit part first
fctr_decloop:
    jsr inc_ptrbit
    dex
    bne fctr_decloop

; now add factor byte to pointer
fctr_addbyte:
    lda CURPTR
    clc
    adc fctr_byte
    sta CURPTR

    ; if we rolled over the pointer low byte, increase the high one
    bcc fctr_chkend
    inc CURPTR+1

fctr_chkend:
    ; see if we've reached the end of our buffer
    lda CURPTR+1
    cmp #>BUF_END
    bcc unset_curbit
    bne unset_halfbuf_end
    lda CURPTR
    cmp #<BUF_END
    bcc unset_curbit
    bne unset_halfbuf_end

; clear bit under pointer
unset_curbit:
    jsr unset_ptrbit

    ; clear next multiple
    jmp unset_halfbuf

unset_halfbuf_end:
    rts

;
; routine: clear bit at pointer
;
unset_ptrbit:
    lda curptr_bit
    eor #$ff
    and (CURPTR),Y
    sta (CURPTR),Y
    rts

;
; routine: count bits in half the buffer
;
cnt_halfbuf:
    lda #<BUF_START
    sta CURPTR
    lda #>BUF_START
    sta CURPTR+1

cnt_byteloop
    ; load byte, and move to next one if 0
    lda (CURPTR),Y
    beq cnt_chkend

cnt_bitloop:
    asl
    bcc cnt_chkempty

    ; we found a set bit
    inc counter
    bne cnt_chkempty
    inc counter+1
    bne cnt_chkempty
    inc counter+2

; if A is zero, we're done with this byte
cnt_chkempty:
    cmp #0
    beq cnt_chkend
    jmp cnt_bitloop

; check if we're at the last buffer byte
cnt_chkend:
    lda #<BUF_END
    cmp CURPTR
    bne cnt_next
    lda #>BUF_END
    cmp CURPTR+1
    bne cnt_next

    ; we're done
    rts

; move on to the next byte
cnt_next:
    inc CURPTR
    bne cnt_byteloop
    inc CURPTR+1
    jmp cnt_byteloop

;
; routine: write string to screen or file
;
write_str:
    ldy #0

write_loop:
    lda (CURPTR),Y
    beq write_str_end
    jsr BSOUT
    iny
    jmp write_loop

write_str_end:
    rts

;
; The following routine has been heavily inspired by, not to say shamelessly stolen from, Ben Eater's YouTube video
; that can be found at https://www.youtube.com/watch?v=v3-a-zqKfgA. While you're there, watch the whole series and check  
; out his project page: https://eater.net/6502.
;
; routine: convert 24-bit value at *clock to decimal string at *clock_string using binary long division
; Note that the value at *clock to *clock+2 will be destroyed (set to 0) in the process
;
clock_to_string:
    ; set remainder to 0
    lda #0
    sta remainder
    clc

    ldx #24

div_loop:
    ; rotate clock and remainder left
    rol clock
    rol clock+1
    rol clock+2
    rol remainder

    ; try to substract divisor
    sec
    lda remainder
    sbc #10
    
    ; dividend < divisor
    bcc ignore_result

    ; store current remainder
    sta remainder

ignore_result:
    ; continue if there are bits left to shift
    dex
    bne div_loop

    ; rotate last bit into quotient
    rol clock
    rol clock+1
    rol clock+2

    ; current remainder is clock digit value
    lda remainder
    clc
    adc #'0'

    pha
    ldy #0

; add clock digit to beginning of clock string
push_loop:
    lda clock_string,Y
    tax
    pla
    sta clock_string,Y
    iny
    txa
    pha

    bne push_loop

    ; don't forget the closing null
    pla
    sta clock_string,Y

    ; are we done dividing?
    lda clock
    ora clock+1
    ora clock+2

    bne clock_to_string

    rts