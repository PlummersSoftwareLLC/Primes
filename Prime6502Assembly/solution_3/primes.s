; Sieve of Eratosthenes implementation for Ben Eater's breadboard 65c02 computer
;
; Peculiarities:
; * The sieve size is set to 250,000: there's not enough memory in the computer for 1,000,000
; * The sieve size needs to be divisable by 16
; * The square root of the sieve size needs to be divisible by 8, or else rounded up to the nearest multiple of 8

SIEVE_SIZE = 250000
SIEVE_SQRT = 504                                ; Actual square root is 500, rounded up to multiple of 8

; expected prime count is 22044 = $561c
CNT_LOW = $1c
CNT_HIGH = $56

BUF_BITS = SIEVE_SIZE / 2
BUF_BYTES = BUF_BITS / 8
SQRT_BYTES = SIEVE_SQRT / 8

BUF_START = $200
BUF_LAST = BUF_BYTES - 1
BUF_END = BUF_START + BUF_LAST

; I/O defines

PORTB = $6000
PORTA = $6001
DDRB = $6002
DDRA = $6003

E  = %10000000
RW = %01000000
RS = %00100000
BF = %10000000


; Zero page variables

    .org $0000

    .dsect

counter: .wrd                                   ; 0000
fctr_byte: .byt                                 ; 00
fctr_bitcnt: .byt                               ; 03
curptr: .wrd                                    ; 00
curptr_bit: .byt                                ; 01
cndptr: .wrd                                    ; 0000
cndptr_bit: .byt                                ; 00
char_pos: .byt                                  ; 00
remainder: .byt                                 ; unset
stringptr: .wrd                                 ; unset

    .dend

; Program in ROM

    .org $8000

reset:

    ; Set up stack pointer
    ldx #$ff
    txs

    ; Initialize variables
    lda #$00
    sta counter
    sta counter + 1
    sta fctr_byte
    sta curptr
    sta curptr + 1
    sta cndptr
    sta cndptr + 1
    sta cndptr_bit
    sta char_pos

    lda #$03
    sta fctr_bitcnt

    lda #$01
    sta curptr_bit

    ; Initalize LCD
    lda #%11111111 ; Set all pins on port B to output
    sta DDRB

    lda #%11100000 ; Set top 3 pins on port A to output
    sta DDRA

    lda #%00111000 ; Set 8-bit mode; 2-line display; 5x8 font
    jsr lcd_instruction
    lda #%00001111 ; Display on; cursor on; blink on
    jsr lcd_instruction
    lda #%00000110 ; Increment and shift cursor right, don't scroll
    jsr lcd_instruction
    lda #%00000001 ; Clear screen
    jsr lcd_instruction

    ;;; Print processing
    
    lda #"-"
    jsr BSOUT

    ; init buffer
    jsr init_buf

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

    ; clear multiples in buffer
    jsr unset_buf

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

    ; read and save the jiffy clock with interrupts disabled; oddly enough it's stored big-endian
    sei
    lda JIFFY
    sta clock+2
    lda JIFFY+1
    sta clock+1
    lda JIFFY+2
    sta clock
    cli

    ; set counter to 1 (2 is also prime but not in the bitmap)
    lda #1
    sta counter
    lda #0
    sta counter+1

    ldy #0

    lda #"+"
    jsr BSOUT

    ; count bits in buffer
    jsr cnt_buf

    lda #"@"
    jsr BSOUT

    ; divert output to file
    ldx #LOGICAL_NUM
    jsr CKOUT

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

finish:
    lda #EOL
    jsr BSOUT

    ; restore temp pointer
    pla
    sta CURPTR
    pla
    sta CURPTR+1

    rts

;
; routine: init the buffer
;
init_buf:

    ; set pointer to buffer address
    lda #<BUF_START
    sta CURPTR
    lda #>BUF_START
    sta CURPTR+1

    lda #<BUF_LAST
    sta counter
    lda #>BUF_LAST
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
    beq init_buf_end

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

init_buf_end:
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
; routine: clear multiples of factor
; 
unset_buf:
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
    bne unset_buf_end
    lda CURPTR
    cmp #<BUF_END
    bcc unset_curbit
    bne unset_buf_end

; clear bit under pointer
unset_curbit:
    jsr unset_ptrbit

    ; clear next multiple
    jmp unset_buf

unset_buf_end:
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
; routine: count bits
;
cnt_buf:
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

;====================================================
; Print zero-terminate string to LCD
;
; In:
;   stringptr: pointer to string to write
;====================================================
print_string:
    ldy #0

.print_loop:
    lda (stringptr), y
    beq .print_string_end

    jsr print_char

    iny
    bra .print_loop

.print_string_end:
    rts

;====================================================
; Wait until the LCD's busy flag is off
;====================================================
lcd_wait:
    pha             ; Push A onto the stack

    lda #%00000000  ; Set all pins on port B to be input
    sta DDRB

.busy_loop:
    lda #RW
    sta PORTA       ; Ask for read

    lda #(RW | E)   ; Enable read
    sta PORTA

    lda PORTB       ; Read status
    and #BF         ; Check for busy flag
    bne .busy_loop   ; Repeat if busy

    lda #%11111111  ; Set all pins on port B to be output
    sta DDRB

    pla             ; Pop A off the stack
    rts

;====================================================
; Send an instruction to the LCD
;
; In:
;   A: Instruction value to send
;====================================================
lcd_instruction:
    jsr lcd_wait

    sta PORTB       ; Latch instruction on port B

    lda #0          ; Clear RS/RW/E bits
    sta PORTA

    lda #E          ; Set E bit to send instruction
    sta PORTA
    
    lda #0          ; Clear RS/RW/E bits
    sta PORTA

    rts

;====================================================
; Send a character to the LCD
;
; In:
;   A: Character to send
;====================================================
print_char:
    jsr lcd_wait

    sta PORTB       ; Latch character on port B

    lda #RS         ; Select character register, clear E bit
    sta PORTA

    lda #(RS | E)   ; Set E bit to send character
    sta PORTA

    lda #RS         ; Clear E bit
    sta PORTA

    rts

;====================================================
; String constants
;====================================================
processing_label:   .string "Processing..."
count_label:        .string "Count: "
valid_label:        .string "Valid: "
yes_value:          .string "yes"
no_value:           .string "no"
emptyline_label:    .string "                "

;====================================================
; Reset vector, end of ROM
;====================================================

    .org $fffc
    .word reset
    .word $0000