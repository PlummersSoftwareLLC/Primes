; Sieve of Eratosthenes implementation for Ben Eater's breadboard 65c02 computer
;
; Peculiarities:
; * The sieve size is set to 250,000: there's not enough memory in the computer for 1,000,000
; * The sieve size needs to be divisable by 16
; * The square root of the sieve size needs to be divisible by 8, or else rounded up to the nearest multiple of 8

;====================================================
; Main symbols
;====================================================

SIEVE_SIZE = 250000
SIEVE_SQRT = 504                                ; Actual square root is 500, rounded up to multiple of 8

; expected prime count is 22044 = $561c
CNT_LOW = $1c
CNT_HIGH = $56

BUF_BITS = SIEVE_SIZE / 2
BUF_BYTES = BUF_BITS / 8
BUF_LAST = BUF_BYTES - 1
SQRT_BYTES = SIEVE_SQRT / 8

;====================================================
; I/O defines
;====================================================

PORTB = $6000
PORTA = $6001
DDRB = $6002
DDRA = $6003

E  = %10000000
RW = %01000000
RS = %00100000
BF = %10000000

DISPLAY_WIDTH = 16

;====================================================
; Zero page variables
;====================================================

    .dsect
    .org $0000              ; .dsect defaults to this, just adding it for clarity

counter: .wrd               ; General counter variable.
fctr_byte: .byt             ; Factor byte component. Initialized to 00 at startup.
fctr_bitcnt: .byt           ; Factor bit component. Initialized to 03 at startup.
curptr: .wrd                ; Address part of current prime candidate pointer. Initialized to 0000 at startup.
curptr_bit: .byt            ; Bit part of current prime candidate pointer. Initialized to 01 at startup.
cndptr: .wrd                ; Address part of last found unset prime candidate pointer.
cndptr_bit: .byt            ; Bit part of last found unset prime candidate pointer.
lcd_line: .byt              ; LCD line we're printing to, either 0 or 1. Initialized to 00 at startup.
lcd_charspace: .byt         ; Number of characters left on current LCD line. Initialized to 17 at startup.
count_string: .blk 6        ; Memory space for string with number of primes found. It's 6 bytes, because the 
                            ;   largest value a 16-bit word can hold is 65535. The 6th byte in the string is for
                            ;   the terminating zero. First byte is initialized to 00 at startup.
remainder: .byt             ; Variable for long division remainder.
stringptr: .wrd             ; Pointer to current string to be printed.

    .dend

;====================================================
; Main sieve buffer
;====================================================

buffer = $200
buffer_end = buffer + BUF_LAST

;====================================================
; Program - ROM starts here
;====================================================

    .org $8000

reset:

    ; Set up stack pointer
    ldx #$ff
    txs

    ; Initialize variables
    lda #00
    sta fctr_byte
    sta curptr
    sta curptr + 1
    sta lcd_line
    sta count_string

    lda #03
    sta fctr_bitcnt

    lda #01
    sta curptr_bit

    lda #DISPLAY_WIDTH + 1
    sta lcd_charspace

    ; Initalize VIA and LCD
    lda #%11111111          ; Set all pins on port B to output
    sta DDRB
    lda #%11100000          ; Set top 3 pins on port A to output
    sta DDRA

    lda #%00111000          ; Set 8-bit mode; 2-line display; 5x8 font
    jsr lcd_instruction
    lda #%00001111          ; Display on; cursor on; blink on
    jsr lcd_instruction
    lda #%00000110          ; Increment and shift cursor right, don't scroll
    jsr lcd_instruction
    lda #%00000001          ; Clear screen
    jsr lcd_instruction

    ; Print "Processing..." on first line
    lda #<processing_label
    sta stringptr
    lda #>processing_label
    sta stringptr + 1

    jsr print_string

    ; Set display line to the second one. We'll show progress there.
    lda #1
    sta lcd_line
    jsr line_start
    
    ; Show that we're clearing memory
    lda #"-"
    jsr print_char

    ; Initialize (zero) buffer
    jsr init_buf

    ;
    ; Main logic
    ;

    ; Set pointer to prime sieve buffer
    lda #<buffer
    sta curptr
    lda #>buffer
    sta curptr + 1

.cnd_loop:
    lda (curptr)            ; Load byte curptr points to

    bit curptr_bit          ; Check if bit at pointer is set 
    bne .unset_fctrs        ; Is it? Unclear multiples then!

    ; It wasn't, so we increase factor and move pointer
    jsr inc_ptrbit
    jsr inc_fctr            ; This also loads the factor byte number into X

    ; We can stop looking for factor if we've reached the square root of the sieve size
    cpx #SQRT_BYTES
    bcc .cnd_loop
    jmp .validate

; We found a factor, let's clear multiples!
.unset_fctrs:

    ; Save pointer, so we can resume factor searching later
    lda curptr
    sta cndptr
    lda curptr + 1
    sta cndptr + 1
    lda curptr_bit
    sta cndptr_bit

    ; Show that we found a factor
    lda #"."
    jsr print_char

    ; Clear multiples in buffer
    jsr clear_multiples

    ; We're done clearing multiples of our current factor, so increase it
    jsr inc_fctr            ; This also loads the factor byte number into X

    ; We can stop if we've reached the square root of the sieve size
    cpx #SQRT_BYTES  
    bcc .next_fctr
    jmp .validate

.next_fctr:

    ; Restore pointer and search for next factor
    lda cndptr
    sta curptr
    lda cndptr + 1
    sta curptr + 1
    lda cndptr_bit
    sta curptr_bit

    jsr inc_ptrbit
    jmp .cnd_loop

; We're done!
.validate:

    ; Set counter to 1 (2 is also prime but not in the bitmap)
    lda #1
    sta counter
    lda #0
    sta counter + 1

    ldy #0

    ; Show that we're counting
    lda #"+"
    jsr print_char

    ; Count bits in buffer
    jsr cnt_buf

    lda #%00001100          ; Display on; cursor off; blink off
    jsr lcd_instruction
    lda #%00000001          ; Clear screen
    jsr lcd_instruction

    ; We write the "result valid" line first, but at the bottom line of the display.
    ;   Why? Because I think it's more logical that way :)
    lda #1
    sta lcd_line
    jsr line_start

    ; Print valid label
    lda #<valid_label
    sta stringptr
    lda #>valid_label
    sta stringptr + 1

    jsr print_string

    ; Check the number of primes we found
    lda counter
    cmp #CNT_LOW
    bne .set_invalid
    lda counter + 1
    cmp #CNT_HIGH
    bne .set_invalid

    ; Prime count is valid
    lda #<yes_value
    sta stringptr
    lda #>yes_value
    sta stringptr + 1
    
    jmp .write_validflag

; Prime count is invalid
.set_invalid:
    lda #<no_value
    sta stringptr
    lda #>no_value
    sta stringptr + 1

.write_validflag:
    jsr print_string

    ; Move to top line of the display
    lda #0
    sta lcd_line
    jsr line_start

    ; Print time label
    lda #<count_label
    sta stringptr
    lda #>count_label
    sta stringptr + 1

    jsr print_string

    ; Convert count value to decimal string
    jsr count_to_string

    ; Print the primes count
    lda #<count_string
    sta stringptr
    lda #>count_string
    sta stringptr + 1

    jsr print_string

; That's it! Time for our "active halt"
.finish:
    jmp .finish

;====================================================
; Initialize the sieve buffer by setting it to all 0s.
;
; Uses: A, X, Y, counter, curptr
;====================================================
init_buf:

    ; Set pointer to buffer address
    lda #<buffer
    sta curptr
    lda #>buffer
    sta curptr + 1

    lda #<BUF_LAST
    sta counter
    lda #>BUF_LAST
    sta counter + 1

    ; Set up registers
    lda #$ff
    ldx #0
    ldy #0

.init_loop:
    sta (curptr), y

    ; Check if we've counted down to 0
    cpx counter
    bne .init_deccntr
    cpx counter + 1
    beq .init_buf_end

; Decrease counter
.init_deccntr:
    dec counter
    cmp counter             ; Use the fact that A contains $ff
    bne .init_next
    dec counter + 1

; move on to next byte
.init_next:
    iny
    bne .init_loop           ; If Y has wrapped, increase high byte of pointer
    inc curptr + 1
    jmp .init_loop

.init_buf_end:
    rts

;====================================================
; Make bit pointer point to the next bit in the sieve buffer.
;
; In: 
;   curptr and curptr_bit: pointer to be increased
; Out: 
;   curptr and curptr_bit: updated to point to tne next sieve bit
;====================================================
inc_ptrbit:
    asl curptr_bit          ; Shift bit part of pointer to the left
    bcc .inc_ptrbit_end     ; If we didn't shift it into the Carry flag, we're done

    ; Time to move on to the next byte
    rol curptr_bit          ; First, rotate the bit out of Carry into bit 0 of the pointer

    inc curptr              ; Increase pointer low byte
    bne .inc_ptrbit_end      ; If we didn't roll over to 0, we're done

    inc curptr + 1          ; Increase high byte of pointer

.inc_ptrbit_end:
    rts

;====================================================
; Add 2 to factor.
;
; In: 
;   fctr_byte and fctr_bitcnt: factor value to be increased
; Out: 
;   X: updated value of fctr_byte
;   fctr_byte and fctr_bitcnt: updated to next factor
; Uses: A
;====================================================
inc_fctr:
    ; Increase factor bit value twice
    inc fctr_bitcnt
    inc fctr_bitcnt

    ; Check if we moved into the next byte
    lda fctr_bitcnt
    cmp #8
    bcc .ld_fctrroot        ; Value <8? We're done!
    sbc #8                  ; Subtract 8 from bit value
    sta fctr_bitcnt         ;   and save it

    inc fctr_byte           ; Add 1 to factor byte value

.ld_fctrroot:
    ldx fctr_byte           ; Load factor byte into X
    rts

;====================================================
; Clear multiples of factor in the sieve buffer. This routine is not 
; optimal from an arithmatic perspective, in the sense that it clears
; all odd multiples of the factor. The most efficient approach would
; be to start at factor * factor. However, with the 65c02 not having a 
; multiply instruction, computing that would itself be almost as much work
; as just starting from the beginning. As such, saving the few actual 
; "clear bit" instructions at runtime would almost double the code of this 
; routine.
;
; In: 
;   curptr and curptr_bit: pointer to the factor bit in the sieve. The
;                          clearing will start at the next (i.e. first) 
;                          multiple from a sieve bit perspective.
;   fctr_byte and fctr_bitcnt: factor of which multiples must be cleared
; Uses: A, X, curptr and curptr_bit
;====================================================
clear_multiples:
    ; Check if the bit part of the factor is 0. If so, 
    ;   skip adding it to the prime candidate pointer
    ldx fctr_bitcnt
    beq .fctr_addbyte

; Increase pointer with factor, bit part first
.fctr_decloop:
    jsr inc_ptrbit
    dex
    bne .fctr_decloop

; Now add factor byte to pointer
.fctr_addbyte:
    lda curptr
    clc
    adc fctr_byte
    sta curptr

    ; If we rolled over the pointer low byte, increase the high one
    bcc .fctr_chkend
    inc curptr + 1

.fctr_chkend:
    ; See if we've reached the end of our buffer. Start with the high byte
    ;   of the pointer.
    lda curptr + 1
    cmp #>buffer_end
    bcc .unset_curbit
    bne .unset_buf_end

    lda curptr
    cmp #<buffer_end
    bcc .unset_curbit
    bne .unset_buf_end

.unset_curbit:
    jsr clear_ptrbit        ; Clear the bit under the pointer

    ; Clear next multiple
    jmp clear_multiples

.unset_buf_end:
    rts

;====================================================
; Clear the bit in the sieve buffer that the relevant pointer points to.
;
; In:
;   curptr and curptr_bit: pointer to sieve bit to be cleared
; Uses: A
;====================================================
clear_ptrbit:
    ; Load and invert bit part of pointer, to create a bit mask for the next step
    lda curptr_bit
    eor #$ff

    ; Clear the correct bit in the byte at curptr by ANDing the mask with it
    and (curptr)
    sta (curptr)

    rts

;====================================================
; Count the bits that are set in the sieve buffer.
;
; Out:
;   counter: number of set bits found
; Uses: A, curptr
;====================================================
cnt_buf:
    ; Make curptr point to start of buffer
    lda #<buffer
    sta curptr
    lda #>buffer
    sta curptr + 1

.cnt_byteloop
    ; Load byte, and move to next one if 0
    lda (curptr)
    beq .cnt_chkend

.cnt_bitloop:
    ; We rotate the leftmost bit into the Carry flag, so we can
    ;   use bcc to check if it is set or not.
    asl
    bcc .cnt_chkempty       ; If Carry is clear, there's nothing to see here

    ; We found a set bit, so increase our counter
    inc counter
    bne .cnt_chkempty       ; If the low byte in counter has rolled over to 0,
    inc counter + 1         ;   then also increase its high byte.

; If A is now zero, we're done with this byte
.cnt_chkempty:
    cmp #0
    beq .cnt_chkend
    jmp .cnt_bitloop        ; More bits left in this byte, so keep looking

; check if we're at the last buffer byte
.cnt_chkend:
    lda #<buffer_end
    cmp curptr
    bne .cnt_next

    lda #>buffer_end
    cmp curptr + 1
    bne .cnt_next

    ; We're done
    rts

; Move on to the next byte
.cnt_next:
    inc curptr
    bne .cnt_byteloop       ; If the low byte in curptr has rolled over to 0,
    inc curptr + 1          ;   then also increase its high byte.
    jmp .cnt_byteloop

;====================================================
; Convert the 16-bit value in counter to a decimal string.
; The approach used here is explained in one of the Ben Eater YouTube episodes 
; about the 65c02 breadboard computer.
;
; Note that the value in counter is cleared by this routine!
;
; Out:
;   count_string: zero-terminated decimal value previously held in counter
; Uses: A, X, Y, counter, remainder
;====================================================
count_to_string:
    ; Set remainder to 0
    lda #0
    sta remainder

    ldx #16                 ; Bit width of the word

    clc                     ; Also clear Carry flag

.div_loop:
    ; Rotate counter and remainder left
    rol counter
    rol counter + 1
    rol remainder

    ; Try to substract divisor
    sec                     ; Set Carry flag before we substract
    lda remainder
    sbc #10                 ; Take 10 off
    bcc .ignore_result      ; If remainder < 10, we overshot

    ; Store current remainder
    sta remainder

.ignore_result:
    ; Continue if there are bits left to shift
    dex
    bne .div_loop

    ; Rotate last bit into quotient
    rol counter
    rol counter + 1

    ; Current remainder is counter digit value
    lda remainder
    clc                     ; Clear Carry flag before we add
    adc #"0"                ; A now holds the digit we want to insert into the string

    ldy #0                  ; Prepare for indexed addressing

; Add counter digit to beginning of count_string. We do this by shifting all
;   existing digits to the right. We use X to temporarily hold the characters we
;   need to push one place to the right.
.push_loop:
    ldx count_string, y     ; X = "old" character at current string position
    sta count_string, y     ; Write the digit we want to put at current position
    iny                     ; Move to next character in the string
    txa                     ; A = "old" character we now want to put there

    bne .push_loop          ; Stop when we're about to write the terminating null...

    ; ...but don't forget to write it!
    sta count_string, y

    ; We're done dividing when counter is 0.
    lda counter
    ora counter + 1
    bne count_to_string     ; Not 0 yet? Continue dividing!

    rts

;====================================================
; Print null-terminated string to LCD.
;
; In:
;   stringptr: pointer to string to write
; Uses: A, Y
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
; Wait until the LCD's busy flag is off.
;====================================================
lcd_wait:
    pha                     ; Push A onto the stack

    lda #%00000000          ; Set all pins on port B to be input
    sta DDRB

.busy_loop:
    lda #RW
    sta PORTA               ; Ask for read

    lda #(RW | E)           ; Enable read
    sta PORTA

    lda PORTB               ; Read status
    and #BF                 ; Check for busy flag
    bne .busy_loop          ; Repeat if busy

    lda #%11111111          ; Set all pins on port B to be output
    sta DDRB

    pla                     ; Pop A off the stack
    rts

;====================================================
; Clears the current display line and sets the LCD memory pointer to its
; beginning.
;
; In:
;   lcd_line: line to clear
; Uses: A, X
;====================================================
clear_line:
    jsr line_start          ; Move to beginning of current line

    ldx #DISPLAY_WIDTH      ; Number of spaces to put

.space_loop:
    lda #" "
    jsr send_char

    dex
    bne .space_loop

; Note: clear_line falls through into line_start!
 
;====================================================
; Sets the LCD memory pointer to the beginning of one of the display lines.
;
; In:
;   lcd_line: line to set the pointer to (0 or 1)
; Uses: A
;====================================================
line_start:
    ; Reset char space counter, as we'll be at the beginning of the line
    lda #DISPLAY_WIDTH + 1
    sta lcd_charspace

    lda lcd_line

    beq .instr_bit          ; If A is 0, that matches the DDRAM address

    lda #$40                ; First character of second line is at $40

.instr_bit:
    ora #%10000000          ; Instruction is to set DDRAM address
    
; Note: line_start falls through into lcd_instruction!

;====================================================
; Send an instruction to the LCD
;
; In:
;   A: Instruction value to send
; Uses: A
;====================================================
lcd_instruction:
    jsr lcd_wait

    sta PORTB               ; Latch instruction on port B

    lda #0                  ; Clear RS/RW/E bits
    sta PORTA

    lda #E                  ; Set E bit to send instruction
    sta PORTA
    
    lda #0                  ; Clear RS/RW/E bits
    sta PORTA

    rts

;====================================================
; Print a character to the LCD. This routine keeps track of where we are on our 
; current line, and clears it if we try to print beyond the end of it.
;
; In:
;   A: Character to print
; Uses: A
;====================================================
print_char:
    dec lcd_charspace       ; Decrease "space left counter"
    bne send_char           ; Not at zero yet? We have room left!

    ; If we're here, we filled the LCD line. We'll clear it and start at the beginning

    pha                     ; Save A
    jsr clear_line          ; Clear current line
    pla                     ; Retrieve A

; Note: print_char falls through into send_char!

;====================================================
; Send a character to the LCD
;
; In:
;   A: Character to send
;====================================================
send_char:
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

;====================================================
; Reset vector, end of ROM
;====================================================

    .org $fffc
    .word reset
    .word $0000