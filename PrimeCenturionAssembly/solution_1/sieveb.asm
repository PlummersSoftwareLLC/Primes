* Sieve of Eratosthenes by ren14500. Public domain.
*
* This version runs bare-metal and uses MMIO to CRT0 for I/O.
*
* Assemble with P.ASM, hit SELECT to get to LOS and enter the executable name.
*
* Search for *** to find optional parts to comment or uncomment.
*
* Maximum prime and square root of it. The number of bytes needed to hold the
* flags. Uncomment one set. If 65,535 is not used, you must uncomment some
* code later as well!
MXPRIME   EQU       65535     ; 65,535
RTPRIME   EQU       255       ; 65,535
FLAGBYTES EQU       65535/8+1 ; 65,535
***MXPRIME   EQU       40000     ; 40,000
***RTPRIME   EQU       200       ; 40,000
***FLAGBYTES EQU       40000/8   ; 40,000
***MXPRIME   EQU       32768     ; 32,768
***RTPRIME   EQU       181       ; 32,768
***FLAGBYTES EQU       32768/8   ; 32,768
***MXPRIME  EQU        10000     ; 10,000
***RTPRIME EQU         100       ; 10,000
***FLAGBYTES EQU       10000/8   ; 10,000
***MXPRIME  EQU        1000      ; 1,000
***RTPRIME EQU         31        ; 1,000
***FLAGBYTES EQU       1000/8    ; 1,000
***MXPRIME  EQU        100       ; 100
***RTPRIME EQU         10        ; 100
***FLAGBYTES EQU       100/8+1   ; 100
*
* Set the program title and begin.
          TITLE     'SIEVEO'
ZSIEVEO   BEGIN     X'0200'
*
* Reserve space for the stack.
          DS        100
STKTOP    EQU       *
*
* Bitmasks to get each bit from a flags byte.
GMASKS    DB        X'01'
          DB        X'02'
          DB        X'04'
          DB        X'08'
          DB        X'10'
          DB        X'20'
          DB        X'40'
          DB        X'80'
*
* Bitmasks to clear each bit from a flags byte.
CMASKS    DB        X'FE'
          DB        X'FD'
          DB        X'FB'
          DB        X'F7'
          DB        X'EF'
          DB        X'DF'
          DB        X'BF'
          DB        X'7F'
*
* Constants.
WELCOME   DB        X'8D'     ; CR
          DW        X'8A8A'   ; LF LF
          DC        'START'
CRLF      DW        X'8D8A'   ; CR LF
          DB        0
TWO       DC        '      2'
          DB        0
COMMA     DC        ','
          DB        0
COUNT     DC        'COUNT:'
          DB        0
PROOT     DW        RTPRIME   ; Square root of prime.
*
* Variables.
PRIMES    DS        FLAGBYTES ; Flags. Bits set to 0 are not prime.
FACTORC   DS        2         ; Factor current.
FACTOR2   DS        2         ; Factor doubled.
PNUM      DS        2         ; Prime number.
PCOUNT    DW        1         ; Count of primes.
PFMT      DC        '@@@@@@@' ; Format for printing primes.
PLEN      EQU       *-PFMT    ; Length of format.
PBUFF     DB        0,PLEN+1  ; Buffer for printing.
*
* Entrypoint. Set the stack pointer.
ENTRY     XFR=      STKTOP,S  ; Literal STKTOP -> S.
          JSR/      PRINTSTR  ; Jump to subroutine direct.
          DW        WELCOME   ; Address of string to print.
*
* Set all odd bits to 1 to assume prime. Non-primes will get zeroed later.
* Register assignments:
*   A = Working register.
*   B = Working register.
*   X =
*   Y = Working register.
*   Z =
          LDAB=     X'AA'        ; Literal 0xAA -> AL.
          STAB/     PRIMES       ; Direct AL -> (PRIMES). Set first flags.
          LDA=      FLAGBYTES-2  ; Literal length remaining-1 -> A.
          LDB=      PRIMES       ; Literal source address -> B.
          XFR=      PRIMES+1,Y   ; Literal destination address -> Y.
          MVL                    ; Move long.
*
* Main loop. Register assignments:
*   A = Working register.
*   B = Working register.
*   X = Working register.
*   Y = Address of primes flags.
*   Z = Current factor.
*
* Start at factor 3.
          XFR=      3,Z       ; Literal 3 -> Z.
          XFR=      PRIMES,Y  ; Literal address of primes flags -> Y.
*
* Outer loop to search for the next prime and clear multiples thereof.
OUTER     EQU       *
*
* Find the next prime.
          XFR       Z,X       ; Z -> X.
          SRR       X,3       ; X / 8 -> X. X = index of byte containing flags.
          ADD       Y,X       ; Y + X -> X. X = address of ^^^.
          XFR       Z,B       ; Z -> B.
          AND=      7,B       ; Literal B % 8 -> B. B = index of bit in byte.
          ADD=      GMASKS,B  ; Literal GMASKS + B -> B. B = address of bitmask.
          LDBB+     B         ; Indexed bitmask (B) -> BL.
          LDAB+     X         ; Indexed flags byte (X) -> AL.
          ANDB      BL,AL     ; BL & AL -> AL.
          BNZ       CLRMULT   ; If the bit was set, branch to clear multiples.
          JMP       NXTPRIME  ; Relative jump to try the next prime.
*
* Clear multiples of this prime.
CLRMULT   XFR       Z,A       ; Z -> A.
          MUL       A,A       ; A * A -> A,B. Start at the factor squared.
          STB/      FACTORC   ; Direct B -> (FACTORC). Ignore A - no overflow.
          XFR       B,X       ; B -> X.
          XFR       Z,A       ; Z -> A.
          SLA                 ; A * 2 -> A. Double the factor.
          STA/      FACTOR2   ; Direct A -> (FACTOR2).
*
* Loop to clear each multiple.
CLRLOOP   XFR       X,B       ; X -> B.
          LDAB=     X'1F'     ; 0x1F -> AL. Mask for clearing after dividing.
          SRR       X,3       ; X / 8 -> X. Arithmetic, so need to clear bits.
          ANDB      AL,XU     ; AL & XU -> XU. X = ind of byte containing flags.
          ADD       Y,X       ; Y + X -> X. X = address of ^^^.
          AND=      7,B       ; Literal B % 8 -> B. B = index of bit in byte.
          ADD=      CMASKS,B  ; Literal CMASKS + B -> B. B = address of bitmask.
          LDBB+     B         ; Indexed bitmask (B) -> BL.
          LDAB+     X         ; Indexed flags byte (X) -> AL.
          ANDB      BL,AL     ; BL & AL -> AL.
          STAB+     X         ; Indexed A -> (X).
          LDX/      FACTORC   ; Direct (FACTORC) -> X.
          LDA/      FACTOR2   ; Direct (FACTOR2) -> A.
          ADD       A,X       ; A + X -> X. X = next multiple.
          BL        NXTPRIME  ; On overflow, must be past max prime.
          STX/      FACTORC   ; Direct X -> (FACTORC).
*** For max prime = 65535 (0xFFFF), comment out the next 4 lines as these
*** checks are unnecessary (though will work). For any other max prime, the
*** next 4 lines must be uncommented.
          LDA=      MXPRIME   ; Literal MXPRIME -> A.
          XFR       X,B       ; X -> B.
          SUB       A,B       ; A - B -> B.
          BNL       NXTPRIME  ; Max prime less than next multiple, break out.
          JMP CLRLOOP         ; Clear the next multiple.
*
* Skip to the next possible factor.
NXTPRIME  INR       Z,2       ; Z + 2 -> Z.
          LDA/      PROOT     ; Direct (PROOT) -> A.
          SUB       Z,A       ; Z - A -> A.
          BLE       OUTER     ; Loop if factor is less than/equal sqrt of prime.
*
* Skip past the first prime (2).
          JSR/      PRINTSTR  ; Jump to subroutine direct.
          DW        TWO       ; Address of string to print.
*
* Print primes and get the count. Register assignments:
*   A = Current bitmask (upper), current flags (lower).
*   B = Current prime.
*   X = Max prime.
*   Y = Address of primes flags, updated.
*   Z = Primes count.
          LDA=      X'0800'   ; Literal 0x08 (bit 3) -> AU, 0 -> AL.
          LDAB+     Y+        ; Indexed (Y) -> AL, then increment Y. 1st flags.
          LDB=      3         ; Literal 3 -> B.
          STB/      PNUM      ; Direct B -> (PNUM).
          XFR=      1,Z       ; Literal 1 -> Z.
PRLOOP    XAB                 ; A -> B.
          ANDB      BU,BL     ; BU & BL -> BL.
          BZ        NOTPRIME  ; If bit not set, skip past output.
          INR       Z         ; Z + 1 -> Z.
*** Comment out the next 10 lines to omit printing results.
          JSR/      PRINTSTR  ; Jump to subroutine direct.
          DW        COMMA     ; Address of string to print.
          MVF       (PLEN)/PFMT,/PBUFF ; Direct move PLEN bytes (PFMT)->(PBUFF).
          STK       A,4       ; Push A,B to the stack.
          LDAB=     PLEN      ; PLEN -> AL. Length of string to convert to.
          LDBB=     X'A0'     ; 0xA0 (' ') -> BL. Padding character.
          CFB       /PBUFF(10),/PNUM(2) ; Direct convert to base-10.
          POP       A,4       ; Pop B,A from the stack.
          JSR/      PRINTSTR  ; Jump to subroutine direct.
          DW        PBUFF     ; Address of string to print.
NOTPRIME  LDB/      PNUM      ; Direct load (PNUM) -> B.
          INR       B         ; B + 1 -> B.
          BZ        PDONE     ; On overflow, we're past the max prime.
          STB/      PNUM      ; Direct B -> (PNUM).
          XFR=      MXPRIME,X ; Literal MXPRIME -> X.
          SUB       X,B       ; X - B -> B.
          BNL       PDONE     ; Max prime less than prime candidate, break out.
          SLRB      AU        ; AU << 1 -> AU
          BNL       PRLOOP    ; If no overflow, loop for next bit in flags.
          INRB      AU        ; AU = 1. New mask for next flags.
          LDAB+     Y+        ; Indexed (Y) -> AL, then increment Y. Next flags.
          JMP       PRLOOP    ; Relative jump to continue printing.
PDONE     EQU       *
*
* Print the count.
          JSR/      PRINTSTR  ; Jump to subroutine direct.
          DW        CRLF      ; Address of string to print.
          JSR/      PRINTSTR  ; Jump to subroutine direct.
          DW        COUNT     ; Address of string to print.
          XFR       Z,A       ; Z -> A.
          STA/      PNUM      ; Direct A -> (PNUM).
          MVF       (PLEN)/PFMT,/PBUFF ; Direct move PLEN bytes (PFMT)->(PBUFF).
          LDAB=     PLEN      ; PLEN -> AL. Length of string to convert to.
          LDBB=     X'A0'     ; 0xA0 (' ') -> BL. Padding character.
          CFB       /PBUFF(10),/PNUM(2) ; Direct convert to base-10.
          JSR/      PRINTSTR  ; Jump to subroutine direct.
          DW        PBUFF     ; Address of string to print.
          JSR/      PRINTSTR  ; Jump to subroutine direct.
          DW        CRLF      ; Address of string to print.
*
* Enter infinite loop.
INFINITE  JMP       INFINITE  ; Relative jump to self.
*
* Print the null-terminated string pointed to by (X). Register assignments:
*   A = MUX status.
*   B = Next character.
*   X = RSR target.
*   Y = MUX status mask.
*   Z = Address of next character.
* See https://github.com/Nakazoto/CenturionComputer/wiki/MUX-Board#mux-mmio
PRINTSTR  STK       A,4       ; Push A,B to the stack.
          STK       Y,4       ; Push Y,Z to the stack.
          LDA+      X+        ; (X) -> A, then increment X.
          XAZ                 ; A -> Z.
          XFR=      X'0002',Y ; Literal bit 1 set -> Y.
PSLOOP    LDBB+     Z+        ; (Z) -> BL, then increment Z.
          BZ        PSEND     ; Branch if zero.
PSWAIT    LDAB/     X'F200'   ; Load direct 0xF200 -> A.
          ANDB      YL,AL     ; YL & AL -> AL
          BZ        PSWAIT    ; Branch if bit was not set.
          STBB/     X'F201'   ; Store direct B -> 0xF201.
          JMP       PSLOOP    ; Jump relative for next character.
PSEND     POP       Y,4       ; Pop Z,Y from the stack.
          POP       A,4       ; Pop B,A from the stack.
          RSR                 ; Return from subroutine.
*
* End of source.
          END       ENTRY     ; Specify the entrypoint.

