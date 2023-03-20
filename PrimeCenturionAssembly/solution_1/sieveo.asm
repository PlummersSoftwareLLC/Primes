* Sieve of Eratosthenes by ren14500. Public domain.
*
* This version runs under OPSYS and uses SVC calls for time and I/O.
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
* Constants for formatting.
FMTC      EQU       2
FMTN      EQU       X'F9'
*
* Constants for services.
SVCTS     EQU       5         ; Load transient module.
SVCEXIT   EQU       10        ; Exit to OJX00.
SVCDT     EQU       11        ; Get binary date.
SVCGB     EQU       27        ; Get binary time.
SVCWN     EQU       49        ; CPL write without newline.
SVCWF     EQU       53        ; CPL write formatted.
SVCOP     EQU       57        ; CPL open.
SVCCT     EQU       58        ; CPL control function.
SVCPBKV   EQU       93        ; Set OS block value.
*
* Set the program title and begin.
          TITLE     'SIEVEO'
ZSIEVEO   BEGIN     0
*
* Reserve space for the stack.
          DS        100
STKTOP    EQU       *
*
* Declare the required variables for formatted I/O SVC calls. These must appear
* in order as they represent a structure.
LLMX      EQU       80        ; Maximum line length.
LMXPTR    DW        @LMX      ; Pointer to word containing max line length.
ZERO      DB        0         ; Zero byte.
STATUS    DB        0,3       ; Status bytes.
@IF       DB        0
@LMX      DW        LLMX      ; Word holding max line length.
@LL       DW        0         ; Current formatted line length.
@LI       DS        LLMX      ; Line buffer.
          DW        0         ; Null terminator.
LMXPTRPTR DW        LMXPTR    ; Pointer to pointer to max line length.
*
* Export required formatted I/O variables.
          ENT       ZERO
          ENT       STATUS
          ENT       @IF
          ENT       @LMX
          ENT       @LL
          ENT       @LI
*
* Declare the required variables for time SVC calls. Export them.
DATEI     DW        0,2       ; 32-bit integer date.
          ENT       DATEI
*
* Format for displaying prime numbers: 1 text character and 7 character
* integer. Equal to C1,N7 in CPL.
PRIMEFMT  DB        FMTC      ; C
          DW        1         ; 1
          DB        FMTN      ; N
          DW        7         ; 7
          DB        0         ; List terminator.
*
* Format for displaying up to 80 characters of text. Equal to C80 in CPL.
LINEFMT   DB        FMTC      ; C
          DW        80        ; 80
          DB        0         ; List terminator.
*
* Format for displaying the count: 6 text characters and 9 character integer.
* Equal to C6,N9 in CPL.
COUNTFMT  DB        FMTC      ; C
          DW        6         ; 6
          DB        FMTN      ; N
          DW        9         ; 9
          DB        0         ; List terminator.
*
* Format for displaying the summary: 11 text characters, 9 character integer,
* 1 text character, 1 character integer, 2 text characters. Equal to
* C11,N9,C1,N1,C2 in CPL.
SUMMFMT   DB        FMTC      ; C
          DW        11        ; 11
          DB        FMTN      ; N
          DW        9         ; 9
          DB        FMTC      ; C
          DW        1         ; 1
          DB        FMTN      ; N
          DW        1         ; 1
          DB        FMTC      ; C
          DW        2         ; 2
          DB        0         ; List terminator.
*
* File Control Block (FCB) for the console.
CRT       DB        0
          DB        X'81'     ; Device. 0x81 = SYSIPT.
          DW        0
          DW        0         ; Buffer size.
          DW        0         ; Buffer address.
          DW        0
          DW        0
          DB        0         ; Class. 0 = console device.
          DB        2         ; Type. 2 = type 'A' ASCII file.
          DB        0,12
          DW        0
          DW        0
          ENT       CRT       ; Declare it as an external entrypoint.
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
TWO       DC        '      2'
EMPTY     DB        0
COMMA     DC        ','
          DB        0
COUNT     DC        'COUNT:'
          DB        0
AUTHOR    DC        'REN14500;1;'
          DB        0
PERIOD    DC        '.'
          DB        0
ONE       DC        ';1'
          DB        0
PMAX32    DW        0         ; Max prime upper word.
PMAX16    DW        MXPRIME   ; Max prime lower word.
PROOT     DW        RTPRIME   ; Square root of prime.
TEN       DW        0         ; Upper word of 32-bit value 10.
          DW        10        ; Lower word of 32-bit value 10.
*
* Variables.
PRIMES    DS        FLAGBYTES ; Flags. Bits set to 0 are not prime.
STARTTIME DS        4         ; Start time in tenths of a second.
ENDTIME   DS        4         ; End time in tenths of a second.
DIFF      DS        4         ; Time difference.
DIFFSEC   DS        4         ; Time difference seconds.
DIFFTENTH DS        4         ; Time difference tenths.
FACTORC   DS        2         ; Factor current.
FACTOR2   DS        2         ; Factor doubled.
PCOUNT    DS        4         ; Count of primes.
PNUM      DS        4         ; Prime number.
*
* Entrypoint. Set the stack pointer. Register the SVC call data.
ENTRY     XFR=      STKTOP,S  ; Literal STKTOP -> S.
          SVC       SVCPBKV   ; Set OS block value.
          DW        2
          DB        25
          DW        LMXPTRPTR ; Pointer to the pointer to the line length word.
          SVC       SVCDT     ; Get binary date.
          SVC       SVCTS     ; Load transient module.
          DB        10        ; 10 = integer date.
          DW        DATEI     ; Address of 32-bit integer date.
*
* Open the console device.
          SVC       SVCOP     ; CPL open.
          DB        3         ; 3 = input/output.
          DW        CRT       ; Device to open.
          DB        0         ; List terminator.
*
* Get the start time.
          SVC       SVCGB     ; Get binary time.
          DW        STARTTIME ; Address of 32-bit integer to receive time.
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
*** Uncomment the next line if you get an abort due to watchdog timeout.
***       SVC       SVCDT     ; Service call to avoid watchdog. Clobbers A.
          LDX/      FACTORC   ; Direct (FACTORC) -> X.
          LDA/      FACTOR2   ; Direct (FACTOR2) -> A.
          ADD       A,X       ; A + X -> X. X = next multiple.
          BL        NXTPRIME  ; On overflow, must be past max prime.
          STX/      FACTORC   ; Direct X -> (FACTORC).
*** For max prime = 65535 (0xFFFF), comment out the next 4 lines as these
*** checks are unnecessary (though will work). For any other max prime, the
*** next 4 lines must be uncommented.
***       LDA/      PMAX16    ; Direct (PMAX16) -> A.
***       XFR       X,B       ; X -> B.
***       SUB       A,B       ; A - B -> B.
***       BNL       NXTPRIME  ; Max prime less than next multiple, break out.
          JMP CLRLOOP         ; Clear the next multiple.
*
* Skip to the next possible factor.
NXTPRIME  SVC       SVCDT     ; Service call to avoid watchdog. Clobbers A.
          INR       Z,2       ; Z + 2 -> Z.
          LDA/      PROOT     ; Direct (PROOT) -> A.
          SUB       Z,A       ; Z - A -> A.
          BLE       OUTER     ; Loop if factor is less than/equal sqrt of prime.
*
* Get the end time.
          SVC       SVCGB     ; Get binary time.
          DW        ENDTIME   ; Address of 32-bit integer to receive time.
*
* Calculate the time difference in seconds and tenths.
          ZAD       /ENDTIME(4),/DIFF(4)   ; Direct 32-bit end -> diff.
          S         /STARTTIME(4),/DIFF(4) ; Direct 32-bit start - diff -> diff.
          LDA=      DIFFTENTH              ; Literal address of DIFFTENTH -> A.
          ZAD       /DIFF(4),/DIFFSEC(4)   ; Direct 32-bit diff -> secs.
          DRM       /TEN(4),/DIFFSEC(4) ; Direct secs/10 -> secs, rem -> tenths.
*
* Skip past the first prime (2).
          SVC       SVCWN     ; CPL write without newline.
          DW        CRT       ; Address of FCB to write to.
          DW        LINEFMT   ; Address of format.
          DW        TWO       ; Address of argument.
          DW        0         ; List terminator.
*
* Print primes and get the count. Register assignments:
*   A = Current bitmask (upper), current flags (lower).
*   B = Working register.
*   X =
*   Y = Address of primes flags, updated.
*   Z =
          LDA=      X'0800'   ; Literal 0x08 (bit 3) -> AU, 0 -> AL.
          LDAB+     Y+        ; Indexed (Y) -> AL, then increment Y. 1st flags.
          ZAD       =3,/PNUM(4)   ; Literal/direct 3 -> 32-bit PNUM.
          ZAD       =1,/PCOUNT(4) ; Literal/direct 1 -> 32-bit PCOUNT.
PRLOOP    XAB                 ; A -> B.
          ANDB      BU,BL     ; BU & BL -> BL.
          BZ        NOTPRIME  ; If bit not set, skip past output.
          A         =1,/PCOUNT(4) ; Literal/direct PCOUNT + 1 -> PCOUNT.
          STK       A,2       ; Push A.
*** Comment out the next 6 lines to omit printing results.
          SVC       SVCWN     ; CPL write without newline.
          DW        CRT       ; Address of FCB to write to.
          DW        PRIMEFMT  ; Address of format.
          DW        COMMA     ; Address of argument.
          DW        PNUM      ; Address of argument.
          DW        0         ; List terminator.
*** And uncomment the next line instead.
***       SVC       SVCDT     ; Service call to avoid watchdog. Clobbers A.
          POP       A,2       ; Pop A.
NOTPRIME  A         =1,/PNUM(4)   ; Literal/direct 32-bit PNUM + 1 -> PNUM.
          C         /PNUM(4),/PMAX32(4) ; Direct 32-bit compare PMAX32 to PNUM.
          BM        PDONE     ; If PNUM greater than PMAX32, we're done.
          SLRB      AU        ; AU << 1 -> AU
          BNL       PRLOOP    ; If no overflow, loop for next bit in flags.
          INRB      AU        ; AU = 1. New mask for next flags.
          LDAB+     Y+        ; Indexed (Y) -> AL, then increment Y. Next flags.
          JMP       PRLOOP    ; Relative jump to continue printing.
PDONE     EQU       *
*
* Print the count.
          SVC       SVCWF     ; CPL write formatted.
          DW        CRT       ; Address of FCB to write to.
          DW        LINEFMT   ; Address of argument.
          DW        EMPTY     ; Address of argument.
          DW        0         ; List terminator.
          SVC       SVCWF     ; CPL write formatted.
          DW        CRT       ; Address of FCB to write to.
          DW        COUNTFMT  ; Address of argument.
          DW        COUNT     ; Address of argument.
          DW        PCOUNT    ; Address of argument.
          DW        0         ; List terminator.
*
* Print summary.
          SVC       SVCWF     ; CPL write formatted.
          DW        CRT       ; Address of FCB to write to.
          DW        LINEFMT   ; Address of argument.
          DW        EMPTY     ; Address of argument.
          DW        0         ; List terminator.
          SVC       SVCWF     ; CPL write formatted.
          DW        CRT       ; Address of FCB to write to.
          DW        SUMMFMT   ; Address of argument.
          DW        AUTHOR    ; Address of argument.
          DW        DIFFSEC   ; Address of argument.
          DW        PERIOD    ; Address of argument.
          DW        DIFFTENTH ; Address of argument.
          DW        ONE       ; Address of argument.
          DW        0         ; List terminator.
*
* Close the console.
          SVC       SVCCT     ; CPL control function.
          DB        10        ; 10 = close.
          DW        CRT       ; Address of FCB to control.
*
* Exit.
          SVC       SVCEXIT   ; Exit to OJX00.
          DB        0         ; Completion code.
*
* End of source.
          END       ENTRY     ; Specify the entrypoint.

