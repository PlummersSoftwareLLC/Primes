********************************************
BrainFuck Prime Sieve 
by aquarel 8/1/21

This file only contains the core function
of the prime sieve; Which is equivalent to 
the "runSieve" method in other
implementations; The setup code and printing
results is handled by PrimeBrainFuck*cpp
********************************************


PART #1 = 
"""
int q = (int) sqrt(sieveSize) plus 1;
"""

Aka a sad excuse for a square root function
It just needs to approximate above the real
value to not miss any number

Using the newton iteration as follows:
Use x as a first guess; y0 = x
y1 = (y0 add x / y0) / 2
y2 = (y1 add x / y1) / 2
y3 = (y3 add x / y3) / 2
and so on for 6 times
y6 = (y5 add x / y5) / 2
y6 add 1 = sqrt(x)

Every "cell" or memory address used for one
iteration is listed below; Each cell has a
tag at the start which dictates its function:
"set" = an input value
"out" = an output value
"use" = value reserved to use when run
"cpy" = a copy of a value
"ign" = ignored/unused value

memory layout = 
{
set 0   x = 1000
set 1   dividend = x
out 2   remainder = x % y0
set 3   divisor = y0
out 4   quotient = x / y0
use 5   first_zero = 0
use 6   last_zero = 0
ign 7

set 8   add = y0 plus quotient
set 9   dividend = add
out 10  remainder = add % 2
set 11  divisor = 2
out 12  y1 = add / 2
use 13  first_zero = 0
use 14  last_zero = 0
cpy 15  copy_of_y1
}

96 cells (384 bytes of memory) are used in
this step

iteration #1
    set x = (1000)
        ++ ++ ++ ++ ++
        [>++ ++ ++ ++ ++<-]
        >[>++ ++ ++ ++ ++<-]
        >[ copy value of 2
            <+<+>> to 0 and 1 (x and dividend)
            >>>> >>>> >>+<< <<<< <<<< to 12 (y1)
        -]<< go to 0

    copy y1 to divisor and copy_of_y1
        >>>> >>>> >>>>[ copy value of 12
            <<<< <<<< <+> >>>> >>>> to 3 (divisor)
            >>>+<<< to 15 (copy of y1)
        -]<<<< <<<< <<<< go to 0

    do first division (credit goes to u/danielcristofani)
    https://www*reddit*com/r/brainfuck/comments/dwdboo/division_in_brainfuck/
        >[ while dividend != 0
            >+ add remainder
            >- sub divisor

            [>>>] if divisor != 0: go to last_zero
            < go to remainder or to first_zero

            [ if at remainder: run the following code once
                [ while remainder != 0
                    >+ add divisor
                    <- sub remainder
                ]
                >>+ add quotient
                > go to first_zero
            ]
            <<<<- sub dividend
        ]<

    copy quotient and copy_of_y1 to add
    effectively adding quotient and copy_of_y1
        >>>>[ copy value of 4
            >>>>+<<<< to 8
        -]<<<< go to 0
        >>>> >>>> >>>> >>>[ copy value of 15
            <<<< <<<+>>> >>>> to 8
        -]<<< <<<< <<<< <<<< go to 0

    copy add to dividend
        >>>> >>>>[ copy value of 8
            >+< to 9
        -]<<<< <<<< go to 0

    set divisor to 2
        >>>> >>>> >>>++<<< <<<< <<<<

    do second division (credit goes to u/danielcristofani)
    https://www*reddit*com/r/brainfuck/comments/dwdboo/division_in_brainfuck/
        >>>> >>>> >[ go to 9 (second dividend)
            >+>-[>>>]<[[>+<-]>>+>]<<<< same as previous division
        -]< <<<< <<<< go to 0

iteration #2 (the same code without comments and with a couple changes)
    >>>> >>>> >>>> >>>> go to 16

    set x = (1000)
    ++++++++++[>++++++++++<-]>[>++++++++++<-]>[<+<+>>-]<<

    copy previous result to divisor and copy_of_y1
        <<<<[ copy value of previous 12
            >>>> >>>+ to 3 (divisor)
            >>>> >>>> >>>>+<<< <<<< <<<< <<<< to 15 (copy of y1)
            <<<< go to previous 12
        -]>>>> go to 0

    simplified code (removed redundant data pointer shifts)
    >[>+>-[>>>]<[[>+<-]>>+>]<<<<-]>>>[>>>>+<<<<-]>>>>>>>>>>>[<<<<<<
    <+>>>>>>>-]<<<<<<<[>+<-]>>>++<<[>+>-[>>>]<[[>+<-]>>+>]<<<<-]

the same code as iteration #2 (simplified and compacted)
I was also too lazy to write a loop
iteration #3
    >>>>>>>++++++++++[>++++++++++<-]>[>++++++++++<-]>[<+<+>>-]<<<<<
    <[>>>>>>>+>>>>>>>>>>>>+<<<<<<<<<<<<<<<<<<<-]>>>>>[>+>-[>>>]<[[>
    +<-]>>+>]<<<<-]>>>[>>>>+<<<<-]>>>>>>>>>>>[<<<<<<<+>>>>>>>-]<<<<
    <<<[>+<-]>>>++<<[>+>-[>>>]<[[>+<-]>>+>]<<<<-]
iteration #4
    >>>>>>>++++++++++[>++++++++++<-]>[>++++++++++<-]>[<+<+>>-]<<<<<
    <[>>>>>>>+>>>>>>>>>>>>+<<<<<<<<<<<<<<<<<<<-]>>>>>[>+>-[>>>]<[[>
    +<-]>>+>]<<<<-]>>>[>>>>+<<<<-]>>>>>>>>>>>[<<<<<<<+>>>>>>>-]<<<<
    <<<[>+<-]>>>++<<[>+>-[>>>]<[[>+<-]>>+>]<<<<-]
iteration #5
    >>>>>>>++++++++++[>++++++++++<-]>[>++++++++++<-]>[<+<+>>-]<<<<<
    <[>>>>>>>+>>>>>>>>>>>>+<<<<<<<<<<<<<<<<<<<-]>>>>>[>+>-[>>>]<[[>
    +<-]>>+>]<<<<-]>>>[>>>>+<<<<-]>>>>>>>>>>>[<<<<<<<+>>>>>>>-]<<<<
    <<<[>+<-]>>>++<<[>+>-[>>>]<[[>+<-]>>+>]<<<<-]
iteration #6
    >>>>>>>++++++++++[>++++++++++<-]>[>++++++++++<-]>[<+<+>>-]<<<<<
    <[>>>>>>>+>>>>>>>>>>>>+<<<<<<<<<<<<<<<<<<<-]>>>>>[>+>-[>>>]<[[>
    +<-]>>+>]<<<<-]>>>[>>>>+<<<<-]>>>>>>>>>>>[<<<<<<<+>>>>>>>-]<<<<
    <<<[>+<-]>>>++<<[>+>-[>>>]<[[>+<-]>>+>]<<<<-]

>>> go to factor
--- sub (3) from factor

PART #2 =
"""
for (int factor = 3;
    factor smaller_than q;
    factor = factor plus 2){

    for (int num = factor * 3;
        num smaller_than sieve_size;
        num = num plus factor * 2){

        rawbits(index // 2) = false;
    }
}
"""

This step is the prime sieve; The two nested
loops are a simplified version of the 
original python implementation by davepl 
(PrimePython/solution_1/PrimePY*py); It 
checks for all factors and does not skip any
The performance of this is sub optimal;
You can check out the more readable version
of this here (replace the stars with dots):
https://github*com/ThatAquarel/Primes/blob/
brainfuck_solution/PrimeBrainFuck/
solution_1/drafts/sieve*py

Also links for the seperated code of these
two loops (replace the stars with dots):
First loop
https://github*com/ThatAquarel/Primes/blob/
brainfuck_solution/PrimeBrainFuck/
solution_1/drafts/for_loop*b
Second loop
https://github*com/ThatAquarel/Primes/blob/
brainfuck_solution/PrimeBrainFuck/
solution_1/drafts/for_loop2*b

Every "cell" or memory address used for one
iteration is listed below; Each cell has a
tag at the start which dictates its function:
"set" = an input value
"out" = an output value
"use" = value reserved to use when run
"cpy" = a copy of a value
"ign" = ignored/unused value

memory layout =
{
set 0   factor = 1000 minus 3
use 1   check = 1
set 2   step = 0
use 3   first_zero = 0
use 4   last_zero = 0
cpy 5   add_copy_1 = add
cpy 6   add_copy_2 = add
cpy 7   factor_copy = factor

set 8   multiplier_1 = factor
out 9   start = multiplier_1 * 3
set 10  num = 1000 minus start
set 11  multiplier_2 = factor
out 12  step = multiplier_2 * 2
cpy 13  step_copy_1 = step
        start_copy_1 = start
cpy 14  start_copy_2 = start
ign 15  

cpy 16  num_copy_1 = num
use 17  check = 1
set 18  step = 0
use 19  first_zero = 0
use 20  last_zero = 0
set 21  dividend = num
out 22  remainder = num % step
set 23  divisor = step

out 24  quotient = num / step
use 25  first_zero = 0
use 26  last_zero = 0
cpy 27  remainder_copy_1 = remainder
cpy 28  remainder_copy_2 = remainder
ign 29
ign 30
ign 31
}

32 cells (128 bytes) are used by the loops
8000 cells (32000 bytes) are used by the 
prime array

FIRST FOR LOOP
[
    >+< set check = 1
    >> go to 2

    [>>] if step != 0: go to last_zero
    < go to check or to first_zero

    [ if at check: run the following code once
        <++ add (2) to factor
        
        SECOND FOR LOOP
            set num = 1000
                >>>> >>>> >> go to num
                ++ ++ ++ ++ ++
                [>++ ++ ++ ++ ++<-]
                >[>++ ++ ++ ++ ++<-]
                >[<<+>>-]
                <<
                << <<<< <<<< go to 0

            copy factor to factor_copy; multiplier_1 and multiplier_2
                [
                    >>>> >>>+ add factor_copy
                    >+ add multiplier_1
                    >>>+ add multiplier_2
                    <<< <<<< <<<< go to factor
                -]

            copy factor_copy to factor
                >>>> >>>[
                    <<<< <<<+>>> >>>> add factor
                -]

            multiply multiplier_1 by 3
                 >[ go to multiplier_1
                    >+++<  add 3 to start
                -]

            multiply multiplier_2 by 2
                >>>[ go to multiplier_2
                    >++< add 2 to step
                -]

            copy start to start_copy_1
            subtract start from num
                <<[ go to start
                    >- sub num
                    >>>+ add start_copy_1
                    <<<< go to start
                -]

            copy start_copy_1 to start
                >>>>[ go to start_copy_1
                    <<<<+>>>> add start
                -]

            copy num to num_copy_1 qnd dividend
                 <<<[ go to 10
                    >>>> >>+ add num_copy_1
                    >>>> >+ add dividend
                    <<<< <<<< <<< go to num
                -]

            copy step to step_copy_1 and divisor
                 >>[ go to step
                    >+ add step_copy_1
                    >>>> >>>> >>+ add divisor
                    <<<< <<<< <<< go to step
                -]
            copy step_copy_1 to step
                >[ go to step_copy_1
                    <+> add step
                -]

            divide dividend by divisor (credit goes to u/danielcristofani)
            https://www*reddit*com/r/brainfuck/comments/dwdboo/division_in_brainfuck/
                >>>> >>>> go to dividend
                [>+>-[>>>]<[[>+<-]>>+>]<<<<-] divide

            <<<< <[ go to num_copy_1
                >+< set check = 1
                >> go to 2

                [>>] if step != 0: go to last_zero
                < go to check or to first_zero

                [ if at check: run the following code once
                    copy start to start_copy_1 2 and 3
                    add start to num_copy_1
                        <<<< <<<<[ go to start
                            >>>>+ add start_copy_1
                            >+ add start_copy_2
                            >>+ add num_copy_1
                            <<<< <<< go to start
                        -]
                    copy start_copy_1 to start
                        >>>>[ go to start_copy_1
                            <<<<+>>>> add start
                        -]>>>> go to check

                    copy remainder to remainder_copy_1 and 2
                    sub remainder from num_copy_1
                        >>>> >[ go to remainder
                            <<<< <<->> >>>> sub num_copy_1
                            >>>> >+ add remainder_copy_1
                            >+ add remainder_copy_2
                            <<<< << go to remainder
                        -]
                    copy remainder_copy_1 to remainder
                        >>>> >[ go to remainder_copy_1
                            <<<< <+> >>>> add remainder
                        -]<< <<<< <<<< go to check

                    PRIME ARRAY

                    memory layout =
                    {
                    set 0   data = 1
                    set 1   go_to = 0
                    use 2   first_zero = 0
                    use 3   last_zero = 0
                    set 4   prime_count = 0
                    set 5   go_back = 0
                    ign 6
                    use 7   check = 1

                    repeat for 999 more times
                    }

                    < go to num_copy_1
                        copy num_copy_1 to num_copy_2; go_to and go_back
                            [
                                >>>> >>>> >>>> >+ add num_copy_2
                                >>>>+ add go_to of first array index
                                >>>>+ add go_back of first array index
                                <<<< <<<< <<<< <<<< <<<< < go to num_copy_1
                            -]
                        copy num_copy_2 to num_copy_1;
                            >>>> >>>> >>>> >[ go to num_copy_2
                                <<<< <<<< <<<< <+> >>>> >>>> >>>> add num_copy_1
                            -]< <<<< <<<< <<<< go to num_copy_1

                        go to array index
                            >>>> >>>> >>>> >>>> go to first array index
                            >[ while go_to != zero
                                copy go_to to next array index
                                    - sub go_to
                                    [ while go_to != (0)
                                        >>>> >>>>+<<<< <<<< add go_to
                                    -]
                                copy go_back to next array index
                                    >>>>[ while go_back != (0)
                                        >>>> >>>>+<<<< <<<< add go_back
                                    -]<<<<

                                >>>> >>>> go to next array index
                            ]<
                        add (1) to array index
                            + set bit = 2
                        go back
                            >>>> >[ while go_back != 0
                                copy go_back to previous array index
                                    - sub go_back
                                    [ while go_back != (0)
                                        <<<< <<<<+>>>> >>>> add go_back
                                    -]

                                <<<< <<<< go to previous array index
                            ]< <<<<
                            <<<< <<<< <<<< <<<< go to num_copy_1
                    > go to check

                    add remainder_copy_2 to num_copy_1
                        >>>> >>>> >>>[ go to remainder_copy_1
                            <<<< <<<< <<<<+>>>> >>>> >>>> add num_copy_1
                        -]<<< <<<< <<<< go to check

                    sub start_copy_2 from num_copy_1
                        <<<[ go to start_copy_2
                            >>-<< add num_copy_1
                        -]

                    copy step to step_copy_1 and step
                        <<[ go to step
                            >+ add step_copy_1
                            >>>> >+ add step
                            <<<< << go to step
                        -]
                    copy step_copy_1 to step
                        >[ go to step_copy_1
                            <+> add step
                        -]>>>> go to check

                    - sub check
                    >> go to first_zero
                ]

                <- sub step
                <<- sub num_copy_1
            ]

            clear memory for next iteration of loop
                >[-] set check = 0
                >[-] set step = 0

                >>>>[-] set remainder = 0
                >[-] set divisor = 0
                >[-] set quotient = 0
                
                <<<< << go to step

                <<<< <<[-] set step = 0
                <<<[-] set start = 0
            <<<< <<<< < go to 0

        --> sub (2) from factor
        - sub check
        >++ set step = 2
        > go to first_zero
    ]
    
    <- sub step
    <<- sub factor
]

PART #3 =
"""
int count = 0;
for (int i = 0; i smaller_than 1000; i = i plus 1) {
    if (rawbits at index i == true) {
        count = count plus 1;
    }
}

return count;
"""

This final step counts all the primes; The
current index equals 0 if it is prime and
larger than 0 if it is composite; The count
is incremented each time it meets a prime;
Half the total (500) is subtracted after
because we haven't ruled out the even 
numbers yet

Every "cell" or memory address used for one
iteration is listed below; Each cell has a
tag at the start which dictates its function:
"set" = an input value
"out" = an output value
"use" = value reserved to use when run
"cpy" = a copy of a value
"ign" = ignored/unused value

memory layout =
{
first_index:
set 0   data = 1
set 1   go_to = 0
use 2   first_zero = 0
use 3   last_zero = 0
set 4   prime_count = 0
set 5   go_back = 0
ign 6
use 7   check = 1
repeat for 998 more times

last index:
set 0
set 1
use 2
use 3
set 4   prime_count
set 5   half_total = 500
ign 6
use 7
}


>>>> >>>> >>>> >>>> >>>> >>>> >>>> >>>> go to first array index
> set go_to to 1000
    ++ ++ ++ ++ ++
    [>++ ++ ++ ++ ++<-]
    >[<++ ++ ++ ++ ++>-]<
<

>[ while go_to != zero
    < go to data
    <+> set previous check = 1

    [>>>] if data != 0: go to last_zero
    < go to previous check or to first_zero

    [ if at previous check: run the following code once
        - sub previous check
        > go to data
        >>>>+ add prime_count
        << go to first_zero
    ]

    < go to go_to

    copy prime_count to next array index
        >>>[
            >>>> >>>>+<<<< <<<< add next prime_count
        -]<<<

    copy go_to to next array index
        - sub go_to
        [ while go_to != (0)
            >>>> >>>>+<<<< <<<< add next go_to
        -]

    >>>> >>>> go to next array index
]<

>>>> go to prime_count

> set half_total = 500
++ ++ ++ ++ ++
[>++ ++ ++ ++ ++<-]
>[<++ ++ +>-]<

[<->-] sub half_total from prime_count
which removes all the even numbers
< return prime_count

Performance:
    Testing platform info:
    CPU: i9 9900K @ 3*60 GHZ
    OS HOST: Windows 10 21h1 (build 19043*1110)
    OS GUEST: WSL version 2; Ubuntu 18*04

    Averages 53 seconds per run
