********************************************
BrainFuck Prime Sieve 
by aquarel 7/23/21

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
and so on for 12 times
y12 = (y11 add x / y11) / 2
y12 add 1 = sqrt(x)

Every "cell" or memory address used for one
iteration is listed below; Each cell has a
tag at the start:
"set" = an input value
"out" = an output value
"use" = value reserved to use when run
"cpy" = a copy of a value
"ign" = ignored/unused value

memory layout = 
{
set 0   x = 1'000'000
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

Performance statistics:
192 cells (768 bytes) are used in this step
Takes ~5 seconds to run on i9 9900K in WSL

iteration #1
    set x = (1'000'000)
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

    ++++++++++[>++++++++++<-]>[>++++++++++<-]>[<+<+>>-]<<

    copy previous result to divisor and copy_of_y1
        <<<<[ copy value of previous 12
            >>>> >>>+ to 3 (divisor)
            >>>> >>>> >>>>+<<< <<<< <<<< <<<< to 15 (copy of y1)
            <<<< go to previous 12
        -]>>>> go to 0

    >[>+>-[>>>]<[[>+<-]>>+>]<<<<-]>>>[>>>>+<<<<-]>>>>>>>>>>>[<<<<<<
    <+>>>>>>>-]<<<<<<<[>+<-]>>>++<<[>+>-[>>>]<[[>+<-]>>+>]<<<<-]

I couldn't be bothered to write a loop
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
void clear_bit(int index){
    rawbits(index // 2) = false;
}

for (int factor = 3;
    factor smaller_than q;
    factor = factor plus 2){

    for (int num = factor * 3;
        num smaller_than sieve_size;
        num = num plus factor * 2){

        clear_bit(num);
    }
}
"""

This step is the core functionality of the
prime sieve; The two nested loops are a
simplified version of the original python
implementation by davepl 
(PrimePython/solution_1/PrimePY*py); It 
checks for all factors and does not skip any
The performance of this is sub optimal;

The first and second for loop are written
seperately in the files drafts/for_loop*b
and drafts/for_loop2*b; The two combined
is stored in drafts/for_loop3*b; PART #2
is a simplified version of the loops in
drafts/for_loop3_simplied*b; It contains
less unecessary steps

Every "cell" or memory address used for one
iteration is listed below; Each cell has a
tag at the start:
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
set 10  num = 1'000'000 minus start
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

Performance statistics:
32 cells (128 bytes) are used by the loops
8 000 000 cells (32 000 000 bytes) are used
by the prime array
Takes ~20 minutes to run on i9 9900K in WSL

FIRST FOR LOOP
[
    >+< set check = 1
    >> go to 2

    [>>] if step != 0: go to last_zero
    < go to check or to first_zero

    [ if at check: run the following code once
        <++ add (2) to factor
        .
        
        SECOND FOR LOOP
            set num = 1'000'000
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

            clear memory
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

>>>> >>>> >>>> >>>> >>>> >>>> >>>> >>>> go to first array index
> set go_to to 1 000 000
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

> set next one = 500 000
++ ++ ++ ++ ++
[>++ ++ ++ ++ ++<-]
>[<++ ++ +>-]<

[<->-] subtract next one from prime_count
which removes all the even numbers
< return prime_count
