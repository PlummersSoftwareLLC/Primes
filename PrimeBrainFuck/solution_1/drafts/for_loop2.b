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
ign 13  
ign 14  
ign 15 

cpy 16  num_copy_1 = num
use 17  check = 1
set 18  step = 0
use 19  first_zero = 0
use 20  last_zero = =
cpy 21  step_copy_1 = step
cpy 22  step_copy_2 = step
cpy 23  step_copy_3 = step

cpy 24  step_copy_4 = step
ign 25
ign 26
ign 27
ign 28
ign 29
ign 30
ign 31

primes are stored after this point
use 32
}


set factor = 999
    ++ ++ ++ ++ ++
    [>++ ++ ++ ++ ++<-]
    >[>++ ++ ++ ++ ++<-]
    >[<<+>>-]
    <<-

set num = 1'000'000
    >>>> >>>> >> go to num
    ++ ++ ++ ++ ++
    [>++ ++ ++ ++ ++<-]
    >[>++ ++ ++ ++ ++<-]
    >[>++ ++ ++ ++ ++<-]
    >[>++ ++ ++ ++ ++<-]
    >[>++ ++ ++ ++ ++<-]
    >[<<<<<+>>>>>-]
    <<<<<
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
    -]<<< <<<< go to 0

multiply multiplier_1 by 3
    >>>> >>>>[ go to multiplier_1
        >+++<  add 3 to start
    -]<<<< <<<< go to 0

multiply multiplier_2 by 2
>>>> >>>> >>>[ go to multiplier_2
    >++< add 2 to step
-]<<< <<<< <<<< go to 0

subtract start from num
    >>>> >>>> >[ go to start
        >-< sub num
    -]< <<<< <<<< go to 0

copy num to num_copy_1
    >>>> >>>> >>[ go to 10
        >>>> >>+<< <<<< add num_copy_1
    -]<< <<<< <<<< go to 0

.

>>>> >>>> >>>> >>>> go to num_copy_1
[
    copy step to step_copy_1 2 3 and 4
    <<<<[ go to 12
        >>>> >>>> >+ add step_copy_1
        >+ add step_copy_2
        >+ add step_copy_3
        >+ add step_copy_4
        <<<< <<<< <<<<
    -]>>>> go to 0

    >+< set check = 1
    >> go to 2

    [>>] if step != 0: go to last_zero
    < go to check or to first_zero

    [ if at check: run the following code once
        >>>> >[ go to step_copy_2
            <<<< <<+>> >>>> add num_copy_1
        -]< <<<< go to check
        
        <.> print num_copy_1

        >>>> >>[ go to step_copy_3
            <<<< <<<->>> >>>> sub num_copy_1
        -]<< <<<< go to check

        . print factor
        >- sub check
        > go to step 
        >>>> >>[ go to step_copy_4
            <<<< <<+>> >>>> add step
        -]<< <<<<

        >>>[ go to step_copy_1
            <<<< <<<< <+> >>>> >>>> add step
        -]<<<

        > go to first_zero
    ]
    
    <- sub step
    <<- sub num_copy_1
]
