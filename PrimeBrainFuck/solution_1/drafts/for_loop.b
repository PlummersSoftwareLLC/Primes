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
cpy 14  step_copy_2 = step
cpy 15  num_cpy_1 = num

cpy 16  num_cpy_2 = num
use 17  check = 1
set 18  step = 0
use 19  first_zero = 0
use 20  last_zero = =
ign 21
ign 22
ign 23
}

set factor = 1000 minus 3
    ++ ++ ++ ++ ++
    [>++ ++ ++ ++ ++<-]
    >[>++ ++ ++ ++ ++<-]
    >[<<+>>-]
    <<---
    
[
    >+< set check = 1
    >> go to 2

    [>>] if step != 0: go to last_zero
    < go to check or to first_zero

    [ if at check: run the following code once
        <++.--> print factor
        - sub check
        >++ set step = 2
        > go to first_zero
    ]
    
    <- sub step
    <<- sub factor
]
