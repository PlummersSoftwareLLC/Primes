memory layout =
{
set 0   bit = 1
set 1   go_to = 0
use     check_1 = 1
use 2   check = 1
use 3   first_zero = 0
use 4   last_zero = 0
set 5   go_back = 0
ign 6
ign 7

repeat the last 8
}

>+++< set go_to to 3
>>>> >+++< <<<< set go_back to 3

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

++ set bit = 2

>>>> >[ while go_back != 0
    copy go_back to previous array index
        - sub go_back
        [ while go_back != (0)
            <<<< <<<<+>>>> >>>> add go_back
        -]

    <<<< <<<< go to previous array index
]< <<<<
