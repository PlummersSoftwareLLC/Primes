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

>>>> >>>> give 8 cells of space before

SET THIRD INDEX OF ARRAY
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

+ set bit = 2

>>>> >[ while go_back != 0
    copy go_back to previous array index
        - sub go_back
        [ while go_back != (0)
            <<<< <<<<+>>>> >>>> add go_back
        -]

    <<<< <<<< go to previous array index
]< <<<<

SET SECOND INDEX OF ARRAY
>++< set go_to to 2
>>>> >++< <<<< set go_back to 2
>[-[>>>>>>>>+<<<<<<<<-]>>>>[>>>>>>>>+<<<<<<<<-]<<<<>>>>>>>>]<
++ set bit = 2
>>>>>[-[<<<<<<<<+>>>>>>>>-]<<<<<<<<]<<<<<

GO TO END OF ARRAY WHILE COUNTING
>+++< set go_to to 3
>>>> >+++< <<<< set go_back to 3

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
    copy go_back to next array index
        >>>>[ while go_back != (0)
            >>>> >>>>+<<<< <<<< add next go_back
        -]<<<<

    >>>> >>>> go to next array index
]<

>>>> go to prime_count
