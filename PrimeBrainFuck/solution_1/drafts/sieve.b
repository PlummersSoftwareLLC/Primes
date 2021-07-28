++ ++ ++ ++ ++
[>++ ++ ++ ++ ++<-]
>[>++ ++ ++ ++ ++<-]
>[<<+>>-]
<<

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

All cells are set to 0 by default; thus we
can use true = 0; false = 1; Any bit that is
prime will be left as zero; So counting the
number of primes is counting the number of
zeros

Every "cell" or memory address used for the
sieve is listed below; Each cell has a tag
at the start:
"set" = an input value
"out" = an output value
"use" = value reserved to use when run
"ign" = ignored/unused value

memory layout =
{
set 0   q = 1000
set 1   factor_copy_1 = factor
set 2   factor = 3
set 3   factor_copy_2 = factor
set 4   multiplier_1 = 3
out 5   multiplicand_1 = factor
set 6   multiplier_2 = 2
out 7   multiplicand_2 = factor

set 8   num_step_copy_1 = num_step
set 9   num_step = factor * 2
set 10  num_step_copy_2 = num_step
set 11  num_copy_1 = num
set 12  num = factor * 3
set 13  num_copy_2 = num
set 14
set 15  
}
