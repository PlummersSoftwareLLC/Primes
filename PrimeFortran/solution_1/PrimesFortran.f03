program primes_fortran
    use iso_fortran_env
    implicit none

    logical, parameter :: show_results = .false.

    ! All bits are set to 1 for an integer value of -1 as in Fortran there are only signed numbers
    ! and the two's complement is used
    
    integer(int64), parameter :: all_true = int((-1), kind=int64)
    integer(int64), parameter :: num_possible_upper_limits = 6_int64 ! how many limits to test
    integer(int64) :: time_start, time_now, count_rate
    integer(int64) :: sieve_size
    integer(int32) :: passes
    integer(kind=int64), dimension (:), allocatable :: array_of_bits

    integer(int64), dimension(num_possible_upper_limits) :: prime_number_limits
    integer(int32), dimension(num_possible_upper_limits) :: prime_number_up_to_limit 

    
    prime_number_limits = [10_int64, &
                           100_int64, &
                           1000_int64, &
                           10000_int64, &
                           100000_int64, &
                           1000000_int64]

    
    prime_number_up_to_limit = [4_int32, &             ! 10
                                25_int32, &            ! 100
                                168_int32, &           ! 1000
                                1229_int32, &          ! 10000
                                9592_int32, &          ! 100000
                                78498_int32]           ! 1000000

    sieve_size = 1000000_int64
        
    call allocate_bitfield(sieve_size)

    passes = 0

    call system_clock(time_start, count_rate)

    do while (.true.)
        array_of_bits = all_true   ! sets each element of the array to the same value
        call run_sieve
        passes = passes + 1_int64
        call system_clock(time_now, count_rate)

	! if time is up
        if (real(time_now - time_start) / count_rate >= 5.0_real64) exit
    end do
    call print_results(real(time_now - time_start, real64) / count_rate, passes)    
    
contains
    subroutine allocate_bitfield (n)
        integer(kind=int64) :: n
        if (mod(n, 64) == 0) then
            allocate (array_of_bits(0:n / 64 - 1))                !integer division
        else
            allocate (array_of_bits(0:n / 64))
        endif
    end subroutine allocate_bitfield

    logical function getbit(bit)
        implicit none
        integer(int32) :: bit
        getbit = btest(array_of_bits(bit / 64), mod(bit, 64))
    end function getbit

    subroutine setbit_false(bit)
        implicit none
        integer(int32) :: bit
        array_of_bits(bit / 64) = ibclr(array_of_bits(bit / 64), mod(bit, 64))
    end subroutine setbit_false

    integer(int64) function count_primes(limit) result(prime_count)
        implicit none
	integer(int64), intent(in) :: limit
        integer(int32) :: i
        prime_count = 1_int32 ! 2 is prime
        do i = 3, limit, 2
            if (getbit(i)) prime_count = prime_count + 1
        end do
    end function count_primes

    logical function validate_results() result(correct)
        implicit none
        integer(kind=int64) :: upper_limit_counter
        correct = .true.
        
        do upper_limit_counter = 1_int64, num_possible_upper_limits
            if (count_primes(prime_number_limits(upper_limit_counter)) &
	        /= prime_number_up_to_limit(upper_limit_counter)) correct = .false.
        end do
    end function validate_results
    
    subroutine print_results(duration, passes)
        implicit none
        real(real64), intent(in) :: duration
        integer(int32),intent(in):: passes
        integer(int32):: count, i
        character(len=50) :: raw_string
        character(len=50) :: info_string
        
        integer:: raw_string_counter, info_string_counter
        
        count = 1
        if (show_results) then
	    write(*, '(I16)', advance='NO') 2
            do i = 3, sieve_size, 2
                if (getbit(i)) then
                    write(ERROR_UNIT, '(I10)', advance='NO') i
                    count = count + 1
                end if
            end do
        end if
        
        
        write(*, '("johandweber_fortran;",I0,";",F0.3,";1;algorithm=base,faithful=no,bits=1" )') passes, duration
        write(ERROR_UNIT,*)

        if (show_results) then
            write(ERROR_UNIT,'("Passes: ",I10, ", Time: ", F10.2, ", Avg: ",E10.2,&
                            ", Count1: ", I10, ", Count2 :", I10,", Valid :", L  )' ) &
                passes,&
                duration,&
                duration/passes,&
                count,&
                count_primes(sieve_size),&
                validate_results()
        end if
        
    end subroutine print_results
    
    subroutine run_sieve
        implicit none
        integer(kind=int32) :: factor, sqrt_limit, i
        
        factor = 3_int32
        sqrt_limit = int(sqrt(real(sieve_size)), kind=int64)
        
        do while (factor <= sqrt_limit)
            do i = factor, sieve_size - 1, 2
                if (getbit(i)) then
                    factor = i
                    exit
                end if
             end do

            i = factor ** 2
            do while (i < sieve_size)
                call setbit_false(i)
           	    i = i + factor * 2
            end do
            factor = factor + 2
        end do
    end subroutine run_sieve

end program primes_fortran
