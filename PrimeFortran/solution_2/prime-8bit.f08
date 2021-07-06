module PrimeModule
    use iso_fortran_env
    implicit none

    integer(kind=int64), dimension(9), parameter :: validated_sieve_sizes = (/ &
        10, &
        100, &
        1000, &
        10000, &
        100000, &
        1000000, &
        10000000, &
        100000000, &
        1000000000 &
    /)
    integer(kind=int64), dimension(9), parameter :: valid_prime_counts = (/ &
        4, &
        25, &
        168, &
        1229, &
        9592, &
        78498, &
        664579, &
        5761455, &
        50847534 &
    /)

    type PrimeSieve
        private
        integer(kind=int8), dimension(:), allocatable :: raw_bits
        integer(kind=int64) :: sieve_size
    contains
        procedure, public :: initialize => primesieve_initialize
        procedure, public :: run_sieve => primesieve_run_sieve
        procedure, public :: validate_results => primesieve_validate_results
        procedure, public :: print_results => primesieve_print_results
        procedure, public :: count_primes => primesieve_count_primes
        procedure, private :: get_bit => primesieve_get_bit
        procedure, private :: clear_bits => primesieve_clear_bits
        final :: primesieve_destructor
    end type
    
contains

    subroutine primesieve_initialize(this, sieve_size)
        class(PrimeSieve), intent(inout) :: this
        integer(kind=int64), intent(in) :: sieve_size

        this%sieve_size = sieve_size

        allocate(this%raw_bits(sieve_size / 2))
        this%raw_bits = 1_int8
    end subroutine

    subroutine primesieve_destructor(this)
        type(PrimeSieve), intent(inout) :: this

        deallocate(this%raw_bits)
    end subroutine

    subroutine primesieve_run_sieve(this)
        class(PrimeSieve), intent(inout) :: this
        integer(kind=int64) :: factor, q, num

        factor = 3
        q = int(sqrt(real(this%sieve_size)))

        do while (factor <= q)
            do num = factor, this%sieve_size - 1, 2
                if (this%get_bit(num)) then
                    factor = num
                    exit
                end if
            end do

            call this%clear_bits(factor**2, this%sieve_size - 1, factor * 2)

            factor = factor + 2
        end do
    end subroutine

    function primesieve_get_bit(this, num) result(bit)
        class(PrimeSieve), intent(in) :: this
        integer(kind=int64), intent(in) :: num
        logical :: bit

        if (iand(num, 1_int64) == 1_int64) then
            ! odd number
            bit = 0 /= this%raw_bits(num/2)
        else
            ! even number
            bit = .false.
        end if
    end function

    subroutine primesieve_clear_bits(this, first, last, step)
        class(PrimeSieve), intent(inout) :: this
        integer(kind=int64), intent(in) :: first, last, step
        integer(kind=int64) :: bitidx

        ! There is one bit per two natural numbers as we're not keeping track
        ! of the evens.

        do bitidx = first/2, last/2, step/2
            this%raw_bits(bitidx) = 0
        end do
    end subroutine

    function primesieve_validate_results(this) result(is_valid)
        class(PrimeSieve), intent(in) :: this
        logical :: is_valid
        integer(kind=int64) :: count, i

        is_valid = .false.
        count = this%count_primes()

        do i = 1, size(valid_prime_counts)
            if (validated_sieve_sizes(i) == this%sieve_size) then
                is_valid = (count == valid_prime_counts(i))
            end if
        end do
    end function

    subroutine primesieve_print_results(this)
        class(PrimeSieve), intent(in) :: this
        integer(kind=int64) :: i

        if (this%sieve_size < 2) then
            return
        end if

        print *, 2
        
        do i = 3, this%sieve_size -1
            if (this%get_bit(i)) then
                print *, i
            end if
        end do
    end subroutine

    function primesieve_count_primes(this) result(count)
        class(PrimeSieve), intent(in) :: this
        integer(kind=int64) :: count, i

        if (this%sieve_size < 2) then
            count = 0
            return
        end if

        count = 1

        do i = 3, this%sieve_size -1
            if (this%get_bit(i)) then
                count = count + 1
            end if
        end do
    end function

end module

program PrimeFortran
    use iso_fortran_env
    use PrimeModule
    implicit none

    integer(kind=int64), parameter :: sieve_size = 1000000
    integer(kind=int64), parameter :: benchmark_secs = 5
    ! type(PrimeSieve) :: main_sieve

    integer(kind=int64) :: start_clock, end_clock, cur_clock, &
                       clock_count_rate, clock_count_max
    ! logical :: valid
    integer(kind=int64) :: iters = 0
    real :: time_elapsed

    ! call main_sieve%initialize(sieve_size)
    ! call main_sieve%run_sieve()
    ! valid = main_sieve%validate_results()
    ! print *, "primes:", main_sieve%count_primes()
    ! print *, "valid:", valid

    ! if (.not. valid) then
    !     call exit(1)
    ! end if 
    
    call system_clock(start_clock, clock_count_rate, clock_count_max)
    end_clock = start_clock + benchmark_secs * clock_count_rate
    cur_clock = start_clock

    do while (cur_clock < end_clock)
        call run_prime_sieve()
        iters = iters + 1
        call system_clock(cur_clock)
    end do

    time_elapsed = real(cur_clock - start_clock) / real(clock_count_rate)

    ! print *, "iterations:", iters
    ! print *, "time:", time_elapsed
    ! print *, ""

    write (*, "(A,I0,A,F0.8,A)") "tjol-8bit;", iters, ";", time_elapsed, ";1;algorithm=base,faithful=yes,bits=8"

contains

    subroutine run_prime_sieve()
        type(PrimeSieve) :: sieve

        call sieve%initialize(sieve_size)
        call sieve%run_sieve()
    end subroutine

end program