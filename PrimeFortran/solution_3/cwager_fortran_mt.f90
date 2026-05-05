! Fortran/OpenMP implementation of the base Sieve of Eratosthenes for the
! Prime Sieve Drag Race. It stores only odd candidates, packs one bit per odd
! candidate, and creates a fresh sieve instance for every benchmark pass.
module prime_sieve_module
  use iso_fortran_env, only: int64
  implicit none

  integer(int64), parameter :: word_bits = bit_size(0_int64)

  ! This derived type owns the full sieve state for one pass:
  ! `sieve_size` is the inclusive upper limit, `bit_count` is the number of
  ! odd candidates represented, `word_count` is the number of 64-bit words
  ! allocated, and `bits` is the packed odd-only candidate bitmap.
  type :: prime_sieve
    integer(int64) :: sieve_size = 0_int64
    integer(int64) :: bit_count = 0_int64
    integer(int64) :: word_count = 0_int64
    integer(int64), allocatable :: bits(:)
  contains
    procedure :: initialize => prime_sieve_initialize
    procedure :: destroy => prime_sieve_destroy
    procedure :: run => prime_sieve_run
    procedure :: get_bit => prime_sieve_get_bit
    procedure :: set_bit => prime_sieve_set_bit
    procedure :: clear_bit => prime_sieve_clear_bit
    procedure :: count_primes => prime_sieve_count_primes
  end type prime_sieve

contains

  function build_sieve(sieve_size) result(sieve)
    integer(int64), intent(in) :: sieve_size
    type(prime_sieve) :: sieve

    call sieve%initialize(sieve_size)
    call sieve%run()
  end function build_sieve

  subroutine prime_sieve_initialize(self, sieve_size)
    class(prime_sieve), intent(inout) :: self
    integer(int64), intent(in) :: sieve_size

    ! Start from a clean slate, then allocate a runtime-sized bitset and mark
    ! every odd candidate as potentially prime.
    call self%destroy()

    self%sieve_size = sieve_size
    if (sieve_size >= 3_int64) then
      self%bit_count = (sieve_size - 1_int64) / 2_int64
      self%word_count = (self%bit_count + word_bits - 1_int64) / word_bits
      allocate(self%bits(self%word_count))
      self%bits = not(0_int64)
    else
      self%bit_count = 0_int64
      self%word_count = 0_int64
    end if
  end subroutine prime_sieve_initialize

  subroutine prime_sieve_destroy(self)
    class(prime_sieve), intent(inout) :: self

    if (allocated(self%bits)) then
      deallocate(self%bits)
    end if

    self%sieve_size = 0_int64
    self%bit_count = 0_int64
    self%word_count = 0_int64
  end subroutine prime_sieve_destroy

  subroutine prime_sieve_run(self)
    class(prime_sieve), intent(inout) :: self
    integer(int64) :: factor
    integer(int64) :: composite

    ! This is the straightforward base sieve: scan odd factors from 3 upward,
    ! test each factor from the current sieve state, and if it is still prime,
    ! clear odd composites starting at factor * factor with a 2 * factor step.
    ! There is no wheel, lookup table, precomputed prime table, dense mask, or
    ! strike mask involved here.
    factor = 3_int64
    do while (factor * factor <= self%sieve_size)
      if (self%get_bit(number_to_index(factor))) then
        composite = factor * factor
        do while (composite <= self%sieve_size)
          call self%clear_bit(number_to_index(composite))
          composite = composite + (2_int64 * factor)
        end do
      end if
      factor = factor + 2_int64
    end do
  end subroutine prime_sieve_run

  logical function prime_sieve_get_bit(self, index) result(is_prime)
    class(prime_sieve), intent(in) :: self
    integer(int64), intent(in) :: index
    integer :: bit_position
    integer :: word_index

    word_index = int(index / word_bits) + 1
    bit_position = int(mod(index, word_bits))
    is_prime = btest(self%bits(word_index), bit_position)
  end function prime_sieve_get_bit

  subroutine prime_sieve_set_bit(self, index)
    class(prime_sieve), intent(inout) :: self
    integer(int64), intent(in) :: index
    integer :: bit_position
    integer :: word_index

    word_index = int(index / word_bits) + 1
    bit_position = int(mod(index, word_bits))
    self%bits(word_index) = ibset(self%bits(word_index), bit_position)
  end subroutine prime_sieve_set_bit

  subroutine prime_sieve_clear_bit(self, index)
    class(prime_sieve), intent(inout) :: self
    integer(int64), intent(in) :: index
    integer :: bit_position
    integer :: word_index

    word_index = int(index / word_bits) + 1
    bit_position = int(mod(index, word_bits))
    self%bits(word_index) = ibclr(self%bits(word_index), bit_position)
  end subroutine prime_sieve_clear_bit

  integer(int64) function prime_sieve_count_primes(self) result(prime_count)
    class(prime_sieve), intent(in) :: self
    integer(int64) :: bit_index

    prime_count = 0_int64
    if (self%sieve_size >= 2_int64) then
      prime_count = 1_int64
    end if

    do bit_index = 0_int64, self%bit_count - 1_int64
      if (self%get_bit(bit_index)) then
        prime_count = prime_count + 1_int64
      end if
    end do
  end function prime_sieve_count_primes

  pure integer(int64) function number_to_index(number) result(index_value)
    integer(int64), intent(in) :: number

    ! Odd number n maps to bit index (n - 3) / 2, so index 0 is 3, index 1 is
    ! 5, and so on.
    index_value = (number - 3_int64) / 2_int64
  end function number_to_index

end module prime_sieve_module

program primes
  use iso_fortran_env, only: error_unit, int64, output_unit, real64
  use omp_lib
  use prime_sieve_module, only: build_sieve, prime_sieve
  implicit none

  integer(int64), parameter :: sieve_limit = 1000000_int64
  integer(int64), parameter :: expected_prime_count = 78498_int64
  real(real64), parameter :: benchmark_seconds = 5.0_real64
  character(len=*), parameter :: label = 'cwager_fortran_mt'
  character(len=*), parameter :: tags = 'algorithm=base,faithful=yes,bits=1'

  integer(int64) :: passes
  real(real64) :: elapsed_seconds
  integer :: actual_threads

  ! Check the known reference count before printing benchmark output.
  call validate_or_exit(sieve_limit, expected_prime_count)

  call omp_set_dynamic(.false.)
  call omp_set_num_threads(omp_get_max_threads())
  call run_benchmark(sieve_limit, benchmark_seconds, passes, elapsed_seconds, actual_threads)

  write (output_unit, '(A,";",I0,";",F0.3,";",I0,";",A)') &
    label, passes, elapsed_seconds, actual_threads, tags

contains

  subroutine validate_or_exit(limit, expected_count)
    integer(int64), intent(in) :: limit
    integer(int64), intent(in) :: expected_count
    type(prime_sieve) :: sieve
    integer(int64) :: prime_count

    ! For the drag race limit of 1,000,000, the sieve must find exactly
    ! 78,498 primes or we stop with an error.
    sieve = build_sieve(limit)
    prime_count = sieve%count_primes()

    if (prime_count /= expected_count) then
      write (error_unit, '(A,I0,A,I0)') &
        'Validation failed: expected ', expected_count, ' primes, got ', prime_count
      stop 1
    end if
  end subroutine validate_or_exit

  subroutine run_benchmark(limit, target_seconds, total_passes, total_time, used_threads)
    integer(int64), intent(in) :: limit
    real(real64), intent(in) :: target_seconds
    integer(int64), intent(out) :: total_passes
    real(real64), intent(out) :: total_time
    integer, intent(out) :: used_threads
    real(real64) :: start_time
    real(real64) :: stop_time

    total_passes = 0_int64
    used_threads = 1
    start_time = omp_get_wtime()
    stop_time = start_time + target_seconds

!$omp parallel default(none) shared(limit, stop_time, used_threads) reduction(+:total_passes)
    ! Each OpenMP worker repeatedly builds its own local sieve state. Every
    ! pass allocates and initializes a fresh bitset, pass totals are reduced
    ! across threads, and workers stop as soon as they notice the time limit.
!$omp single
    used_threads = omp_get_num_threads()
!$omp end single
    do while (omp_get_wtime() < stop_time)
      block
        type(prime_sieve) :: sieve

        sieve = build_sieve(limit)
        total_passes = total_passes + 1_int64
      end block
    end do
!$omp end parallel

    total_time = omp_get_wtime() - start_time
  end subroutine run_benchmark

end program primes
