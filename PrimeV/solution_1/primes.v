import time
import math

const (
	sieve_size          = 1_000_000
	q                   = math.sqrt(sieve_size)
	all_bits_true_array = []bool{len: sieve_size, init: true}
	dictionary          = {
		'10':          4
		'100':         25
		'1000':        168
		'10000':       1229
		'100000':      9592
		'1000000':     78498
		'10000000':    664579
		'100000000':   5761455
		'1000000000':  50847534
		'10000000000': 455052511
	}
)

struct Sieve {
	sieve_size u64
mut:
	bits []bool
}

[direct_array_access]
fn (mut sieve Sieve) run_sieve() {
	mut factor := u64(3)

	for factor <= q {
		for num := factor; num < sieve.sieve_size; num += u64(2) {
			if sieve.bits[num] {
				factor = num
				break
			}
		}

		for num := factor * factor; num < sieve.sieve_size; num += factor * u64(2) {
			sieve.bits[num] = false
		}

		factor += u64(2)
	}
}

fn (sieve Sieve) print_results(show_results bool, duration time.Duration, passes int) {
	if show_results {
		print('2, ')
	}

	mut count := u64(1)
	for num := u64(3); num <= sieve.sieve_size; num += u64(2) {
		if sieve.bits[num] {
			if show_results {
				print('$num, ')
			}

			count++
		}
	}

	if show_results {
		println('')
	}

	avg := f64(duration / passes)
	count_primes := sieve.count_primes()
	valid := (count_primes == u64(dictionary[sieve.sieve_size.str()]))
	eprintln('Passes: $passes, Time: $duration, Avg: $avg, Limit: $sieve.sieve_size, Count1: $count, Count2: $count_primes, Valid: $valid')

	println('marghidanu;$passes;$duration;1;algorithm=base,faithful=yes')
}

fn (sieve Sieve) count_primes() u64 {
	mut count := u64(1)

	for i := u64(3); i < sieve.sieve_size; i += u64(2) {
		if sieve.bits[i] {
			count++
		}
	}

	return count
}

fn main() {
	mut passes := 0
	start_time := time.now()

	for {
		mut sieve := Sieve{
			sieve_size: 1_000_000
			bits: all_bits_true_array
		}
		sieve.run_sieve()

		passes++
		duration := time.now() - start_time
		if duration.seconds() >= 5 {
			sieve.print_results(false, duration.seconds(), passes)
			break
		}
	}
}
