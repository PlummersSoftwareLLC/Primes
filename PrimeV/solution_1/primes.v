import time
import math

const (
	sieve_size = 1_000_000
	q = math.sqrt(sieve_size)	
	all_bits_true_array = []bool{len: sieve_size, init: true}	
)

struct Sieve {
	sieve_size u64
mut:
	bits []bool
}

fn get_dict_value(key string) int {
	// I couldn't declare a global typed inline map. 
	// The documentation is almost non-existent, and I can't find any examples of how to do this.

	mut dictionary := map[string]int{}
	dictionary['10'] = 4
	dictionary['100'] = 25
	dictionary['1000'] = 168
	dictionary['10000'] = 1229
	dictionary['100000'] = 9592
	dictionary['1000000'] = 78498
	dictionary['10000000'] = 664579
	dictionary['100000000'] = 5761455
	dictionary['1000000000'] = 50847534
	dictionary['10000000000'] = 455052511

	return dictionary[key]
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
	valid := (count_primes == get_dict_value(sieve.sieve_size.str()))
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
