import os
import time
import math

const (
	dictionary = map {
		u64(10):     4
		100:         25
		1000:        168
		10000:       1229
		100000:      9592
		1000000:     78498
		10000000:    664579
		100000000:   5761455
		1000000000:  50847534
		10000000000: 455052511
	}
)

struct Sieve {
	size u64
pub mut:
	buf &u64
}

[direct_array_access]
fn (sieve Sieve) get_num(i u64) bool {
	return unsafe { (sieve.buf[i >> 6] & (u64(1) << (i & 0x3F))) } == 0
}

[direct_array_access]
fn (mut sieve Sieve) run_sieve() {
	mut factor := u64(3)
	size := sieve.size
	q := math.sqrt(size)
	for factor <= q {
		for num := factor; num < size; num += u64(2) {
			if unsafe { (sieve.buf[num >> 6] & (u64(1) << (num & 0x3F))) } == 0 {
				factor = num
				break
			}
		}

		for num := factor * factor; num < size; num += (factor << 1) {
			unsafe {
				sieve.buf[num >> 6] |= (u64(1) << (num & 0x3F))
			}
		}
		factor += u64(2)
	}
}

fn (sieve Sieve) print_results(show_results bool, duration time.Duration, passes int) {
	if show_results {
		print('2, ')
	}

	mut count := u64(1)
	for num := u64(3); num <= sieve.size; num += u64(2) {
		if sieve.get_num(num) {
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
	valid := (count_primes == dictionary[sieve.size])
	eprintln('Passes: $passes, Time: $duration, Avg: $avg, Limit: $sieve.size, Count1: $count, Count2: $count_primes, Valid: $valid')

	println('Penguindark;$passes;$duration;1;algorithm=base,faithful=yes')
}

fn (sieve Sieve) count_primes() u64 {
	mut count := u64(1)
	for i := u64(3); i < sieve.size; i += u64(2) {
		if sieve.get_num(i) {
			count++
		}
	}
	return count
}

fn main() {
	sw := time.new_stopwatch(time.StopWatchOptions{})

	size := os.args[1].u64()
	mut passes := 0

	mut sieve := Sieve{
		size: size
		buf: unsafe { &u64(vcalloc(8 * int(size / 64 + 1))) }
	}
	for {
		sieve.run_sieve()

		passes++
		elapsed := sw.elapsed().seconds()
		if elapsed >= 5 {
			sieve.print_results(false, elapsed, passes)
			break
		}
		unsafe {
			free(sieve.buf)
			sieve.buf = &u64(vcalloc(8 * int(size / 64 + 1)))
		}
	}
}
