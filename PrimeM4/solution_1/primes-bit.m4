divert(-1)
dnl --- Command-line parameters ---
dnl LIMIT = Upper limit for calculating prime numbers. Default: 1000000
dnl TIME = Time limit in seconds. Default: 5
dnl SHOW_RESULTS = Print found prime numbers. Default: 0 (false)
ifdef(`LIMIT',, `define(`LIMIT', 1000000)')
ifdef(`TIME',, `define(`TIME', 5)')
ifdef(`SHOW_RESULTS',, `define(`SHOW_RESULTS', 0)')

dnl Time limit in csec (centiseconds)
define(`TIME_CSEC', eval(TIME * 100))
dnl ---

dnl --- Time macros ---
dnl M4 does not have any support for timers. Therefore, a system call is made to output
dnl "/proc/uptime" to get the current time in milliseconds. The format out this is
dnl
dnl   uptime in seconds as a decimal number (in csec), space, another number, newline
dnl
dnl M4 does not support floating point, so this needs to be converted to an integer,
dnl and that integer will be truncated to 32 bits, regardless of how large the value
dnl actually is.
dnl
dnl macro time():
dnl   return current time in csec (truncated to 32 bits)
define(`time', `patsubst(esyscmd(`cat /proc/uptime'), `\.\([0-9][0-9]\).*\s*', `\1')')
dnl ---

dnl --- Bit macros for sieve ---
dnl Sieve is the macro "s#", where "#" is a word number -- e.g., s0, s1, ..., s<N-1>.
dnl
dnl Each word contains 32 bits, where 0 means prime, and 1 means composite: 
dnl
dnl - s0:
dnl   - 3: bit 0
dnl   - 5: bit 1
dnl   - ...
dnl   - 65: bit 31
dnl - s1:
dnl   - 67: bit 0
dnl   - 69: bit 1
dnl   - ...
dnl   - 129: bit 31
dnl - ...
dnl - s<N-1>:
dnl   - 64*(N - 1) + 3: bit 0
dnl   - 64*(N - 1) + 5: bit 1
dnl   - ...
dnl   - 64*(N - 1) + 65: bit 31
dnl
dnl where N is the number of words needed represent odd factors from 3 to LIMIT, inclusive

dnl macro get_word(factor):
dnl   return (factor - 3) // 64
define(`word', `eval(($1 - 3) >> 6)')

dnl macro mask(factor):
dnl   return 1 << (((factor - 3) / 2) % 32)
define(`mask', `eval(1 << ((($1 - 3) >> 1) & 31))')

dnl macro get_bit(factor):
dnl   return s<word(factor)> & mask(factor)
dnl
dnl Zero is returned if bit is clear, non-zero otherwise.
define(`get_bit', `eval(defn(s`'word($1)) & mask($1))')

dnl macro set_bit(factor):
dnl   s<word(factor)> = s<word(factor)> | mask(factor)
define(`set_bit', `define(s`'word($1), eval(defn(s`'word($1)) | mask($1)))')

dnl Maximum word number
define(`MAX_WORD_NUM', word(LIMIT))
dnl ---

dnl --- Sieve macros ---
dnl macro timed_prime_sieve():
dnl   passes = 0
dnl   start_time = time()
dnl   elapsed_time = 0
dnl   do:
dnl     prime_sieve()
dnl     passes = passes + 1
dnl     elapsed_time = time() - start_time
dnl   while elapsed_time < TIME_CSEC
define(`timed_prime_sieve', `define(`passes', 0)define(`elapsed_time', 0)_tps(time())')
define(`_tps',
`prime_sieve()define(`passes', incr(passes))define(`elapsed_time', eval(time() - $1))dnl
ifelse(eval(elapsed_time < TIME_CSEC), 1, `_tps($1)')'dnl
)

dnl macro prime_sieve():
dnl   clear_sieve()
dnl   n = 3
dnl   while n*n <= LIMIT:
dnl     if get_bit(n) == 0:
dnl       for k = n*n to LIMIT step 2*n:
dnl         set_bit(k)
dnl     n = n + 2
define(`prime_sieve', `clear_sieve()_pso(3)')
define(`_pso', `ifelse(eval($1 * $1 <= LIMIT), 1, `ifelse(get_bit($1), 0, `_psi(eval($1 * $1), eval(2 * $1))')_pso(eval($1 + 2))')')
define(`_psi', `ifelse(eval($1 <= LIMIT), 1, `set_bit($1)_psi(eval($1 + $2), $2)')')

dnl macro clear_sieve():
dnl   for n = 0 to MAX_WORD_NUM:
dnl     s<n> = 0
define(`clear_sieve', `_cs(0)')
define(`_cs', `ifelse(eval($1 <= MAX_WORD_NUM), 1, `define(`s$1', 0)_cs(incr($1))')')

dnl macro show_sieve():
dnl   output "2"
dnl   for n = 3 to LIMIT step 2
dnl     if get_bit(n) == 0:
dnl       output " <n>"
dnl   output newline
define(`show_sieve', `2`'_ss(3)')
define(`_ss', `ifelse(eval($1 <= LIMIT), 1, `ifelse(get_bit($1), 0, ` $1')_ss(eval($1 + 2))', `
')')

dnl macro count_sieve():
dnl   count = 1
dnl   for n = 3 to LIMIT step 2
dnl     if get_bit(n) == 0:
dnl       count = count + 1
dnl   return count
define(`count_sieve', `_ns(3, 1)')
define(`_ns', `ifelse(eval($1 <= LIMIT), 1, `_ns(eval($1 + 2), ifelse(get_bit($1), 0, incr($2), $2))', $2)')

dnl macro valid_sieve_count(count):
dnl   return 1 if count is valid, 0 otherwise
define(`valid_sieve_count',
`ifelse(
  LIMIT, 10, `eval($1 == 4)',
  LIMIT, 100, `eval($1 == 25)',
  LIMIT, 1000, `eval($1 == 168)',
  LIMIT, 10000, `eval($1 == 1229)',
  LIMIT, 100000, `eval($1 == 9592)',
  LIMIT, 1000000, `eval($1 == 78498)',
  LIMIT, 10000000, `eval($1 == 664579)', `0'dnl
)'dnl
)
dnl ---

dnl Elapsed time is in csec, so it needs to be converted to a decimal number of
dnl sec for display like this:
dnl
dnl   <whole part>.<fractional part to 2 decimal places>
dnl
dnl The average is calculated like this:
dnl
dnl   average (sec) = 100 * passes / elapsed time (csec)
dnl
dnl That also needs to be converted for display like this:
dnl
dnl   <whole part>.<fractional part to 3 decimal places>
dnl
dnl macro show_results():
dnl   output "<passes>, <elapsed time in sec>, <average in sec>, <count>, <validity>\n"
dnl   output "<label>;<passes>;<elapsed time in sec>;1;algorithm=base,faithful=no\n"
define(`show_results',
`define(`avg', eval(100000 * passes / elapsed_time))define(`count', count_sieve())dnl
format(
  `Passes: %d, Time: %d.%02d, Avg: %d.%03d, Count: %d, Valid: %s',
  passes,
  eval(elapsed_time / 100), eval(elapsed_time % 100),
  eval(avg / 1000), eval(avg % 1000),
  count,
  ifelse(valid_sieve_count(count), 1, `true', `false'))
format(
  `rzuckerm-m4-bit;%d;%d.%02d;1;algorithm=base,faithful=no,bits=1',
  passes,
  eval(elapsed_time / 100), eval(elapsed_time % 100))
'dnl
)

divert(0)dnl
timed_prime_sieve()dnl
ifelse(SHOW_RESULTS, 0, , `show_sieve()')dnl
show_results()
