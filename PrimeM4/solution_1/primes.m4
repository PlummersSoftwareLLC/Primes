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

dnl --- Sieve macros ---
dnl Sieve is the macro "s#", where "#" is a candidate prime number -- e.g., s2, s3, ...

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
dnl     if s<n>:
dnl       for k = n*n to LIMIT step n:
dnl         delete s<k>
dnl     n = n + 2
define(`prime_sieve', `clear_sieve()_pso(3)')
define(`_pso', `ifelse(eval($1 * $1 <= LIMIT), 1, `ifdef(`s$1', `_psi(eval($1 * $1), $1)')_pso(eval($1 + 2))')')
define(`_psi', `ifelse(eval($1 <= LIMIT), 1, `undefine(`s$1')_psi(eval($1 + $2), $2)')')

dnl macro clear_sieve():
dnl   s2 = 1
dnl   for n = 3 to LIMIT step 2:
dnl     s<n> = 1
define(`clear_sieve', `define(`s2', 1)_cs(3)')
define(`_cs', `ifelse(eval($1 <= LIMIT), 1, `define(`s$1', 1)_cs(eval($1 + 2))')')

dnl macro show_sieve():
dnl   output "2"
dnl   for n = 3 to LIMIT step 2
dnl     if s<n>:
dnl       output " <n>"
dnl   output newline
define(`show_sieve', `2`'_ss(3)')
define(`_ss', `ifelse(eval($1 <= LIMIT), 1, `ifdef(`s$1', ` $1')_ss(eval($1 + 2))', `
')')

dnl macro count_sieve():
dnl   count = 1
dnl   for n = 3 to LIMIT step 2
dnl     if s<n>:
dnl       count = count + 1
dnl   return count
define(`count_sieve', `_ns(3, 1)')
define(`_ns', `ifelse(eval($1 <= LIMIT), 1, `_ns(eval($1 + 2), ifdef(`s$1', incr($2), $2))', $2)')

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
dnl   output "<label>;<passes>;<elapsed time in sec>;algorithm=base;faithful=no\n"
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
  `rzuckerm-m4;%d;%d.%02d;algorithm=base;faithful=no',
  passes,
  eval(elapsed_time / 100), eval(elapsed_time % 100))
'dnl
)

divert(0)dnl
timed_prime_sieve()dnl
ifelse(SHOW_RESULTS, 0, , `show_sieve()')dnl
show_results()
