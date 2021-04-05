%%  MATLAB Prime Sieve
%%
%%  Fran Spigel, 5. 4. 2021.
%%
%%  This is the main function. Change the 'limit' variable in order to count
%%  prime numbers up to that number.

% A small library of pre-computed prime counts for powers of 10
% Used for validation down the line
prime_counts = [
  [10, 1],
  [100, 25],
  [1000, 168],
  [10000, 1229],
  [100000, 9592],
  [1000000, 78498],
  [10000000, 664579],
  [100000000, 5761455]
];  

% Set the number up to which primes should be counted. For example, 
% if limit is set to 10, the program will count 4 primes (1, 3, 5, 7).
limit = 1000;

% Count how many times the program has completed
passes = 0;

start_time = time();
while time()-start_time < 10
  count = run_sieve(limit);
  passes++;
endwhile
final_time = time()-start_time;

% Average runtime per pass
avg = final_time/count;

% Validate if a known limit was used
f = find(prime_counts(:,1)==limit)
if size(f,2)==1
  if prime_counts(f,2)==count
    valid = 1
  else
    valid = 0
  endif
else
  valid = 1
endif

% Print results
sprintf('Passes: %d, Time: %0.5f, Avg: %0.5f, Limit: %d, Count: %d, Valid: %d', passes, final_time, avg, limit, count, valid)