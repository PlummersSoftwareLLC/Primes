%%  MATLAB Prime Sieve
%%
%%  Fran Špigel, 5. 4. 2021.
%%
%%  This is the run_sieve function. It simply counts the number of primes
%%  from 1 to 'limit' (inclusice) and returns it.

function count=run_sieve(limit)
  % Initialize the sieve to an array of ones
  % Only store information about odd numbers, as all even numbers are
  % automatically prime
  % rbergen: wrapped 2 in int32() to fix idivide integer input error in Octave 6.2.0
  raw_bits = true(1,idivide(limit,int32(2)));
  
  factor = 3;
  q = sqrt(limit);
  while(factor<=q)
    % Find the next prime (look for the first bit in raw_bits that is still set
    for num=factor:2:limit
      if raw_bits((num+1)/2)
        factor = num;
        break
      endif
    endfor

    % Mark all odd multiples of 'factor' as non-prime
    % Even multiples are already known to be non-prime
	raw_bits((factor*3+1)/2:factor:end) = false;
    
    % Increment 'factor' to the next prime candidate
    factor += 2;
  endwhile

  % Count the number of primes by counting how many bits have successfully 
  % passed through the sieve without being cleared  
  count = sum(raw_bits);
 endfunction
