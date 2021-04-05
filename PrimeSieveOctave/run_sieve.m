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
  raw_bits = ones(1,idivide(limit,2));
  
  % Check if 'index' has been eliminated as non-prime yet. Returns 1 if
  % it has not yet been eliminated (still considered a prime candidate)
  function bit = get_bit(index)
    if rem(index,2) == 0
      % If multiple of 2, automatically return false and warn the user 
      % that he is checking even numbers unnecessarily
      'checking even bits!'
      bit = 0;
      return
    else
      bit = raw_bits((index+1)/2);
      return
    endif
  endfunction

  function clear_bit(index)
    % Mark a number as non-prime
    if(rem(index,2)==1)
      raw_bits((index+1)/2) = 0;
    endif
  endfunction

  factor = 3;
  q = sqrt(limit);
  while(factor<=q)
    % Find the next prime (look for the first bit in raw_bits that is still set
    for num=factor:2:limit
      if get_bit(num)
        factor = num;
        break
      endif
    endfor

    % Mark all odd multiples of 'factor' as non-prime
    % Even multiples are already known to be non-prime
    for num=factor*3:factor*2:limit
      clear_bit(num);
    endfor
    
    % Increment 'factor' to the next prime candidate
    factor += 2;
  endwhile

  % Count the number of primes by counting how many bits have successfully 
  % passed through the sieve without being cleared  
  count = sum(raw_bits);
 endfunction
