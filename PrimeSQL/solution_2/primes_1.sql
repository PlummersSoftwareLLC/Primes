SET SESSION max_recursive_iterations = 1000000;
SET SESSION max_heap_table_size = (1024 * 1024 * 1024 * 1); -- 1GB

drop database if exists primedb;
create database primedb;

use primedb;

-- static table to validate the result
drop table if exists known_prime_counts;
create table  known_prime_counts (
  max_limit     INT,
  nr_of_primes  INT
 ) engine = MEMORY;
 insert into known_prime_counts values 
    (10,4),
    (100,25),
    (1000,168),
    (10000,1229),
    (100000,9592),
    (1000000,78498),
    (10000000,664579),
    (100000000,5761455)
 ;

DELIMITER //
drop procedure if exists run_sieve//
create procedure run_sieve(
  IN max_limit_input   INT
)
begin
  declare maxroot       INT;
  declare prime         INT;
  declare start         INT;
  declare next_prime    INT;
  
  drop table if exists primes_table;
  create table primes_table like primes_table_template;
  insert into primes_table select * from primes_table_template;

  create index primes_i on primes_table(n);

  set maxroot = floor(sqrt(max_limit_input));

  set prime = 3;
  while (prime <= maxroot) do
    set start = prime * prime;
    -- cross out all non primes based on this prime  
    update primes_table 
    inner join (
      with recursive
        not_prime(n) as (
          select start
          union all
          select (n + prime) from not_prime where (n + prime) <= max_limit_input
        )
      select n from not_prime
    ) not_primes on primes_table.n = not_primes.n
    set primes_table.isPrime = 0
    ;

    -- find next prime
    select IFNULL(min(n),max_limit_input) 
    into next_prime
    from primes_table
    where
          n > prime
      and isPrime = 1
    ;

    set prime = next_prime;
   end while;
end//

drop procedure if exists print_results//
create procedure print_results(
  IN max_limit      INT,
  IN show_results   BOOLEAN,
  IN duration       REAL,
  IN passes         INT
)
begin
  declare avg       REAL;
  declare count     INT;
  declare valid     VARCHAR(10);

  set avg = duration / passes;
  select count(*) into count from primes_table where isPrime = 1;
  select case
            when result_found = 1 then 'True'
            else 'False' 
         end
    into valid 
  from
    (
      select count(*) as result_found
      from known_prime_counts
      where 
            known_prime_counts.max_limit = max_limit
        and nr_of_primes = count 
    ) as dummy
  ;

  if show_results then
    select * from primes_table where isPrime = 1 order by n asc;
  end if;

  select concat_ws('',
                'Passes: ', passes,
                ', Time: ', duration,
                ', Avg: ', avg,' (sec/pass)',
                ', Limit: ', max_limit,
                ', Count: ', count,
                ', Valid: ', valid,
                '\n\n',
                'fvbakel_MariaDB1;',passes,
                ';',duration,
                ';1;algorithm=base,faithful=no,bits=32'
                ) as '';
end//

drop procedure if exists init//
create procedure init(
  IN  max_limit   INT
)
begin
  -- static table used for fast init
  -- this table represents a static list of the natural numbers 
  -- with isPrime initialized to 1
  -- it can take up to 1 seccond to create this table.
  -- result is a table with 2,3,5,7,...,max_limit
  -- This table is copied each time in the calculation 
  -- to the actual table
  drop table if exists primes_table_template;
  create table primes_table_template engine = MEMORY TRANSACTIONAL=0 as
  with recursive
      naturals (n,isPrime) as (
          select 2,1
      union all
          select n+1,1 from naturals where n=2
      union all
          select n+2,1 from naturals where n>2 and n+2<=max_limit
      )
      select n,isPrime from naturals
  ;
end//

drop procedure if exists main//
create procedure main()
begin
  DECLARE  max_limit     INT default 1000000;
  DECLARE  max_time      INT default 5;
  DECLARE  show_results  BOOLEAN default False;

  declare start_time    BIGINT;
  declare duration_ms   INT default 0;
  declare duration      REAL;
  declare max_time_ms   INT;
  declare passes        INT default 0;

  call init(max_limit);

  set max_time_ms = max_time * 1000;
  set start_time = (UNIX_TIMESTAMP(NOW()) * 1000 + floor(MICROSECOND(NOW(6))/1000));
  while duration_ms < max_time_ms DO
    set passes = passes + 1;
    drop table if exists primes_table;
    call run_sieve(max_limit);
    set duration_ms =   (UNIX_TIMESTAMP(NOW()) * 1000 + floor(MICROSECOND(NOW(6))/1000))
                        - start_time ;
  end while;
  set duration = duration_ms / 1000;
  call print_results(max_limit,show_results,duration,passes);
end//

DELIMITER ;

call main();
