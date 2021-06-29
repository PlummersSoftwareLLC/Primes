SET SESSION max_recursive_iterations = 1000000;
SET SESSION max_heap_table_size = (1024 * 1024 * 1024 * 2); -- 1GB

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

  drop table if exists primes_first_segment;
  create table  primes_first_segment engine = MEMORY TRANSACTIONAL=0 AS
  with recursive 
      max_limit(max_nr) as (
          select floor(sqrt(max_limit_input))
  ),
      naturals(n)
  as (
      select 2
      union all
          select n+1 from naturals where n=2
      union all
          select n+2 from naturals where n>2 and n+2<=(select max_nr from max_limit)
  ),

  -- in the recursive call below we are calculating everything that can not be prime
  -- based on the primes < 1000 we found in the first run
  product (num,not_prime)
  as (
      select n, n*n as sqr
        from naturals
        where 
              (n*n) <= (select max_nr from max_limit)
      union all -- all because we know there is no overlap between the two sets, is a bit faster than just union
      select 
        num, -- because recursive does not allow to reuse n
        not_prime + (2 * num) as prod -- because we know that every other number must be even
      from
        product
      where
        (not_prime + (2 * num) ) <= (select max_nr from max_limit)
    ),
    primes(n)
    as (
        select n from naturals
        except
        select product.not_prime from product
    )
  select n from primes
  ;

  -- second run
  drop table if exists primes_table;
  create table  primes_table engine = MEMORY TRANSACTIONAL=0 AS
  with recursive 
      start_at(n) as (
          select floor(sqrt(max_limit_input)) + 1
  ),
      max_limit(max_nr) as (
          select max_limit_input
  ),
      naturals(n)
  as (
      select n from start_at
      union all
          select n+2 from naturals where n+2<=(select max_nr from max_limit)
  ),
  --
  -- in the recursive call below we are calculating everything that can not be prime
  product (num,not_prime)
  as (
      select n, n*n as sqr
        from primes_first_segment
        where
              (n*n) <= (select max_nr from max_limit)
          and n !=2 -- this filters out all the recursive calls for evennumbers!
      union all -- all because we know there is no overlap between the two sets, is a bit faster than just union
      select 
        num, -- because recursive does not allow to reuse n
        not_prime + (2 * num) as prod -- because we know that every other number must be even
      from
        product
      where
        (not_prime + (2 * num)) <= (select max_nr from max_limit)
    ),
    primes(n)
    as (
        select n from naturals
        except
        select product.not_prime from product
    )
  select n from primes_first_segment
  union all
  select n from primes
  ;

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
  select count(*) into count from primes_table;
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
    select * from primes_table order by n asc;
  end if;

  select concat_ws('',
                'Passes: ', passes,
                ', Time: ', duration,
                ', Avg: ', avg,' (sec/pass)',
                ', Limit: ', max_limit,
                ', Count: ', count,
                ', Valid: ', valid,
                '\n\n',
                'fvbakel_MariaDB2;',passes,
                ';',duration,
                ';1;algorithm=other,faithful=no,bits=32'
                ) as '';
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
