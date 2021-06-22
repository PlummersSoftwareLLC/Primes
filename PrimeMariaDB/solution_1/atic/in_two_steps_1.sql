-- for mysql use
--SET SESSION cte_max_recursion_depth = 0;
-- for MariaDB
SET SESSION max_recursive_iterations = 100000000;
--SET GLOBAL innodb_buffer_pool_size =  (1024 * 1024 * 1024 * 2); -- 2GB
SET max_heap_table_size = (1024 * 1024 * 1024 * 1); -- 1GB

drop database if exists primedb;
create database primedb;

use primedb;

drop table if exists timing;
create table timing  (
    what        varchar(50),
    time_stamp  BIGINT DEFAULT (UNIX_TIMESTAMP(NOW()) * 1000 + floor(MICROSECOND(NOW(6))/1000))
) engine = MEMORY;

-- configure the limit here
drop table if exists max_limit_conf;
create table  max_limit_conf engine = MEMORY as 
        select 1000000 as max_nr,
               1000 as sqr -- workaround because sqlite does not have sqrt function
;

DELIMITER //
drop procedure if exists run_sieve//
create procedure run_sieve()
begin

  drop table if exists primes_first_segment;
  create table  primes_first_segment engine = MEMORY AS
  with recursive 
  -- configure the limit here
      max_limit(max_nr) as (
          select sqr from max_limit_conf LIMIT 1 
  ),
      naturals(n)
  -- init of the narural numbers 2,3,5,7,...
  as (
      select 2
      union all
          select n+1 from naturals where n=2
      union all
          select n+2 from naturals where n>2 and n+2<=(select max_nr from max_limit)
  ),
  --
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
  create table  primes_table engine = MEMORY AS
  with recursive 
  -- configure the limit here
      start_at(n) as (
          select ((select sqr from max_limit_conf LIMIT 1) + 1)
  ),
      max_limit(max_nr) as (
          select max_nr from max_limit_conf  LIMIT 1
  ),
      naturals(n)
  -- init of the narural numbers 2,3,5,7,...
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

DELIMITER //
drop procedure if exists print_results//
create procedure print_results(
  IN show_results   BOOLEAN,
  IN duration       INT,
  IN passes         INT
)
begin
  declare avg       REAL;
  declare count     INT;
  declare valid     VARCHAR(10);
  declare max_limit INT;

  set avg = duration / passes;
  select count(*) into count from primes_table;
  set valid = 'True';
  select max_nr into max_limit from max_limit_conf limit 1;

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
                'fvbakel_MariaDB;',passes,
                ';',duration,
                ';1;algorithm=other,faithful=no,bits=32'
                ) as '';
end//

DELIMITER //
drop procedure if exists main//
create procedure main()
begin
  declare start_time    BIGINT;
  declare duration_ms   INT default 0;
  declare duration      REAL;
  declare max_time      INT default 5;
  declare max_time_ms   INT;
  declare passes        INT default 0;

  set max_time_ms = max_time * 1000;
  set start_time = (UNIX_TIMESTAMP(NOW()) * 1000 + floor(MICROSECOND(NOW(6))/1000));
  while duration_ms < max_time_ms DO
    set passes = passes + 1;
    call run_sieve();
    set duration_ms =   (UNIX_TIMESTAMP(NOW()) * 1000 + floor(MICROSECOND(NOW(6))/1000))
                        - start_time ;
  end while;
  set duration = duration_ms / 1000;
  call print_results(False,duration,passes);
end//

DELIMITER ;

call main();

