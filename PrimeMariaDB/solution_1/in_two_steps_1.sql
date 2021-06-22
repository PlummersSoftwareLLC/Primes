-- for mysql use
--SET SESSION cte_max_recursion_depth = 1000000
-- for MariaDB
SET SESSION max_recursive_iterations = 1000000;

drop database if exists primedb;
create database primedb;

use primedb;


-- configure the limit here
drop table if exists max_limit_conf;
create TEMPORARY table  max_limit_conf engine = MEMORY as 
        select 1000000 as max_nr,
               1000 as sqr -- workaround because sqlite does not have sqrt function
;

drop table if exists primes_first_segment;
create TEMPORARY table  primes_first_segment engine = MEMORY AS
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
create TEMPORARY table  primes_table engine = MEMORY AS
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

-- outputs
select count(*) from primes_table;
--select * from primes_table;
