-- based on https://stackoverflow.com/questions/7370761/sqlite-loop-statements
-- and https://blog.devart.com/increasing-sqlite-performance.html

-- pragma settings have little to no impact on Linux
-- might have more impact on windows
PRAGMA TEMP_STORE = 2;
PRAGMA JOURNAL_MODE = OFF;
PRAGMA SYNCHRONOUS = 0;
PRAGMA LOCKING_MODE = EXCLUSIVE;

drop table if exists timing;
create table timing (
    what,
    time_stamp_jday,
    duration
);

-- first entry
insert into timing values ("start",julianday("now"),0);

-- starting a transaction makes no difference in performance
begin transaction;


drop table if exists naturals;
create table naturals (
  -- not using a unique key improvess the peformance by 30% because the inserts are faster
  -- the index is created after the insers
  -- n integer unique primary key asc,
    n integer,
    isprime bool
--    debug_info
);

drop table if exists not_primes;
create table not_primes (
    n integer
);

with
  ts(time_stamp,previous_ts) as (
    select julianday("now"), (select max(time_stamp_jday) from timing)
  )
insert into timing 
select  "Init",
        ts.time_stamp,
        (ts.time_stamp - ts.previous_ts ) *86400.0
from ts
;
-- only include 2 and the odd numbers in the table
-- not much impact on the performance compared to
-- adding the even numbers too
insert into naturals values(2, 1);
with recursive
  nn (n)
as (
  select 3
  union all
  select n+2 as newn from nn
  where newn <= 1000000
)
insert into naturals
--select n, 1, null from nn; -- debug
select n, 1 from nn;

with
  ts(time_stamp,previous_ts) as (
    select julianday("now"), (select max(time_stamp_jday) from timing)
  )
insert into timing 
select  "fill naturals",
        ts.time_stamp,
        (ts.time_stamp - ts.previous_ts ) *86400.0
from ts
;
-- adding the index makes the program twice as fast!
create unique index naturals_i on naturals(n);

with
  ts(time_stamp,previous_ts) as (
    select julianday("now"), (select max(time_stamp_jday) from timing)
  )
insert into timing 
select  "Index 1",
        ts.time_stamp,
        (ts.time_stamp - ts.previous_ts ) *86400.0
from ts
;
/*
  The recursive statement delivers a virtual table
  with two columns n, not prime.
  This is done for all number that are smaller than the squareroot of the size.

  The logic works in multiple stages due to the recursive nature. This is what happens:
  in the first run the virtual table is filled with
  n,not_prime
  2,4
  3,9
  5,25
  7,49
  9,81 
  etc

  note that 9 is not prime, but is still added

  Then in the second run of the recursive statement the second part 
  of the union becomes effective.
  Each new row of the previous statement is processed. The following will be added
  n,not_prime
  2,8
  3,15
  5,35
  7,63
  9,99
  etc

  This continues recursive for all entries in the virual table, 
  until the max is reached for the non_prime value
*/
--insert or replace into naturals
  with recursive
    product (num,not_prime)
  as (
    -- n*n is the start.
    -- so if prime number is 3 we start at 9 with crossing off
    -- for prime 5 we start at 25
    -- what about 15 then in this case? 15 is already crossed of by
    -- the processing of prime 3
    -- downside of the approach below is that we are also crossing off for multiplications 
    -- of not primes
    select n, n*n as sqr
      from naturals
      where 
            sqr <= (select max(n) from naturals)
        and n !=2 -- this filters out all the recursive calls for evennumbers!
    union all -- all because we know there is no overlap between the two sets, is a bit faster than just union
    select 
      num, -- because recursive does not allow to reuse n
      not_prime+2*num as prod --2*num because we know that every other number must be even
    from
      product
    where
      prod <= (select max(n) from naturals)
  )
insert into not_primes
select distinct not_prime from product;

with
  ts(time_stamp,previous_ts) as (
    select julianday("now"), (select max(time_stamp_jday) from timing)
  )
insert into timing 
select  "Not primes calculation",
        ts.time_stamp,
        (ts.time_stamp - ts.previous_ts ) *86400.0
from ts
;
create unique index not_primes_i on not_primes(n);

with
  ts(time_stamp,previous_ts) as (
    select julianday("now"), (select max(time_stamp_jday) from timing)
  )
insert into timing 
select  "Index 2",
        ts.time_stamp,
        (ts.time_stamp - ts.previous_ts ) *86400.0
from ts
;
insert or replace into naturals
select naturals.n, 0 
from not_primes
join naturals
  on (not_primes.n = naturals.n)
;

--select n, 0,(select count(*) from product where not_prime=18)
--select n, 0
--from product join naturals
--  on (product.not_prime = naturals.n)
--;
commit;

-- end
with
  ts(time_stamp,previous_ts) as (
    select julianday("now"), (select min(time_stamp_jday) from timing)
  )
insert into timing 
select  "End",
        ts.time_stamp,
        (ts.time_stamp - ts.previous_ts ) *100000
from ts
;


-- output
select count(*)  
from 
    naturals
where 
    isprime =1
;

attach "results.db" as db_results;
insert into db_results.results
select "once",(select count(*) from naturals where isPrime = 1),* from timing;