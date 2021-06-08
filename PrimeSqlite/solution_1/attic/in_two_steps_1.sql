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

-- configure the limit here
drop table if exists max_limit_conf;
create table max_limit_conf as 
        select 1000000 as max_nr,
               1000 as sqr -- workaround because sqlite does not have sqrt function
;

-- first entry
insert into timing values ("start",julianday("now"),0);

drop table if exists primes_first_segment;
CREATE TABLE primes_first_segment AS
with recursive 
-- configure the limit here
    max_limit(max_nr) as (
        select sqr from max_limit_conf
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
            sqr <= (select max_nr from max_limit)
    union all -- all because we know there is no overlap between the two sets, is a bit faster than just union
    select 
      num, -- because recursive does not allow to reuse n
      not_prime+2*num as prod --2*num because we know that every other number must be even
    from
      product
    where
      prod <= (select max_nr from max_limit)
  ),
  primes(n)
  as (
      select n from naturals
      except
      select product.not_prime from product
  )
select n from primes
;


with
  ts(time_stamp,previous_ts) as (
    select julianday("now"), (select max(time_stamp_jday) from timing)
  )
insert into timing 
select  "First segment calculation",
        ts.time_stamp,
        (ts.time_stamp - ts.previous_ts ) *86400.0
from ts
;

-- second run
drop table if exists primes_table;
CREATE TABLE primes_table AS
with recursive 
-- configure the limit here
    start_at(n) as (
        select 1001
),
    max_limit(max_nr) as (
        select max_nr from max_limit_conf
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
            sqr <= (select max_nr from max_limit)
        and n !=2 -- this filters out all the recursive calls for evennumbers!
    union all -- all because we know there is no overlap between the two sets, is a bit faster than just union
    select 
      num, -- because recursive does not allow to reuse n
      not_prime+2*num as prod --2*num because we know that every other number must be even
    from
      product
    where
      prod <= (select max_nr from max_limit)
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

-- outputs
select count(*) from primes_table;
attach "results.db" as db_results;
insert into db_results.results
select "in_two_steps_1",(select count(*) from primes_table),* from timing;