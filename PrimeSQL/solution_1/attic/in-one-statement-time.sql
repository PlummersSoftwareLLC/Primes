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

drop table if exists primes_table;
CREATE TABLE primes_table AS
with recursive 
-- configure the limit here
    max_limit(max_nr) as (
        select 1000000
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
    product (num,not_prime)
as (
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
select 
    n,
    case
        when not_prime is null then 1
    else 0
    end as isPrime
from naturals
left join (select distinct not_prime from product) unique_list on (
    naturals.n = unique_list.not_prime
);

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

attach "results.db" as db_results;
insert into db_results.results
select "in_one_statement",(select sum(isPrime) from primes_table),* from timing;