-- based on https://stackoverflow.com/questions/7370761/sqlite-loop-statements
-- and https://blog.devart.com/increasing-sqlite-performance.html

PRAGMA TEMP_STORE = 2;
PRAGMA JOURNAL_MODE = OFF;
PRAGMA SYNCHRONOUS = 0;
PRAGMA LOCKING_MODE = EXCLUSIVE;


--begin transaction;

drop table if exists naturals;
create table naturals
( n integer unique primary key asc,
  isprime bool);
 -- factor integer);

with recursive
  nn (n)
as (
  select 2
  union all
  select n+1 as newn from nn
  where newn <= 25
)
insert into naturals
--select n, 1, null from nn;
select n, 1 from nn;

insert or replace into naturals
  with recursive
    product (prime,not_prime)
  as (
    select n, n*n as sqr
      from naturals
      where sqr <= (select max(n) from naturals)
    union all
    select prime, not_prime+prime as prod
    from
      product
    where
      prod <= (select max(n) from naturals)
  )
--select n, 0, prime
select n, 0
from product join naturals
  on (product.not_prime = naturals.n)
;

--commit;

-- output
select * --count(n) 
from 
    naturals;
    /*
where 
    isprime =1
;*/