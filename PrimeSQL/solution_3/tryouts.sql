SET PAGESIZE 1000

-- static table to validate the result
create table  known_prime_counts (
  max_limit     INT,
  nr_of_primes  INT
 );

 insert into known_prime_counts 
              select 10,4 from dual
    union all select 100,25 from dual
    union all select 1000,168 from dual
    union all select 10000,1229 from dual
    union all select 100000,9592 from dual
    union all select 1000000,78498 from dual
    union all select 10000000,664579 from dual
    union all select 100000000,5761455 from dual
 ;

-- with recursive in Oracle
with naturals (n) as (
                select 2 from dual
      union all select n+1 from naturals where n=2
      union all select n+1 from naturals where n>2 and n <=max_limit
  )
  select n from naturals
;


-- time stamp
select systimestamp from dual;

create table TIMING as (select systimestamp  as time from dual);

create table TIMING as (select systimestamp  as time from dual);

select (time - systimestamp)  from TIMING;  

-- difference in time in seconds with a fraction
CREATE OR REPLACE function timestamp_diff(a timestamp, b timestamp) return number is 
begin
  return extract (day    from (a-b))*24*60*60 +
         extract (hour   from (a-b))*60*60+
         extract (minute from (a-b))*60+
         extract (second from (a-b));
end;
/

-- 
select timestamp_diff (systimestamp, time)  from TIMING; 




    select 2 from dual

with naturals (n) as (
                select 3 from dual
      union all select n+2 from naturals where n+2<100
  )
  select 2 from dual
  union all
  select n from naturals
;

-- private temp table
create PRIVATE TEMPORARY table ORA$PTT_naturals_table as (
  select n as n from (  
    with naturals (n) as (
                    select 3 from dual
          union all select n+2 from naturals where n+2<1000000
    )
    select 2 as n from dual
    union all
    select n as n from naturals
  )
)
;

-- real slow:
create PRIVATE TEMPORARY table ORA$PTT_naturals_table as (
  select 2 as n from dual
  union all
  select ((rownum * 2) +1) as n
  from ALL_OBJECTS
  where ((rownum * 2) +1) < 1000000
)
;

-- in one go the not primes:
create table not_primes as (
  select n as n from ( 
    with
    product (num,not_prime) as (
        select n as num,n*n as not_prime from naturals_table where n < 1000
      union all 
        select num,not_prime + (2 * num) 
        from product 
        where (not_prime + (2 * num))<=1000000
    )
    select not_prime as n from product
  )
)
;

set pagesize 1000
set timing on
--
-- below is the basic calculation
drop table naturals_table;
create table naturals_table as (
  select 2 as n from dual
  union all
  select n from (
    select level, ((level * 2) +1) as n
    from dual
    connect by level <(1000000/2)
  )
)
;
alter table naturals_table INMEMORY;


create index naturals_table_1 on naturals_table(n);


create table primes_first_segment as (
  select n from naturals_table where n <=1000
  minus
  select distinct(n) as n from ( 
    with
    product (num,not_prime) as (
        select n as num,n*n as not_prime from naturals_table where n < 334
      union all 
        select num,not_prime + (2 * num) 
        from product 
        where (not_prime + (2 * num))<=1000
    )
    select not_prime as n from product
  )
)
;

alter table primes_first_segment INMEMORY;

create index primes_first_segment_i on primes_first_segment(n);

-- one one go, is slow in oracle
create table primes as (
  select n from primes_first_segment
  union all 
  select n from naturals_table where n >1000
  minus
  select n as n from ( 
    with
    product (num,not_prime) as (
        select n,n*n from primes_first_segment
      union all 
        select num,not_prime + (2 * num) 
        from product 
        where (not_prime + (2 * num))<=1000000
    )
    select not_prime as n from product
  )
)
;

-- alternative:
-- 7 seconds
create table not_primes as (
  select n as n from ( 
    with
    product (num,not_prime) as (
        select n,n*n from primes_first_segment
      union all 
        select num,not_prime + (2 * num) 
        from product 
        where (not_prime + (2 * num))<=1000000
    )
    select not_prime as n from product
  )
)
;

alter table not_primes INMEMORY;
create index not_primes_i on not_primes(n);

-- this is fast
create table primes as (
  select n from primes_first_segment
  union all 
  select n from naturals_table where n >1000
  minus
  select n from not_primes
)
;

-- if done in one go
create table primes as (
  select n as n from (
          select n from naturals_table
    minus select n from not_primes
  )
)
;


select count(*) from naturals_table;
select count(*) from primes;


drop table primes_first_segment;
drop table primes;
drop table not_primes;
drop table naturals_table;

SELECT TABLE_NAME, INMEMORY FROM USER_TABLES
where table_name like '%PRIME%';

Alter system set inmemory_size = 20G scope=spfile;
set autotrace traceonly;