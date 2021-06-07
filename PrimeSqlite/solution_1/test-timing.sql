drop table if exists timing;
create table timing (
    what,
    time_stamp_jday,
    duration
);

-- first entry
insert into timing values ("start",julianday("now"),0);


-- since previous
with
  ts(time_stamp,previous_ts) as (
    select julianday("now"), (select max(time_stamp_jday) from timing)
  )
insert into timing 
select  "End task 3",
        ts.time_stamp,
        (ts.time_stamp - ts.previous_ts ) *86400.0
from ts
;

-- total time
with
  ts(time_stamp,previous_ts) as (
    select julianday("now"), (select min(time_stamp_jday) from timing)
  )
insert into timing 
select  "End",
        ts.time_stamp,
        (ts.time_stamp - ts.previous_ts ) *86400.0
from ts
;


select * from timing;