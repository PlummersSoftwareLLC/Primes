/*
  Author:   Frank van Bakel

  Purpose:
  This script is as close as possible to the SQLLite
  solution_1. 
*/

SET SERVEROUTPUT ON
SET FEEDBACK OFF
SET LINESIZE 200

/*
  Purpose:
  Calculate the difference between two TIMESTAMPs in seconds
*/
CREATE OR REPLACE function TIMESTAMP_diff(a TIMESTAMP, b TIMESTAMP) return number is 
BEGIN
  return extract (day    from (a-b))*24*60*60 +
         extract (hour   from (a-b))*60*60+
         extract (minute from (a-b))*60+
         extract (second from (a-b));
END;
/

/*
  Purpose:
  Drop a schema object if it exists
*/
CREATE OR REPLACE procedure save_drop (
  object_type  IN VARCHAR2,
  object_name  IN VARCHAR2
) as
BEGIN
  BEGIN
    EXECUTE IMMEDIATE 'DROP ' || object_type || ' ' || object_name;
  EXCEPTION
    WHEN OTHERS THEN
        -- this is a bit unsave incase of other errors, 
        -- but oke for now
        NULL;
  END;
END;
/

/*
  Purpose:
  The code block below is excecuted before the create statements below
  to make sure that fresh instances are created
*/
BEGIN
  save_drop('table','known_prime_counts');
  save_drop('table','primes_table_template');
  save_drop('table','primes_first_segment');
  save_drop('table','primes');
  save_drop('table','not_primes');
  save_drop('table','naturals_table');
END;
/

/*
  Create tables
*/
create table known_prime_counts (
  max_limit     INT,
  nr_of_primes  INT
);

create table primes_table_template (
  n     INT
);

create table naturals_table (
  n     INT
);

create table primes_first_segment (
  n     INT
);
create table not_primes (
  n     INT
);
create table primes (
  n     INT
);

alter table naturals_table INMEMORY;
alter table primes_first_segment INMEMORY;
alter table not_primes INMEMORY;
alter table primes INMEMORY;

/*
  Purpose:
  - Create a static table to validate the result
  - Create a static table with 2 plus the odd natural numbers
*/
CREATE OR REPLACE procedure init_tables(
  max_limit  IN  INT
) as
BEGIN

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
  commit;

  insert into primes_table_template
    select 2 as n from dual
    union all
    select n from (
      select level, ((level * 2) +1) as n
      from dual
      connect by level <(max_limit/2)
    )
  ;
  commit;
  
 END;
 /

/*
  Purpose:
  The procedure below is used to check if the caculation
  outcome is a valid number, based on the known counts
*/
CREATE or REPLACE function isValid(
    max_limit_in  INT,
    count_nr      INT
) return VARCHAR2 is
  valid     VARCHAR2(10);
BEGIN
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
            known_prime_counts.max_limit = max_limit_in
        and nr_of_primes = count_nr 
    )
  ;

  return valid;
END;
/

/*
  Purpose:
  The procedure below is used to print the results after 
  the calculation
*/
CREATE or REPLACE procedure print_results(
  max_limit       IN    INT,
  show_results    IN    BOOLEAN,
  duration        IN    REAL,
  passes          IN    INT
) as
    speed       REAL;
    count_nr    INT;
    valid       VARCHAR(10);
BEGIN
  
    speed := duration / passes;

    select count(*) into count_nr from primes;
    valid := isValid(max_limit,count_nr);

    if show_results then
        for rec in (select n from primes order by n asc)
        loop
            dbms_output.put_line(rec.n);
        end loop;
        dbms_output.put(chr(10));
    END if;

    dbms_output.put_line(
                'Passes: '  || passes ||
                ', Time: '  || duration ||
                ', Avg: '   || speed || ' (sec/pass)' ||
                ', Limit: ' || max_limit ||
                ', Count: ' || count_nr ||
                ', Valid: ' || valid
                );

    dbms_output.put(chr(10));
    dbms_output.put_line(
                'fvbakel_Oracle2;' || passes ||
                ';' || duration ||
                ';1;algorithm=other,faithful=no,bits=32'
                );

END;
/

/*
    Purpose:
    This procedure calculates the primes to the specified maxium number
*/
CREATE or REPLACE procedure run_sieve (
   max_limit  IN    INT
) as
BEGIN
    -- make sure we start with empty calculation tables
    EXECUTE IMMEDIATE 'truncate table naturals_table';
    EXECUTE IMMEDIATE 'truncate table primes_first_segment';
    EXECUTE IMMEDIATE 'truncate table primes';
    EXECUTE IMMEDIATE 'truncate table not_primes';

    insert into naturals_table
      select n from primes_table_template
    ;
    
    -- first get the primes that are smaller than the square root of the limit
    insert into primes_first_segment
      select n from naturals_table where n <=sqrt(max_limit)
      minus
      select distinct(n) as n from ( 
        with
        product (num,not_prime) as (
            select n as num,n*n as not_prime from naturals_table where n < sqrt(sqrt(max_limit))
          union all 
            select num,not_prime + (2 * num) 
            from product 
            where (not_prime + (2 * num))<=sqrt(max_limit)
        )
        select not_prime as n from product
      )
    ;
    
    -- the statement below is taking most of the time
    insert into not_primes
      select distinct(n) as n from ( 
        with
        product (num,not_prime) as (
            select n,n*n from primes_first_segment
          union all 
            select num,not_prime + (2 * num) 
            from product 
            where (not_prime + (2 * num))<=max_limit
        )
        select not_prime as n from product
      )
    ;
    
    insert into primes
      select n from primes_first_segment
      union all 
      select n from naturals_table where n >sqrt(max_limit)
      minus
      select n from not_primes
    ;
    
END;
/

/*
   Purpose:
   This is the main procedure that runs the run_sieve method as much as possible in 
   5 seconds
*/
CREATE or REPLACE procedure main as
  max_limit     INT         default 1000000;
  show_results  BOOLEAN     default FALSE;
  max_time      INT         default 5;
  duration      REAL        default 0.0;
  passes        INT         default 0;
  start_time    TIMESTAMP;
  now           TIMESTAMP;
BEGIN
  
    init_tables(max_limit);
    start_time := CURRENT_TIMESTAMP();
    while duration < max_time
    loop
        run_sieve(max_limit);
        passes := passes + 1;
        now := CURRENT_TIMESTAMP();
        duration := TIMESTAMP_diff(now,start_time);
    end loop;

    print_results(
        max_limit       => max_limit,
        show_results    => show_results,
        duration        => duration,
        passes          => passes
    );

END;
/

BEGIN
  main();
END;
/
