/*
  Author:   Frank van Bakel
  
  Purpose:
  This script makes use of Oracle specific features
  and is an attempt to have an implementation as close as possible
  to the original C++ program by Dave.
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

  save_drop('type','bit_tab');
  save_drop('type','flag_t');

END;
/

/*
  Purpose:
  The types below are used to represent the bit array.
*/
CREATE or REPLACE TYPE flag_t as OBJECT (isPrime number(1));
/

CREATE or REPLACE TYPE bit_tab as TABLE of flag_t;
/

/*
  Create tables
*/
CREATE TABLE known_prime_counts (
  max_limit     INT,
  nr_of_primes  INT
);

/*
  Purpose:
  Create a static table to validate the result
*/
CREATE OR REPLACE procedure init_tables as
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
 
 END;
 /

/*
  Purpose:
  The procedure below is used to check if the caculation
  outcome is a valid number, based on the known counts
*/
CREATE or REPLACE function isValid(
    max_limit_in  NATURAL,
    count_nr      NATURAL
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
  bit_array       IN OUT  NOCOPY  bit_tab,
  max_limit       IN              NATURAL,
  show_results    IN              BOOLEAN,
  duration        IN              REAL,
  passes          IN              NATURAL
) as
    speed       REAL;
    count_nr    NATURAL;
    valid       VARCHAR(10);
BEGIN
  
    speed := duration / passes;

    select (count(*) +1) into count_nr from TABLE(bit_array) where isPrime=1;
    valid := isValid(max_limit,count_nr);

    if show_results then
        dbms_output.put(2|| ', ');
        for index_nr in 1..bit_array.COUNT
        loop
            if (bit_array(index_nr).isPrime = 1) then
              dbms_output.put_line(((index_nr *2 ) +1 ));
            end if;
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
                'fvbakel_Oracle1;' || passes ||
                ';' || duration ||
                ';1;algorithm=base,faithful=yes,bits=32'
                );

END;
/

/*
    Purpose:
    This procedure calculates the primes to the specified maxium number
*/
CREATE or REPLACE procedure run_sieve (
  bit_array  IN OUT  NOCOPY  bit_tab,
  max_limit  IN              NATURAL
) as
    flag_0          flag_t      default flag_t(0);
    flag_1          flag_t      default flag_t(1);
    index_nr        NATURAL     default 1;
    max_index_nr    NATURAL;
    q               NATURAL;
    q_index_nr      NATURAL;
    i               NATURAL;
    prime           NATURAL;
BEGIN
    -- Fill the array with all flags to 1
    -- also allocates the memory required
    max_index_nr  := floor(max_limit / 2);
    if mod(max_limit,2)=0 then
      max_index_nr := max_index_nr -1;
    end if;

    select flag_1
        bulk collect
    into
        bit_array
    from 
        (
          select level
          from dual
          connect by level <=max_index_nr
        )
    ;

    q             := floor(sqrt(max_limit));
    q_index_nr    := floor(q / 2);
    if mod(q,2)=0 then
      q_index_nr := q_index_nr -1;
    end if;

    while index_nr <= q_index_nr
    loop
      if bit_array(index_nr).isPrime = 1 then
        -- crossout
        prime := (index_nr * 2) + 1;
        i := floor((prime * prime) / 2);
        while i <= max_index_nr
        loop
          bit_array(i).isPrime := 0;
          i := i + prime;
        end loop;
      end if;
      index_nr := index_nr + 1;  
    end loop;
END;
/

/*
   Purpose:
   This is the main procedure that runs the run_sieve method as much as possible in 
   5 seconds
*/
CREATE or REPLACE procedure main as
  max_limit     NATURAL     default 1000000;
  show_results  BOOLEAN     default FALSE;
  max_time      NATURAL     default 5;
  duration      REAL        default 0.0;
  passes        NATURAL     default 0;
  bit_array     bit_tab;
  start_time    TIMESTAMP;
  now           TIMESTAMP;
BEGIN
  
    init_tables();
    start_time := CURRENT_TIMESTAMP();
    while duration < max_time
    loop
        bit_array := bit_tab();
        run_sieve(bit_array,max_limit);
        passes := passes + 1;
        now := CURRENT_TIMESTAMP();
        duration := TIMESTAMP_diff(now,start_time);
    end loop;

    print_results(
        bit_array       => bit_array,
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
