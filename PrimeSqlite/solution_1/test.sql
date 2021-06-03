
PRAGMA recursive_triggers = ON;

create table primes(nr INT,isPrime INT);
create TEMP table not_primes(nr INT);

-- dummy table and view to enable a recursive trigger
--create table run_sieve_table(new_prime INT);
create VIEW run_sieve_table(new_prime) as select 1;

create table last_prime(nr INT);
insert into last_prime(nr) values (0);

create table not_prime(nr INT);

create trigger fill_not_primes
AFTER INSERT on not_prime
BEGIN
    insert into not_prime(nr)
    select next_nr from (
        SELECT NEW.nr+(2*(select min(nr) from last_prime))  as next_nr
    )
    WHERE next_nr<=10
    ;
END;

create trigger run_sieve_table
INSTEAD OF INSERT on run_sieve_table
BEGIN
    -- stripout the not primes for new_prime
    -- below is not working inside a trigger :-(
 --   WITH RECURSIVE
--        cnt(x,y) AS (VALUES(NEW.new_prime,NEW.new_prime) UNION ALL SELECT x+(2*y),y FROM cnt WHERE x<=10)
--    update primes set isPrime=0
--    where nr in (select x from cnt where x!=NEW.new_prime and x<=10)
--    ;
    -- so as a workaround, use an other trigger

    -- only one row
    update last_prime set nr = NEW.new_prime;

  --  create TEMP table not_prime(nr INT);
    -- start at 3 * prime number, the trigger recursion will handle it
    insert into not_prime(nr)
    select NEW.new_prime*3
    ;

    update primes set isPrime=0
    where nr in (select nr from not_prime)
    ;


  --  drop table not_prime;

    -- find the next prime and insert that in the table to cause a recursive call
    insert into run_sieve_table(new_prime)
    select min(nr)
    from primes
    where
       nr > NEW.new_prime
    ;    
END;


-- init the array
INSERT into primes (nr,isPrime) values (2,1);

INSERT into primes (nr,isPrime)
WITH RECURSIVE
  -- start at 3, ignore all even numbers and hard coded that 2 isPrime
  cnt(x) AS (VALUES(3) UNION ALL SELECT x+2 FROM cnt WHERE x<=10)
select x,1 FROM cnt;

create unique index primes_i on primes(nr);

insert into run_sieve_table(new_prime) values (3);  

select nr from primes where isPrime = 1;
