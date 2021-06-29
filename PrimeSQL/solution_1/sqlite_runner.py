"""
Sqlite Prime Sieve

Although this is a Python script, the actual calculation of 
the prime sieve is done in SQLite.

"""
import sqlite3
from math import sqrt

prime_counts = { 10 : 4,                 # Historical data for validating our results - the number of primes
                100 : 25,                # to be found under some limit, such as 168 primes under 1000
                1000 : 168,
                10000 : 1229,
                100000 : 9592,
                1000000 : 78498,
                10000000 : 664579,
                100000000 : 5761455
                }

def init_db(limit):
    """
        - Initialize a in memory database in sqlite
        - set important sqlite performance parameters, 
          specific for the problem at hand
        - passing of configuration parameters to sqlite
        - workaround for the lack of a sqrt built in function in sqlite
    """
    try:
        sqliteConnection = sqlite3.connect(':memory:')
        cursor = sqliteConnection.cursor()
        cursor.executescript('''
            PRAGMA TEMP_STORE = 2;
            PRAGMA JOURNAL_MODE = OFF;
            PRAGMA SYNCHRONOUS = 0;
            PRAGMA LOCKING_MODE = EXCLUSIVE;

            -- table to hold the limit, 
            -- and way to pass this argument to the sqlite script
            -- this method has less impact than a paramater during run_sieve
            drop table if exists max_limit_conf;
            create table max_limit_conf (
                max_nr      INT,
                sqrt_max    INT -- workaround because sqlite does not have sqrt function
            );  

            '''
        )
        cursor.execute('''
            insert into max_limit_conf(max_nr,sqrt_max) values (?,?);
            ''',
            ( 
                limit,round(sqrt(limit))
            )
        )
    except sqlite3.Error as error:
        print("Error in init_db", error)
        return None
    finally:
        if cursor:
            cursor.close()

    return sqliteConnection

def run_sieve(sqliteConnection):
    """
        This function does the actual calculation of the prime numbers
        - it clears any previous results
        - in the first stage all prime numbers smaller than the square root
          of the limit are calculated. This is done with a brute force method
        - in the second stage the elimination is done for the found prime numbers 
          in the first stage
        - all prime numbers are stored in a table, and this is considered the
          end result.   
    """
    try:
        cursor = sqliteConnection.cursor()

        cursor.executescript('''
drop table if exists primes_table;
CREATE TABLE primes_table AS
with recursive 
  naturals_1(n)
  -- init of the natural numbers 2,3,5,7,...
  as (
      select 2
      union all
          select n+1 from naturals_1 where n=2
      union all
          select n+2 from naturals_1 where n>2 and n+2<=(select sqrt_max from max_limit_conf)
  ),
  --
  -- in the recursive call below we are calculating everything that can not be prime
  -- based on the primes < 1000 we found in the first run
  product_1 (num,not_prime)
  as (
    select n, n*n as sqr
      from naturals_1
      where 
            sqr <= (select sqrt_max from max_limit_conf)
    union all -- all because we know there is no overlap between the two sets, is a bit faster than just union
    select 
      num, -- because recursive does not allow to reuse n
      not_prime+2*num as prod --2*num because we know that every other number must be even
    from
      product_1
    where
      prod <= (select sqrt_max from max_limit_conf)
  ),
  -- all primes <= sqrt from the max_limit
  primes_1(n)
  as (
      select n from naturals_1
      except
      select product_1.not_prime from product_1
  ),
  -- Below is for all primes > sqrt from the max_limit
  -- init of the natural numbers for the second segment , eg 1001,1003,1005
  naturals(n)
  as (
    select (select sqrt_max from max_limit_conf) + 1
    union all
        select n+2 from naturals 
        where 
                n+2<=(select max_nr from max_limit_conf)
  ),
  --
  -- in the recursive call below we are calculating everything that can not be prime
  product (num,not_prime)
  as (
    select n, n*n as sqr
      from primes_1
      where 
            sqr <= (select max_nr from max_limit_conf)
        and n !=2 -- this filters out all the recursive calls for evennumbers!
    union all -- all because we know there is no overlap between the two sets, is a bit faster than just union
    select 
      num, -- because recursive does not allow to reuse n
      not_prime+2*num as prod --2*num because we know that every other number must be even
    from
      product
    where
      prod <= (select max_nr from max_limit_conf)
  ),
  primes(n)
  as (
      select n from primes_1
      union all
      select n from naturals
      except
      select product.not_prime from product
  )
select n from primes
;
            '''
        )
    except sqlite3.Error as error:
        print("Error in run_sieve:", error)
        return None
    finally:
        if cursor:
            cursor.close()
    
def print_results(sqliteConnection,show_results, duration, passes):
    try:
        cursor = sqliteConnection.cursor()

        if show_results:
            cursor.execute('''
                select n from primes_table;
                '''
            )
            record = cursor.fetchall()
            print("Primes are: ", record)
        
        cursor.execute('''
            select count(*) from primes_table;
            '''
        )
        record = cursor.fetchall()
        count = record[0][0]

        if show_results:
            print()
        print("Passes: %s, Time: %s, Avg: %s, Limit: %s, Count: %s, Valid: %s" % (passes, duration, duration/passes, limit, count, validate_results(limit,count)))

        # Following 2 lines added by to conform to drag race output format
        print()
        print("fvbakel_sqlite; %s;%s;1;algorithm=other,faithful=no,bits=8" % (passes, duration))
          
    except sqlite3.Error as error:
        print("Error in print_primes", error)
        return None
    finally:
        if cursor:
            cursor.close()

def close_db(sqliteConnection):
    if sqliteConnection:
        sqliteConnection.close()

def validate_results(limit,count):                      # Check to see if this is an upper_limit we can

    """Look up our count of primes in the historical data (if we have it)
    to see if it matches"""

    if limit in prime_counts:                # the data, and (b) our count matches. Since it will return
        return prime_counts[limit] == count  # false for an unknown upper_limit, can't assume false == bad
    else:
        return 'unkown'
    return False

# MAIN Entry
if __name__ == "__main__":
    from argparse import ArgumentParser
    from timeit import default_timer  # For timing the durations

    parser = ArgumentParser(description="Sqlite Prime Sieve")
    parser.add_argument("--limit", "-l", help="Upper limit for calculating prime numbers", type=int, default=1_000_000)
    parser.add_argument("--time", "-t", help="Time limit", type=float, default=5)
    parser.add_argument("--show", "-s", help="Print found prime numbers", action="store_true")
    parser.add_argument("--version", "-v", help="Print version information", action="store_true")


    args = parser.parse_args()
    limit = args.limit
    timeout = args.time
    show_results = args.show

    if args.version:
        import sys
        print("Python version", sys.version_info)
        print ("SQLite version", sqlite3.sqlite_version)
        print("SQLite python module version ",sqlite3.version)
        exit()

    sqliteConnection = init_db(limit)

    time_start = default_timer()                           # Record our starting time
    passes = 0                                             # We're going to count how many passes we make in fixed window of time

    while (default_timer() - time_start < timeout):        # Run until more than 5 seconds have elapsed
        run_sieve(sqliteConnection)                        # Calc the primes with sqlite
        passes = passes + 1                                # Count this pass

    time_delta = default_timer() - time_start              # After the "at least 5 seconds", get the actual elapsed

    print_results(sqliteConnection,show_results,time_delta,passes)

    close_db(sqliteConnection)


