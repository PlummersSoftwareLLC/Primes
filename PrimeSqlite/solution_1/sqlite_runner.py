"""
Sqlite Prime Sieve

"""
import sqlite3

prime_counts = { 10 : 4,                 # Historical data for validating our results - the number of primes
                100 : 25,                # to be found under some limit, such as 168 primes under 1000
                1000 : 168,
                10000 : 1229,
                100000 : 9592,
                1000000 : 78498,
                10000000 : 664579,
                100000000 : 5761455
                }

def init_db():
    try:
        sqliteConnection = sqlite3.connect(':memory:')
        cursor = sqliteConnection.cursor()
        cursor.executescript('''
            PRAGMA TEMP_STORE = 2;
            PRAGMA JOURNAL_MODE = OFF;
            PRAGMA SYNCHRONOUS = 0;
            PRAGMA LOCKING_MODE = EXCLUSIVE;

            -- configure the limit here
            drop table if exists max_limit_conf;
            create table max_limit_conf as 
                select 1000000 as max_nr,
                       1000 as sqrt_max -- workaround because sqlite does not have sqrt function
            ;

            '''
        )
    except sqlite3.Error as error:
        print("Error in init_db", error)
        return None
    finally:
        if cursor:
            cursor.close()

    return sqliteConnection

def run_sieve(sqliteConnection):
    try:
        cursor = sqliteConnection.cursor()

        cursor.executescript('''
drop table if exists primes_table;
CREATE TABLE primes_table AS
with recursive 
  naturals_1(n)
  -- init of the narural numbers 2,3,5,7,...
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
            select count(*) from primes_table;;
            '''
        )
        record = cursor.fetchall()

        count = record[0][0]
        if show_results:
            print()
        print("Passes: %s, Time: %s, Avg: %s, Limit: %s, Count: %s, Valid: %s" % (passes, duration, duration/passes, self._size, count, self.validate_results()))

        # Following 2 lines added by to conform to drag race output format
        print()
        print("fvbakel_sqlite; %s;%s;1" % (passes, duration))
          
    except sqlite3.Error as error:
        print("Error in print_primes", error)
        return None
    finally:
        if cursor:
            cursor.close()



def close_db(sqliteConnection):
    if sqliteConnection:
        sqliteConnection.close()
        print("The SQLite connection is closed")

# MAIN Entry
if __name__ == "__main__":
    from argparse import ArgumentParser
    from timeit import default_timer  # For timing the durations

    parser = ArgumentParser(description="Sqlite Prime Sieve")
    parser.add_argument("--time", "-t", help="Time limit", type=float, default=5)
    parser.add_argument("--show", "-s", help="Print found prime numbers", action="store_true")

    args = parser.parse_args()
    limit = args.limit
    timeout = args.time
    show_results = args.show

    sqliteConnection = init_db(limit)

    time_start = default_timer()                           # Record our starting time
    passes = 0                                             # We're going to count how many passes we make in fixed window of time

    while (default_timer() - time_start < timeout):        # Run until more than 5 seconds have elapsed
        run_sieve(sqliteConnection)                        # Calc the primes with sqlite
        passes = passes + 1                                # Count this pass
        if passes == 2:
            break

    time_delta = default_timer() - time_start              # After the "at least 5 seconds", get the actual elapsed

    print_results(sqliteConnection,show_results,time_delta,passes)

    close_db(sqliteConnection)


