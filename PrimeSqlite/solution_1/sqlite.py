"""
Sqlite Prime Sieve

"""
import sqlite3


def init_db(limit):
    try:
        sqliteConnection = sqlite3.connect(':memory:')
        cursor = sqliteConnection.cursor()
        cursor.executescript('''
            create table variables(name STRING,value INT);

            insert into variables(name,value) values("last_prime",0);
            insert into variables(name,value) values("ready",0);
            insert into variables(name,value) values("max_root",0);
            '''
        )

        cursor.execute('''
            insert into variables(name,value) values (?,?);
            ''',
            ('limit',limit)
        )


    except sqlite3.Error as error:
        print("Error in init_db", error)
        return None
    finally:
        if cursor:
            cursor.close()

    return sqliteConnection

def run_sieve(sqliteConnection, limit):
    try:
        cursor = sqliteConnection.cursor()

        cursor.executescript('''
            -- clear previous results
            --drop index if exists primes_i;
            drop table IF EXISTS primes;
            
            create table primes(nr INT,isPrime INT);


            -- init the array
            INSERT into primes (nr,isPrime) values (2,1);

            -- init the variables
            UPDATE variables set value=2 where name ="last_prime";
            UPDATE variables set value=0 where name ="ready";

            UPDATE variables 
                -- below not used because it is not part of sqlite3 by default
                -- set value=floor(sqrt((select value from variables where name = "limit"))) 
                set value=(select value from variables where name = "limit")
            where name ="max_root";

            INSERT into primes (nr,isPrime)
            WITH RECURSIVE
            -- start at 3, ignore all even numbers and hard coded that 2 isPrime
                cnt(x) AS (VALUES(3) UNION ALL SELECT x+2 FROM cnt WHERE x<(select value from variables where name = "limit"))
            select x,1 FROM cnt WHERE x<(select value from variables where name = "limit");

            create unique index primes_i on primes(nr);

            -- array is initialized
            '''
        )

        #
        # below is the main reason to use python as "driver" language
        # sqlite does not support stored procedures
        ready =0
        while (ready ==0):
            run_sql_cycle(sqliteConnection)
            cursor.execute('''
                select value from variables where name="ready";
            '''
            )
            record = cursor.fetchall()
            ready=record[0][0]

    except sqlite3.Error as error:
        print("Error in run_sieve:", error)
        return None
    finally:
        if cursor:
            cursor.close()
    

def print_variables(sqliteConnection):
    try:
        cursor = sqliteConnection.cursor()

        cursor.execute('''
            select name,value from variables
            union
            select "check",(
                select 
                    case
                        when
                            ((select value from variables where name="last_prime")
                            <
                            (select value from variables where name="max_root"))
                        then 0
                        else 1
                    end
            )
            
            ;
            '''
        )
        record = cursor.fetchall()
        print("variables are: ", record)
        
    except sqlite3.Error as error:
        print("Error in print_variables", error)
    finally:
        if cursor:
            cursor.close()


def run_sql_cycle(sqliteConnection):
    try:
        cursor = sqliteConnection.cursor()
        cursor.executescript('''
            UPDATE variables set value= (
                select min(nr) 
                from primes
                where
                    nr > (select value from variables where name="last_prime")
            ) 
            where name ="last_prime"
            ;

            -- update all non primes
            WITH RECURSIVE
                cnt(x,y) AS (VALUES((select value from variables where name="last_prime"),(select value from variables where name="last_prime")) UNION ALL SELECT x+(2*y),y FROM cnt WHERE x<(select value from variables where name = "limit"))
            update primes set isPrime=0
            where nr in (select x from cnt where x!=(select value from variables where name="last_prime") and x<(select value from variables where name = "limit"))
            ;

            -- check if we are ready
            update variables set value= (
                select 
                    case
                        when
                            ((select value from variables where name="last_prime")
                            <
                            (select value from variables where name="max_root"))
                        then 0
                        else 1
                    end
            )
            where name="ready"
            ;


            '''
        )
    except sqlite3.Error as error:
        print("Error in run_sql_cycle:", error)
        return None
    finally:
        if cursor:
            cursor.close()


def print_primes(sqliteConnection,print_primes):
    try:
        cursor = sqliteConnection.cursor()

        if print_primes:
            cursor.execute('''
                select nr,isPrime from primes where isPrime = 1;
                '''
            )
            record = cursor.fetchall()
            print("Primes are: ", record)
        
        cursor.execute('''
            select count(nr) from primes where isPrime = 1;
            '''
        )
        record = cursor.fetchall()
        print("Number of primes are: ", record[0][0])
        
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
    parser.add_argument("--limit", "-l", help="Upper limit for calculating prime numbers", type=int, default=1_000_000)
    parser.add_argument("--time", "-t", help="Time limit", type=float, default=5)
    parser.add_argument("--show", "-s", help="Print found prime numbers", action="store_true")

    args = parser.parse_args()
    limit = args.limit
    timeout = args.time
    show_results = args.show

    sqliteConnection = init_db(limit)

    time_start = default_timer()                           # Record our starting time
    passes = 0                                             # We're going to count how many passes we make in fixed window of time

    while (default_timer() - time_start < timeout):        # Run until more than 10 seconds have elapsed
        run_sieve(sqliteConnection,limit)                                        # Calc the primes up to a million
        passes = passes + 1                                # Count this pass
        if passes == 2:
            break

    time_delta = default_timer() - time_start              # After the "at least 10 seconds", get the actual elapsed

    print_primes(sqliteConnection,show_results)

    close_db(sqliteConnection)


