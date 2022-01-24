# Interactive mode for testing
#"$ydb_dist"/yottadb -di
#exit

# Unfortunately, running all tests in the same process noticably impacts
# performance for all but the first test. So even though this works, it's
# better to run them all in separate instances of YottaDB.
#"$ydb_dist"/yottadb -run ^primes

"$ydb_dist"/yottadb -run test0^primes
"$ydb_dist"/yottadb -run test1^primes
"$ydb_dist"/yottadb -run test2^primes
"$ydb_dist"/yottadb -run test3^primes
"$ydb_dist"/yottadb -run test4^primes
"$ydb_dist"/yottadb -run test5^primes
