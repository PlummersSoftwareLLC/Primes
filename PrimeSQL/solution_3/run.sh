#!/bin/sh
sqlplus / as sysdba <<<'STARTUP' 1>/dev/null 2>&1

# Don't output oracle 'error' for password that is about to expire
sqlplus -F -S system/oracle <primes_1.sql | grep -v 'ERROR' | grep -v 'ORA-28002'
sqlplus -F -S system/oracle <primes_2.sql | grep -v 'ERROR' | grep -v 'ORA-28002'
