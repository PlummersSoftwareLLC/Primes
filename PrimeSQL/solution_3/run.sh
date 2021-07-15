#!/bin/sh
sqlplus / as sysdba <<<'STARTUP'

sqlplus -F system/oracle  <primes_1.sql

sqlplus -F -S system/oracle <primes_1.sql