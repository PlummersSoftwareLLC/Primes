#!/bin/bash -v

# collect cpu info and ignore errors
rm cpuinfo.db 
sqlite3 cpuinfo.db <make_cpuinfo_db.sql 1>/dev/null 2>/dev/null

# tests
sqlite3 <once.sql                   | grep -v off | grep -v exclusive                 
sqlite3 <once_1000.sql              | grep -v off | grep -v exclusive    
sqlite3 <in-one-statement-time.sql  | grep -v off | grep -v exclusive                
sqlite3 <in_one_2.sql               | grep -v off | grep -v exclusive   
sqlite3 <in_one_3.sql               | grep -v off | grep -v exclusive   
sqlite3 <in_one_4.sql               | grep -v off | grep -v exclusive   
sqlite3 <in_two_steps_1.sql         | grep -v off | grep -v exclusive         
sqlite3 <in_two_steps_2.sql         | grep -v off | grep -v exclusive         
                 
                 
                 