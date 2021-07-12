#!/usr/bin/tclsh
#
# Tcl implementation of Prime Sieve
#
# Author:   Frank van Bakel
# Date:     2020-06-02
#
# Based on:
# - PrimeCPP.cpp : Dave's Garage Prime Sieve in C++

set time_limit 5
set limit 1000000
# 0=off, 1=on
set show_results 0

# Dictionary of known number of primes, used in validation
dict set knownPrimeCounts 10 4
dict set knownPrimeCounts 100 25
dict set knownPrimeCounts 1000 168
dict set knownPrimeCounts 10000 1229
dict set knownPrimeCounts 100000 9592
dict set knownPrimeCounts 1000000 78498
dict set knownPrimeCounts 10000000 664579
dict set knownPrimeCounts 100000000 5761455

::oo::class create prime_sieve {
    variable limit
    variable bit_array
    
    constructor {limit_input} {
        variable limit
        variable bit_array

        set limit $limit_input
        set size [expr $limit +1]
        set bit_array [lrepeat $size 1]
    }

    destructor {
        # nothing to destroy
    }

    #
    # Convert the bit_array to prime numbers
    method bits_to_primes {} {
        variable bit_array
        set res {}

        lappend res 2
        set i 0    
        foreach {even odd} $bit_array {
            if {$i != 0  && $odd == 1} {
                lappend res $odd
            }
            incr i 2
        }
        return $res
    }

    #
    # Calculate the bit array
    method run_sieve {} {
        variable limit
        variable bit_array

        set maxroot [expr {round(floor(sqrt($limit)))}]
       
        set num 3
        while {$num<$maxroot} {
            # search next bit set to 1
            foreach {odd even} [lrange bit_array $num $maxroot] {
                if { $odd == 1} {
                    break
                }
                incr num 2
            }

            # Start at the square root of factor
            set start [expr $num * $num]
            set step [expr $num * 2]
            set i $start
            while {$i<=$limit} {
                lset bit_array $i 0
                incr i $step
            }

            incr num 2
        }
    }

    #
    # Check if the number of found primes matches the 
    # known number of primes for the given limit
    method check_valid {count} {
        variable limit
 
        global knownPrimeCounts

        set res "false"

        if {[dict exists $knownPrimeCounts $limit]} { 
            set known_value [dict get $knownPrimeCounts $limit]
            if {$known_value==$count } {
                set res "true"
            } 
        } else {
                set res "unknown"
            }
        return $res
    }

    #
    # Write the results to the output
    method print_results {show_results duration passes} {
        variable limit

        set prime_nrs [my bits_to_primes]

        if {$show_results==1} {
            puts "Primes found:"
            puts "$prime_nrs"
        }
        
        set avg [expr {$duration / $passes}]
        set count [llength $prime_nrs]
        set valid [my check_valid $count]
        puts "Passes: $passes, Time: $duration, Avg: $avg (sec/pass), Limit: $limit, Count: $count, Valid: $valid"
        # Following 2 lines are to conform to drag race output format
        puts ""
        puts "fvbakel_ootcl2;$passes;$duration;1;algorithm=base,faithful=yes,bits=32"

    }
}

#
# The entry point
proc main {time_limit limit show_results} {
    set stop_after_ms [expr {$time_limit * 1000} ]
    set passes 0
    set start [clock milliseconds]
    while {1} {
        incr passes
        
        set sieve [prime_sieve new $limit]

        $sieve run_sieve 

        set now [clock milliseconds]
        set duration_ms [expr {$now -$start}]
        set duration_sec [expr {double($duration_ms) /1000}]

        if {$duration_ms > $stop_after_ms} {
            $sieve print_results $show_results $duration_sec $passes
            break
        }

        $sieve destroy
    }
}

# run main, pass the contants defined at the top
main $time_limit $limit $show_results
