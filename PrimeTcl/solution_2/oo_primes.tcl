#!/usr/bin/tclsh
#
# Tcl implementation of Prime Sieve
#
# Author:   Frank van Bakel
# Date:     2020-06-02
#
# Based on:
# - https://wiki.tcl-lang.org/page/Bit+vectors
# - PrimeCPP.cpp : Dave's Garage Prime Sieve in C++
# - Python Prime Sieve: recuces the bit array size by using it only for the odd numbers

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

::oo::class create bit_array {
    variable bit_vector
    variable size

    constructor {size_input init_value} {
        variable bit_vector
        variable size

        set size $size_input

        set word_fill " 0xFFFFFFFF"
        if {$init_value == 0} {
            set word_fill " 0x00000000"
        }

        set bit_vector [string repeat "$word_fill" [expr {($size+31)/32}]]

        # mask out excess bits at the end
        for {set i [expr $size]} {$i<=(($size+31)/32)*32} {incr i} {
            my bit $i 0
        }
    }

    destructor {
        # no sub objects to be cleared
    }

    #
    # set the value of a bit at the given position to the given value
    # if no value is specified the value of the given bit is returned
    method bit {pos {bitval {}}} {
        variable bit_vector

        set element [expr {$pos/32}]
        while {$element >= [llength $bit_vector]} {
            lappend bit_vector 0
        }
        set bitpos [expr {1 << $pos%32}]
        set word [lindex $bit_vector $element]
        if {$bitval != ""} {
            if {$bitval} {
                set word [expr {$word | $bitpos}]
            } else {
                set word [expr {$word & ~$bitpos}]
            }
            lset bit_vector $element $word
        }
        expr {($word & $bitpos) != 0}
    }

    #
    # Convert the bit array to an array of index numbers
    method bits {} {
        variable bit_vector
        
        set res {}
        set pos 0
        foreach word $bit_vector {
            for {set i 0} {$i<32} {incr i} {
                if {$word & 1<<$i} {lappend res $pos}
                incr pos
            }
        }
        return $res
    }

}

::oo::class create prime_sieve {
    variable limit
    variable primes
    variable size
    
    constructor {limit_input} {
        variable limit
        variable primes
        variable size

        set limit $limit_input
        set size [expr {$limit_input/2}]

        if {[expr ($limit_input%2)]} {
            # uneven, add one to the size
            incr size 
        }

        set primes [bit_array new $size 1]

    }

    destructor {
        $primes destroy
    }

    #
    # Convert an array of index numbers to prime numbers
    method bits_to_primes {} {
        variable primes

        set res {}
        set pos 1

        lappend res 2
        set i 0    
        foreach bit_index [$primes bits] {
            if {$i!=0} {
                lappend res [expr {($bit_index * 2) + 1}]
            }
            incr i
        }

        return $res
    }

    #
    # Calculate the bit array
    method run_sieve {} {
        variable limit
        variable primes
        variable size

        set maxroot [expr {sqrt($limit)}]
       
        set factor 1
        while {$factor<$maxroot} {
            # Start at the square root of factor
            # But since factor is the index of the odd number, 
            # it requires some calculation
            #
            set cur_prime [expr ($factor *2) + 1]
            set start [expr (($cur_prime * $cur_prime) / 2)]
            set step [expr ($factor * 2) + 1]
            set i $start
            while {$i<=$size} {
                $primes bit $i 0
                incr i $step
            }
            # search next bit set to 1
            for {set i [expr {$factor + 1}]} {$i<=$size} {incr i} {
                if {[$primes bit $i]} {
                    set factor $i
                    break
                }
            }
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
        puts "fvbakel_ootcl;$passes;$duration;1;algorithm=base,faithful=yes,bits=1"

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
