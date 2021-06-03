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

#
# set the value of a bit at the given position to the given value
proc bit {varName pos {bitval {}}} {
    upvar 1 $varName var
    if {![info exist var]} {
        set var 0
    }
    set element [expr {$pos/32}]
    while {$element >= [llength $var]} {
        lappend var 0
    }
    set bitpos [expr {1 << $pos%32}]
    set word [lindex $var $element]
    if {$bitval != ""} {
        if {$bitval} {
            set word [expr {$word | $bitpos}]
        } else {
            set word [expr {$word & ~$bitpos}]
        }
        lset var $element $word
    }
    expr {($word & $bitpos) != 0}
}

#
# Convert the bit array to an array of index numbers
proc bits bitvec {
    set res {}
    set pos 0
    foreach word $bitvec {
        for {set i 0} {$i<32} {incr i} {
            if {$word & 1<<$i} {lappend res $pos}
            incr pos
        }
    }
    return $res
 }

#
# Convert an array of index numbers to prime numbers
proc bits_to_primes bitvec {
    set res {}
    set pos 1

    lappend res 2
    set i 0    
    foreach bit_index [bits $bitvec] {
        if {$i!=0} {
        lappend res [expr {($bit_index * 2) + 1}]
        }
        incr i
    }

    return $res
}

#
# Calculate the bit array
proc run_sieve {primesName max} {
    upvar 1 $primesName primes

    set maxroot [expr {sqrt($max)}]
    set size [expr {$max/2}]
    if {[expr ($max%2)]} {
        # uneven, add one to the size
        incr size 
    }
    set primes [string repeat " 0xFFFFFFFF" [expr {($size+31)/32}]]

    # mask out excess bits at the end
    for {set i [expr $size]} {$i<=(($size+31)/32)*32} {incr i} {
        bit primes $i 0
    }
    
    set factor 1
    while {$factor<$maxroot} {
        set start [expr ($factor * 3) + 1]
        set step [expr ($factor * 2) + 1]
        set i $start
        while {$i<=$size} {
            bit primes $i 0
            incr i $step
        }
        # search next bit set to 1
        for {set i [expr {$factor + 1}]} {$i<=$size} {incr i} {
            if {[bit primes $i]} {
                set factor $i
                break
            }
        }
    }
}

#
# Check if the number of found primes matches the 
# known number of primes for the given limit
proc check_valid {limit count} {
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
proc print_results {primesName limit show_results duration passes} {
    upvar 1 $primesName primes
    
    set prime_nrs [bits_to_primes $primes]

    if {$show_results==1} {
        puts "Primes found:"
        puts "$prime_nrs"
    }
    
    set avg [expr {$duration / $passes}]
    set count [llength $prime_nrs]
    set valid [check_valid $limit $count]
    puts "Passes: $passes, Time: $duration, Avg: $avg (sec/pass), Limit: $limit, Count: $count, Valid: $valid"
    # Following 2 lines are to conform to drag race output format
    puts ""
    puts "fvbakeltcl;$passes;$duration;1;algorithm=base,faithful=yes,bits=1"

}

#
# The entry point
proc main {time_limit limit show_results} {
    set stop_after_ms [expr {$time_limit * 1000} ]
    set passes 0
    set start [clock milliseconds]
    while {1} {
        incr passes
        set primes  {}

        run_sieve primes $limit

        set now [clock milliseconds]
        set duration_ms [expr {$now -$start}]
        set duration_sec [expr {double($duration_ms) /1000}]

        if {$duration_ms > $stop_after_ms} {
            print_results primes $limit $show_results $duration_sec $passes
            break
        }
    }
}

# run main, pass the contants defined at the top
main $time_limit $limit $show_results
