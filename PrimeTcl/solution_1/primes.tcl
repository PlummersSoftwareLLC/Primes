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

set time_limit 5
set limit 1000000

# 0=off, 1=on
set show_results 0

dict set knownPrimeCounts 10 4
dict set knownPrimeCounts 100 25
dict set knownPrimeCounts 1000 168
dict set knownPrimeCounts 10000 1229
dict set knownPrimeCounts 100000 9592
dict set knownPrimeCounts 1000000 78498
dict set knownPrimeCounts 10000000 664579
dict set knownPrimeCounts 100000000 5761455

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


proc bits bitvec {
    set res {}
    set pos 0
    foreach word $bitvec {
        for {set i 0} {$i<32} {incr i} {
            if {$word & 1<<$i} {
                lappend res $pos
            }
            incr pos
        }
    }
    set res
}

proc run_sieve max {
    set maxroot [expr {sqrt($max)}]
    set primes [string repeat " 0xFFFFFFFF" [expr {($max+31)/32}]]
    bit primes 0 0; bit primes 1 0
    for {set i [expr $max+1]} {$i<=(($max+31)/32)*32} {incr i} {
        bit primes $i 0 ;# mask out excess bits
    }
    for {set i 2} {$i<=$maxroot} {incr i} {
       if {[bit primes $i]} {
           for {set j [expr $i<<1]} {$j<=$max} {incr j $i} {
               bit primes $j 0
           }
       }
    }
    #bits $primes
    set primes
}

proc check_valid {limit count} {
    global knownPrimeCounts
    set res "false"
    set known_value [dict get $knownPrimeCounts $limit]
    puts "known_value=$known_value"
    if {$known_value==$count } {
        set res "true"
    }
    set res
}

proc print_results {primes limit show_results duration passes} {
    #upvar 1 $primes_name primes
    set prime_nrs [bits $primes]

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
    puts "fvbakeltcl;$passes;$duration;1"

}


proc main {time_limit limit show_results} {
#time {set res [run_sieve $limit]}
#puts $res
    set stop_after_ms [expr {$time_limit * 1000} ]
    set passes 0
    set start [clock milliseconds]
    while {1} {
        incr passes
        set primes [run_sieve $limit]
        set now [clock milliseconds]
        set duration_ms [expr {$now -$start}]
        set duration_sec [expr {$duration_ms /1000}]
        if {$duration_ms > $stop_after_ms} {
            print_results $primes $limit $show_results $duration_sec $passes
            break
        }
    }
}

main $time_limit $limit $show_results
