#!/usr/bin/tclsh

#
# Based on:
# - https://wiki.tcl-lang.org/page/Bit+vectors

 proc bit {varName pos {bitval {}}} {
    upvar 1 $varName var
    if {![info exist var]} {set var 0}
    set element [expr {$pos/32}]
    while {$element >= [llength $var]} {lappend var 0}
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
            if {$word & 1<<$i} {lappend res $pos}
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
    bits $primes
 }

dict set knownPrimeCounts 10 4, 
dict set knownPrimeCounts 100 25,                
dict set knownPrimeCounts 1000 168,
dict set knownPrimeCounts 10000 1229,
dict set knownPrimeCounts 100000 9592,
dict set knownPrimeCounts 1000000 78498,
dict set knownPrimeCounts 10000000 664579,
dict set knownPrimeCounts 100000000 5761455

set timeLimit 5
#set limit 1000000
set limit 100
time {set res [run_sieve $limit]}
puts $res