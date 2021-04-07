#!/usr/bin/env perl

use strict;
use warnings;

package PrimeSieve {
    my $DICT = {
        10          => 4,
        100         => 25,
        1000        => 168,
        10000       => 1229,
        100000      => 9592,
        1000000     => 78498,
        10000000    => 664579,
        100000000   => 5761455,
        1000000000  => 50847534,
        10000000000 => 455052511,
    };

    sub new {
        my ( $class, $sieve_size ) = @_;

        my $self = {
            sieve_size => $sieve_size,
            bits       => [],
        };
        bless( $self, $class );

        return $self;
    }

    sub run_sieve {
        my $self   = shift();
        my $factor = 3;

        while ( $factor <= sqrt $self->{sieve_size} ) {
            my $num1 = $factor;
            while ( $num1 < $self->{sieve_size} ) {
                if ( !$self->{bits}->[$num1] ) {
                    $factor = $num1;
                    last;
                }

                $num1 += 2;
            }

            my $num2 = $factor * $factor;
            while ( $num2 < $self->{sieve_size} ) {
                $self->{bits}->[$num2] = 1;
                $num2 += $factor << 1;
            }

            $factor += 2;
        }
    }

    sub count_primes {
        my $self = shift();

        my $count = 1;
        for ( my $i = 3 ; $i < $self->{sieve_size} ; $i += 2 ) {
            $count++ if ( !$self->{bits}->[$i] );
        }

        return $count;
    }

    sub validate_results {
        my $self = shift();

        return $DICT->{ $self->{sieve_size} } == $self->count_primes();
    }

    sub print_results {
        my ( $self, $show_results, $duration, $passes ) = @_;

        my $count = 1;
        for ( my $num = 3 ; $num <= $self->{sieve_size} ; $num += 2 ) {
            $count++ if ( !$self->{bits}->[$num] );
        }

        printf(
"Passes: %d Time: %s Avg: %s Limit: %d Count1: %d Count2: %d Valid: %d\n",
            $passes, $duration, ( $duration / $passes ),
            $self->{sieve_size}, $count, $self->count_primes(),
            $self->validate_results()
        );
    }
}

package main {
    my $passes = 0;
    my $start  = time();

    while (1) {
        my $sieve = PrimeSieve->new(1000000);
        $sieve->run_sieve();
        $passes++;

        my $duration = time() - $start;
        if ( $duration >= 5 ) {
            $sieve->print_results( 0, $duration, $passes );
            last;
        }
    }
}
