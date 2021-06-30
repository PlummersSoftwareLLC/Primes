#!/usr/bin/env perl

use strict;
use warnings;

use constant SIEVESIZE => 1000000;

package PrimeSieve {

    sub new {
        my ( $class ) = @_;

        return bless {
            bits       => [],
        }, $class;
    }

    sub run_sieve {
        my $self = shift;

        my $factor = 3;
        my $q      = sqrt main::SIEVESIZE;
        my $b      = $self->{bits};
        while ( $factor <= $q ) {
            my $num = $factor;
            for ( my $num = $factor ; $num < main::SIEVESIZE ; $num += 2 ) {
                unless ( $b->[$num] ) {
                    $factor = $num;
                    last;
                }
            }

            my $num2 = $factor * $factor;
            while ( $num2 < main::SIEVESIZE ) {
                $b->[$num2] = 1;
                $num2 += $factor * 2;
            }

            $factor += 2;
        }
    }

    sub print_results {
        my ( $self, $show_results, $duration, $passes ) = @_;

        print "2, " if ($show_results);

        my $count = ( main::SIEVESIZE >= 2 );
        for ( my $num = 3 ; $num <= main::SIEVESIZE ; $num += 2 ) {
            unless ( $self->{bits}[$num] ) {
                printf( "%d, ", $num ) if ($show_results);
                $count++;
            }
        }

        print "" if ($show_results);

        printf "dohnuts;%d;%f;%d;algorithm=base,faithful=yes\n", $passes, $duration, 1;
#         printf STDERR
# "Passes: %d, Time: %f, Avg: %f, Limit: %d, Count1: %d, Count2: %d, Valid: %d\n",
#           $passes, $duration, $duration / $passes,
#           main::SIEVESIZE, $count, $self->count_primes(),
#           $self->validate_results();
    }

    sub count_primes {
        my $self = shift;

        my $count = ( main::SIEVESIZE >= 2 );
        for ( my $i = 3 ; $i < main::SIEVESIZE ; $i += 2 ) {
            $count++ unless ( $self->{bits}[$i] );
        }

        return $count;
    }

    sub validate_results {
        my $self = shift;
        my $x = $self->count_primes();
        my %DICT = (
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
        );
        my $y = $DICT{ main::SIEVESIZE };
        # my $y = 78498;
        return ( $y == $x );
    }
};

package main {
    use Time::HiRes qw(time);

    my $passes     = 0;
    my $start_time = time;

    while (1) {
        my $sieve = PrimeSieve->new();
        $sieve->run_sieve();
        $passes++;

        my $duration = time - $start_time;
        if ( $duration >= 5 ) {
            $sieve->print_results( 0, $duration, $passes );
            last;
        }
    }
};

__END__

