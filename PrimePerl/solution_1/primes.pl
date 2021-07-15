#!/usr/bin/env perl

use strict;
use warnings;

package PrimeSieve {
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

    sub new {
        my ( $class, $sieve_size ) = @_;

        return bless {
            sieve_size => $sieve_size,
            bits       => [],
        }, $class;
    }

    sub run_sieve {
        my $self = shift;

        my $factor = 3;
        my $q      = sqrt $self->{sieve_size};
        my @bits   = @{$self->{bits}};
        my $ss     = $self->{sieve_size};
        $#bits     = $ss;

        while ( $factor <= $q ) {
            for ( my $num = $factor ; $num < $ss ; $num += 2 ) {
                unless ( $bits[$num] ) {
                    $factor = $num;
                    last;
                }
            }

            my $num2 = $factor ** 2;
            while ( $num2 < $ss ) {
                $bits[$num2] = 1;
                $num2 += $factor * 2;
            }

            $factor += 2;
        }

        $self->{bits} = \@bits;
    }

    sub print_results {
        my ( $self, $show_results, $duration, $passes ) = @_;

        print "2, " if ($show_results);

        my $count = ( $self->{sieve_size} >= 2 );
        for ( my $num = 3 ; $num <= $self->{sieve_size} ; $num += 2 ) {
            unless ( $self->{bits}[$num] ) {
                printf( "%d, ", $num ) if ($show_results);
                $count++;
            }
        }

        print "" if ($show_results);

        printf "marghidanu;%d;%f;%d;algorithm=base,faithful=yes\n", $passes, $duration, 1;
#         printf STDERR
# "Passes: %d, Time: %f, Avg: %f, Limit: %d, Count1: %d, Count2: %d, Valid: %d\n",
#           $passes, $duration, $duration / $passes,
#           $self->{sieve_size}, $count, $self->count_primes(),
#           $self->validate_results();
    }

    sub count_primes {
        my $self = shift;

        my $count = ( $self->{sieve_size} >= 2 );
        for ( my $i = 3 ; $i < $self->{sieve_size} ; $i += 2 ) {
            $count++ unless ( $self->{bits}[$i] );
        }

        return $count;
    }

    sub validate_results {
        my $self = shift;

        return ( $DICT{ $self->{sieve_size} } == $self->count_primes() );
    }
};

package main {
    use Time::HiRes qw(time);

    my $passes     = 0;
    my $start_time = time;

    while (1) {
        my $sieve = PrimeSieve->new(1000000);
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
