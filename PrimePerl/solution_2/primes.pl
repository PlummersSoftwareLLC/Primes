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
        bless {
            sieve_size => $sieve_size,
            bits       => 0 x $sieve_size,
        }, $class;
    }

    sub run_sieve {
        my $self   = shift;
        my $size   = $self->{sieve_size};
        my $bits   = \$self->{bits};
        my $q      = sqrt $size;
        my $factor = 1;
        while ( $factor <= $q ) {
            $factor += 2;
            $factor += 2 while $factor < $size and substr($$bits,$factor,1);
            my $repeat = 1  .  0 x (2*$factor-1);
            my $times = (length($$bits)-$factor**2)/2/$factor + 1;
            my $s = 0 x $factor**2  .  $repeat x $times;
            $$bits |= $s;
        }
    }

    sub primes {
        my $self = shift;
        my $bits=\ $self->{bits};
        grep!substr($$bits,$_,1),2,grep$_%2,3..$self->{sieve_size};
    }

    sub print_results {
        my ( $self, $show_results, $duration, $passes ) = @_;
        my @primes = $self->primes();
        my $count = 0 + @primes;
        print join", ", @primes if $show_results;
        my $f = $DICT{$self->{sieve_size}} == $count ? 'yes' : 'no';
        printf "kjetillll;%d;%f;%d;algorithm=base,faithful=%s\n", $passes, $duration, 1, $f;
#        printf STDERR "Passes: %d, Time: %f, Avg: %f, Limit: %d, Count1: %d, Count2: %d, Valid: %d\n",
#           $passes, $duration, $duration / $passes,
#           $self->{sieve_size}, $count, $self->count_primes(),
#           $self->validate_results();
    }

    sub count_primes {
        my $self = shift;
        0 + $self->primes();
    }

    sub validate_results {
        my $self = shift;
        $DICT{ $self->{sieve_size} } == $self->count_primes();
    }
};

package main {
    use Time::HiRes 'time';
    my $passes     = 0;
    my $start_time = time();
    my $sieve;
    sub duration { time() - $start_time }

    while ( duration() < 5 ) {
        $sieve = PrimeSieve->new(1000000);
        $sieve->run_sieve();
        $passes++;
    }
    $sieve->print_results( 0, duration(), $passes );
};

__END__
