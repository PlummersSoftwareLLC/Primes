#!/usr/bin/env perl
package PrimeSieve;
use v5.12;
use strict;
use warnings;
use PDL;
use PDL::NiceSlice;

my %primes_lower_than = (
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
    # die "Expect a positive power of ten" unless (0+$sieve_size) =~ /^10+$/;
    my $bits=zeroes(byte, $sieve_size+1);
    my $q=sqrt($sieve_size);
    $bits(0:1).=1; # 0 and 1 are not prime
    return bless {
	sieve_size => $sieve_size,
	bits       => $bits,
	q          => $q,
	even       => 0,
	ran        => 0
    }, $class;
}

sub run_sieve {
    my $self = shift;
    return if $self->{ran};
    my $q      = $self->{q};
    my $bits   = $self->{bits};
    my $one=pdl(byte, 1);
    for(my $factor=3; $factor<=$q; $factor+=2) {
	$bits($factor*$factor:-1:2*$factor).=$one unless $bits(($factor));
    }
    $self->{ran}=1;
}

sub print_results {
    my ( $self, $show_primes, $show_stats, $duration, $passes ) = @_;
    say $self->get_primes if $show_primes;
    printf "Luis_Mochán_(wlmb)_Perl/PDL;%d;%f;%d;algorithm=base,faithful=yes,bits=8\n",
	$passes, $duration, 1;
    say "Passes: $passes, Time: $duration, Per pass: ", $duration/$passes,
	" Limit: ", $self->{sieve_size}, " Count: ", $self->count_primes,
	" Valid: ", $self->validate_results if $show_stats;
}

sub deal_with_even {
    my $self = shift;
    my $bits=$self->{bits};
    $bits(2*2:-1:2).=1 unless $self->{even};
    $self->{even}=1;
}

sub get_primes {
    my $self = shift;
    $self->deal_with_even;
    my $bits=$self->{bits};
    $bits->long->xvals->where(!$bits);
}

sub count_primes {
    my $self = shift;
    $self->deal_with_even;
    my $bits=$self->{bits};
    (!$bits)->sumover;
}

sub validate_results {
    my $self = shift;
    return ( $primes_lower_than{ $self->{sieve_size} } == $self->count_primes() );
}


package main;
use Time::HiRes qw(time);
my $size=1_000_000;
my $passes = 0;
my $duration=0;
my $sieve;
my $start_time = time;
while ($duration<5) {
    $sieve = PrimeSieve->new($size);
    $sieve->run_sieve();
    $passes++;
    $duration = time - $start_time;
}
$sieve->print_results( 0, 0, $duration, $passes );

__END__
