# By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
# that the 6th prime is 13.
#
# What is the 10 001st prime number?

use 5.025;
use strict;
use warnings;

use subs 'gen_n_primes';

my $count = 6;
my @primes = gen_n_primes $count;
say $primes[-1];

sub is_divisible_by_any {
    my ($n, $list_ref) = @_;

    foreach (@$list_ref) {
        if($n % $_ == 0) {
            return 1;
        }
    }

    return 0;
}

sub gen_n_primes {
    my ($count) = @_;

    my @primes;

    for(my $n = 2; scalar(@primes) < $count; $n++) {
        if(!is_divisible_by_any $n, \@primes) {
            push @primes, $n;
        }
    }

    return @primes;
}
