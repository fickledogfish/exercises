# If we list all the natural numbers below 10 that are multiples of 3 or 5, we
# get 3, 5, 6 and 9. The sum of these multiples is 23.
#
# Find the sum of all the multiples of 3 or 5 below 1000.

use 5.025;
use strict;
use warnings;

use subs 'sum_divisible_by';

my $cap = 999;

my $sum3 = sum_divisible_by 3, $cap;
my $sum5 = sum_divisible_by 5, $cap;
my $sum15 = sum_divisible_by 15, $cap;

my $total = $sum3 + $sum5 - $sum15;

say "Multiples of 3 or 5 below $cap: $total";

sub sum_divisible_by {
    use integer;
    my ($n, $cap) = @_;

    my $p = $cap/$n;
    return $n*$p*($p + 1)/2;
}
