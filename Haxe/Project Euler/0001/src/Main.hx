/*
Problem 1: Multiples of 3 and 5

If we list all the natural numbers below 10 that are multiples of 3 or 5, we
get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
*/

function main() {
	var muls = find_multiples([3, 5], 10);
	trace(muls, sum(muls));

	trace(sum(find_multiples([3, 5], 1000)));
}

function find_multiples(divisors: Array<Int>, cap: Int): List<Int> {
	var ret = new List<Int>();

	for (i in 2...cap) {
		for (divisor in divisors) {
			if (i % divisor == 0) {
				ret.add(i);
				break;
			}
		}
	}

	return ret;
}

function sum(lst: List<Int>): Int {
	var ret = 0;

	for (el in lst) {
		ret += el;
	}

	return ret;
}
