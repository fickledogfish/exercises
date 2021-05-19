/*
Problem 1: Multiples of 3 and 5

If we list all the natural numbers below 10 that are multiples of 3 or 5, we
get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
*/

package main

import "core:fmt"
import "core:math"
import "core:slice"
import "core:testing"

find_multiples_of :: proc(divisors: []int, cap: int) -> [dynamic]int {
	ret := make([dynamic]int);

	for i in 1..<cap {
		is_multiple_of_any := false;

		for divisor in divisors {
			if i % divisor == 0 {
				is_multiple_of_any = true;
				break;
			}
		}

		if is_multiple_of_any {
			append(&ret, i);
		}
	}

	return ret;
}

@(test)
test_find_multiples_of :: proc(t: ^testing.T) {
	// Provided test case
	divisors := []int{3, 5};
	cap := 10;

	muls := find_multiples_of(divisors, cap);
	defer delete(muls);

	expected := []int{3, 5, 6, 9};

	testing.expect(t, slice.equal(
		expected,
		muls[:],
	), fmt.tprintf(
		"expected find_multiples_of(%v, %d)=%v, but got %v",
		divisors,
		cap,
		expected,
		muls,
	));
}

DIVISORS :: []int{3, 5};
CAP :: 1000;

main :: proc() {
	muls := find_multiples_of(DIVISORS, CAP);
	defer delete(muls);

	sum := math.sum(muls[:]);

	fmt.println(sum);
}

@(test)
test_answer :: proc(t: ^testing.T) {
	answer :: 233168;

	muls := find_multiples_of(DIVISORS, CAP);
	defer delete(muls);
	sum := math.sum(muls[:]);

	testing.expect(t, sum == answer, fmt.tprintf(
		"Incorrect answer, got find_mutiples_of(%v, %d) = %d",
		DIVISORS,
		CAP,
		sum,
	));
}
