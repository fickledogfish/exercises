/*
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?
*/

package main

import "core:fmt"
import "core:math"
import "core:slice"
import "core:testing"

Number :: u64;

prime_factors :: proc(num: Number) -> []Number {
	max_factor := math.sqrt(f64(num));

	num := num;
	factors := make([dynamic]Number);

	if num % 2 == 0 {
		append(&factors, 2);
	}

	for i := Number(3); num > 1 && f64(i) < max_factor; i += 2 {
		if num % i == 0 {
			append(&factors, i);

			for num % i == 0 {
				num /= i;
			}
		}
	}

	return factors[:];
}

@(test)
test_prime_factors :: proc(t: ^testing.T) {
	num :: 13195;
	divs :: []Number{5, 7, 13, 29};

	facs := prime_factors(num);
	defer delete(facs);

	testing.expect(t, slice.equal(divs, facs[:]), fmt.tprintf(
		"expected prime_factors(%d) = %v, but got %v",
		num,
		divs,
		facs,
	));
}

NUM : Number : 600851475143;

main :: proc() {
	facs := prime_factors(NUM);
	defer delete(facs);

	fmt.println(facs[len(facs) - 1]);
}

@(test)
test_answer :: proc(t: ^testing.T) {
	answer :: Number(6857);

	facs := prime_factors(NUM);
	defer delete(facs);

	testing.expect(t, answer == facs[len(facs) - 1], fmt.tprintf(
		"incorrect answer, got prime_factors(%d) = %v",
		NUM,
		facs,
	));
}
