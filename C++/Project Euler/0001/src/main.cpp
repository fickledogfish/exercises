/*
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, we
 * get 3, 5, 6 and 9. The sum of these multiples is 23.
 *
 * Find the sum of all the multiples of 3 or 5 below 1000.
 *
 * Answer: 233168
 */

#include <iostream>
#include <vector>
#include <numeric>

std::vector<int> find_multiples_of(
	const std::vector<int> &divisors,
	const int cap
) {
	std::vector<int> nums;
	nums.reserve(10);

	for (int i = 1; i < cap; ++i) {
		bool is_multiple_of_any = false;

		for (auto divisor : divisors) {
			if (i % divisor == 0) {
				is_multiple_of_any = true;
				break; // only the inner loop
			}
		}

		if (is_multiple_of_any) nums.push_back(i);
	}

	return nums;
}

void print_vector(const std::vector<int> &v) {
	const int v_len = v.size();

	std::cout << "{ ";

	for (int i = 0; i < v_len; ++i) {
		std::cout << v[i];

		if (i != v_len - 1)
			std::cout << ", ";
		else
			std::cout << ' ';
	}

	std::cout << "}";
}

auto main() -> int {
	{ // Sample case: multiples of 3 and 5 below 10
		const std::vector<int> expected = {3, 5, 6, 9};
		const int expected_sum = 23;

		// What the solution found
		const auto nums = find_multiples_of({3, 5}, 10);
		const int nums_sum = std::accumulate(
			nums.begin(),
			nums.end(),
			0
		);

		if (nums.size() != expected.size() ||
			nums != expected ||
			nums_sum != expected_sum
		) {
			std::cout << "Expected find_multiples_of({3, 5}, 10) to be ";
			print_vector(expected);
			std::cout << " (len = "
				<< expected.size()
				<< ", sum = "
				<< expected_sum
				<< "), but it returned ";
			print_vector(nums);
			std::cout << " (len = " << nums.size() << ", sum = "
				<< nums_sum
				<< ")"
				<< std::endl;

			return -1;
		} else {
			std::cout << "Solution passes the sample case" << std::endl;
		}
	}

	{ // Solution
		const auto result = find_multiples_of({3, 5}, 1000);

		std::cout << "Found "
			<< result.size()
			<< " numbers, which sum up to "
			<< std::accumulate(result.begin(), result.end(), 0)
			<< std::endl;
	}
}
