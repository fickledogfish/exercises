/*
A palindromic number reads the same both ways. The largest palindrome made from
the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
*/

package main

import (
	"fmt"
	"strconv"
)

func reverse(str string) string {
	n := len(str)

	runes := make([]rune, n)

	for _, rune := range str {
		n--
		runes[n] = rune
	}

	return string(runes[n:])
}

func isPalindrome(num uint64) (ret bool) {
	stringfied := strconv.FormatUint(num, 10)

	return stringfied == reverse(stringfied)
}

func largestPalindrome() (record uint64) {
	for a := uint64(100); a < 1000; a++ {
		for b := uint64(100); b < 1000; b++ {
			res := a * b

			if isPalindrome(res) && res > record {
				record = res
			}
		}
	}

	return
}

func main() {
	fmt.Println(largestPalindrome())
}
