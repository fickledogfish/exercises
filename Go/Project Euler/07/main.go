/*
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that
the 6th prime is 13.

What is the 10 001st prime number?
*/

package main

import "fmt"

func isDivisibleByAny(n uint64, list []uint64) bool {
	for _, i := range list {
		if n%i == 0 && i != 1 {
			return true
		}
	}

	return false
}

func listPrimes(cap uint32) (primeList []uint64) {
	primeList = make([]uint64, cap)

	currCount := uint64(0)
	for n := uint64(2); primeList[cap-1] == 0; n++ {
		if !isDivisibleByAny(n, primeList[:currCount]) {
			primeList[currCount] = n
			currCount++
		}
	}

	return
}

func main() {
	list := listPrimes(10001)
	fmt.Println(list[len(list)-1])
}
