package main

import (
	"fmt"
)

func sumDivisibleBy(n uint64, cap uint64) uint64 {
	p := cap / n
	return n * p * (p + 1) / 2
}

func main() {
	cap := uint64(999)

	fmt.Println(sumDivisibleBy(3, cap) + sumDivisibleBy(5, cap) -
		sumDivisibleBy(15, cap))
}
