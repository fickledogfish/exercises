/*
 * 3: Largest prime factor
 *
 * The prime factors of 13195 are 5, 7, 13 and 29.
 *
 * What is the largest prime factor of the number 600851475143?
 */

import Foundation

let answer = 6857

func prime_factors_of(_ num: Int) -> [Int] {
    let maxFactor = Int(ceil(Double(num).squareRoot()))

    var num = num
    var factors = [Int]()

    factors.append(1)

    if 0 == num % 2 {
        factors.append(2)
        num = reduce_all_ns(num, 2)
    }

    var currentFactor = 3
    while num > 1 && currentFactor <= maxFactor {
        if 0 == num % currentFactor {
            factors.append(currentFactor)
            num = reduce_all_ns(num, currentFactor)
        }

        currentFactor += 1
    }

    return factors
}

func reduce_all_ns(_ num: Int, _ factor: Int) -> Int {
    var num = num

    while 0 == num % factor {
        num /= factor
    }

    return num
}

func main() {
    let result = prime_factors_of(600851475143).max { (a: Int, b: Int) -> Bool in a < b }!

    print((result == answer ? "[ok]" : "[fail]") + " got \(result)")
}

main()