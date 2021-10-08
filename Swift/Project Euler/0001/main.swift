/*
 * 1: Multiples of 3 and 5
 *
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, we
 * get 3, 5, 6 and 9. The sum of these multiples is 23.
 *
 * Find the sum of all the multiples of 3 or 5 below 1000.
 */

let answer = 233168

func list_multiples_of(_ divisors: [Int], _ cap: Int) -> [Int] {
    return (0..<cap).filter { (num: Int) -> Bool in
        0 < divisors.map { 0 == num % $0 }.filter { $0 }.count
    }
}

func main() {
    let sum = list_multiples_of([3, 5], 1000).reduce(0, +)

    print((sum == answer ? "[ok]" : "[fail]") + " got \(sum)")
}

main()
