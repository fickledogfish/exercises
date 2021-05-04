/*
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 *
 * Find the sum of all the primes below two million.
*/

// A type to hold the data. Should be big enough to hold the sum of all primes
// bellow 2M.
//
// `u32` was big enough to hold each individual prime, but the sum blew it up,
// so `u64` it is.
type Num = u64;

// Just to avoid hard-coding the number, a constant representing the desired
// limit of the primes.
//const MAXIMUM_PRIME: Num = 10;
const MAXIMUM_PRIME: Num = 2_000_000;

// Printing all this stuff probably makes the program run slower, so here's an
// option to disable it.
//
// Not that much slower. Results on my machine without reserving memory:
//
//     not printing: 75.43s user 0.06s system 100% cpu 1:15.41 total
//     printing:     76.91s user 0.38s system  99% cpu 1:17.41 total
//
// Huh. Well, the option's in place already, might as well keep it.
const PRINT_PROGRESS: bool = true;

/*
 * Checking a prime against a list of known primes.
 *
 * This avoids a bunch of checks, so it is much faster than the naive
 * approach. Still pretty slow, though.
 */
fn is_prime(acc: &[Num], num: &Num) -> bool {
    // `num & 1 == 0` is a quick and dirty way of checking if num is an even
    // number; basically, if the last bit isn't set, it's even.
    //
    // This check should make the program somewhat twice as fast, since it
    // avoids checking half the numbers.
    if num & 1 == 0 {
        return false;
    }

    // Now, if num is odd, we have a problem. Now we need to check against every
    // other prime on the accumulated list to see if `num` is a multiple of it.
    for p in acc {
        if num % p == 0 {
            return false;
        }
    }

    // If `num` isn't a multiple of anything on the list, it sure is a prime.
    true
}

/*
 * Generates a list of all primes less than `cap`.
 */
fn gen_primes(cap: Num) -> Vec<Num> {
    // Start primes with 2 because that's what the example showed. This also
    // affects the sum at the end, so not a good idea to change it.
    //
    // Should I reserve the memory for the primes? But how much?
    let mut primes: Vec<Num> = vec![2];
    //let mut primes: Vec<Num> = Vec::with_capacity(150_000);
    //primes.push(2);

    // In case the pre-calculated list decides to grow, don't hard-code the last
    // value, but get it dinamically.
    for i in (primes.last().unwrap() + 1)..cap {
        if PRINT_PROGRESS {
            // Printing the process probably makes this program much slower than
            // it needes to be, so make it a configurable option.
            print!(
                "{:03}% => checking {} (prime size: {:09})\r",
                (i as f32 / (MAXIMUM_PRIME - 1) as f32 * 100.0).round(),
                i,
                primes.len()
            );
        }

        if is_prime(&primes, &i) {
            primes.push(i);
        }
    }

    if PRINT_PROGRESS {
        // Put a new line to preserve the previous information on screen.
        println!("");
    }

    primes
}

fn main() {
    // 10 => 17
    // 2M => 142913828922
    println!("{}", gen_primes(MAXIMUM_PRIME).iter().sum::<Num>());
}
