/*
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that
the 6th prime is 13.

What is the 10 001st prime number?
*/

/*
Checks if `n` is divisible by at least one element of `list`.
*/
fn is_divisible_by_any(n: &u64, list: &[u64]) -> bool {
    let mut check = false;

    for i in list {
        if n%i == 0 && i != &1 {
            check = true;
            break;
        }
    }

    check
}

/*
Create a list of `cap` primes.
*/
fn list_primes(cap: usize) -> Vec<u64> {
    let mut prime_list: Vec<u64> = vec![0; cap];

    let mut curr_count: usize = 0;
    let mut n: u64 = 2;

    // we can unwrap safetly because we initialized the whole vector at the
    // start of this function
    while prime_list.last().unwrap() == &0 {
        if !is_divisible_by_any(&n, &prime_list[..curr_count]) {
            prime_list[curr_count] = n;
            curr_count += 1;
        }
        n += 1;
    }

    prime_list
}

fn main() {
    let n: usize = 10_001;
    let list = list_primes(n);
    println!("{}", list.last().unwrap());
}
