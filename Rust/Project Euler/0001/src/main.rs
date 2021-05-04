/*
                        Problem 1: Multiples of 3 and 5
                        ===============================


If we list all the natural numbers below 10 that are multiples of 3 or 5, we get
3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
*/

pub fn list_multiples(cap: u64) -> Vec<u64> {
    let mut multiples: Vec<u64> = Vec::new();
    let mut count = 1;

    loop {
        if count >= cap {
            break;
        }

        if count % 3 == 0 || count % 5 == 0 {
            multiples.push(count)
        }

        count += 1;
    }

    multiples
}

fn main() {
    let cap = 1000;
    println!("{}", list_multiples(cap).iter().sum::<u64>());
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn multiples_under_10() {
        assert_eq!(list_multiples(10), vec![3, 5, 6, 9]);
    }
}
