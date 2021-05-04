/*
 * The following iterative sequence is defined for the set of positive integers:
 *
 *     n -> n/2    (n is even)
 *     n -> 3n + 1 (n is odd)
 *
 * Using the rule above and starting with 13, we generate the following sequence:
 *
 *     13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
 *
 * It can be seen that this sequence (starting at 13 and finishing at 1) contains
 * 10 terms. Although it has not been proved yet (Collatz Problem), it is thought
 * that all starting numbers finish at 1.
 *
 * Which starting number, under one million, produces the longest chain?
 *
 * NOTE: Once the chain starts the terms are allowed to go above one million.
 *
 * Answer: 837_799
 */

type CollatzNum = u64;
type CollatzSeq = Vec<CollatzNum>;

const COLLATZ_CAP: CollatzNum = 1_000_000;

fn main() {
    let (num, _) = naive::solution(COLLATZ_CAP);
    //let (num, _) = map::solution(COLLATZ_CAP);
    //let (num, _) = static_arr::solution(COLLATZ_CAP);
    println!("{}", num);
}

fn next_collatz(n: CollatzNum) -> CollatzNum {
    if n & 1 == 0 {
        n / 2
    } else {
        3 * n + 1
    }
}

#[allow(dead_code)]
mod naive {
    use super::*;

    pub fn collatz(n: CollatzNum) -> CollatzSeq {
        let mut seq = vec![];
        let mut next = n;

        while next != 1 {
            next = next_collatz(next);
            seq.push(next);
        }

        seq
    }

    pub fn solution(n: CollatzNum) -> (CollatzNum, usize) {
        let mut top_collatz = 0;
        let mut top_len = 0;

        for i in 1..=n {
            let c = collatz(i);

            if c.len() > top_len {
                top_collatz = i;
                top_len = c.len();
            }
        }

        (top_collatz, top_len)
    }
}

#[allow(dead_code)]
mod map {
    use super::*;

    use std::collections::BTreeMap;
    type CollatzMap = BTreeMap<CollatzNum, CollatzSeq>;

    pub fn collatz(n: CollatzNum, precalc: &mut CollatzMap) -> CollatzSeq {
        let mut seq = Vec::new();
        let mut next = n;

        while next != 1 {
            if let Some(s) = precalc.get(&next) {
                seq.extend(s);
                break;
            }

            next = next_collatz(next);
            seq.push(next);
        }

        precalc.insert(n, seq.clone());

        seq
    }

    pub fn solution(n: CollatzNum) -> (CollatzNum, usize) {
        let mut collatz_table = CollatzMap::new();

        for i in 1..=n {
            collatz(i, &mut collatz_table);
        }

        collatz_table
            .iter()
            .fold((0, 0), |(top_collatz, top_len), (num, seq)| {
                if seq.len() > top_len {
                    (*num, seq.len())
                } else {
                    (top_collatz, top_len)
                }
            })
    }
}

#[allow(dead_code)]
mod static_arr {
    use super::*;

    //const TABLE_SIZE: usize = 1_000_000;
    const TABLE_SIZE: usize = 1;

    fn lookup(n: CollatzNum) -> CollatzNum {
        static mut TABLE_LEN: usize = 0;
        static mut COLLATZ_TABLE: [(CollatzNum, CollatzNum); TABLE_SIZE] = [(0, 0); TABLE_SIZE];

        let curr_len = unsafe { TABLE_LEN };

        for i in 0..curr_len {
            let (num, nxt) = unsafe { COLLATZ_TABLE[i] };

            if n == num {
                return nxt;
            }
        }

        let nxt = next_collatz(n);

        unsafe {
            TABLE_LEN += 1;
            COLLATZ_TABLE[TABLE_LEN] = (n, nxt);
        }

        nxt
    }

    pub fn collatz(mut n: CollatzNum) -> CollatzSeq {
        let mut seq = Vec::new();

        while n != 1 {
            n = lookup(n);
            seq.push(n);
        }

        seq
    }

    pub fn solution(n: CollatzNum) -> (CollatzNum, usize) {
        let mut top_collatz = 0;
        let mut top_len = 0;

        for i in 1..=n {
            let c = collatz(i);

            if c.len() > top_len {
                top_collatz = i;
                top_len = c.len();
            }
        }

        (top_collatz, top_len)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn naive_collatz_13() {
        // Instead of 10 elements as enunciated above, I'm only counting 9, as
        // 13 won't be part of my list. This makes more sense when considering
        // the other implementations.
        assert_eq!(naive::collatz(13).len(), 9);
        assert_eq!(naive::collatz(13), [40, 20, 10, 5, 16, 8, 4, 2, 1]);
    }

    #[test]
    fn naive_solution() {
        assert_eq!(naive::solution(10), (9, 19));
        assert_eq!(naive::solution(100), (97, 118));
        assert_eq!(naive::solution(1_000), (871, 178));
        assert_eq!(naive::solution(10_000), (6_171, 261));
    }

    #[test]
    fn hashmap_solution() {
        assert_eq!(hashmap::solution(10), (9, 19));
        assert_eq!(hashmap::solution(100), (97, 118));
        assert_eq!(hashmap::solution(1_000), (871, 178));
        assert_eq!(hashmap::solution(10_000), (6_171, 261));
    }

    #[test]
    fn static_arr_collatz_13() {
        assert_eq!(naive::collatz(13).len(), 9);
        assert_eq!(naive::collatz(13), [40, 20, 10, 5, 16, 8, 4, 2, 1]);
    }

    #[test]
    fn static_arr_solution() {
        assert_eq!(static_arr::solution(10), (9, 19));
        assert_eq!(static_arr::solution(100), (97, 118));
        assert_eq!(static_arr::solution(1_000), (871, 178));
        assert_eq!(static_arr::solution(10_000), (6_171, 261));
    }
}
