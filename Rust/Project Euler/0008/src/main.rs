/*
The four adjacent digits in the 1000-digit number that have the greatest product
are 9 × 9 × 8 × 9 = 5832.

73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450

Find the thirteen adjacent digits in the 1000-digit number that have the
greatest product. What is the value of this product?
*/

use std::env;
use std::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader};

/*
Read the number from the `filename` file.
*/
fn read_file(filename: &str) -> Result<String, String> {
    let f = match File::open(filename) {
        Ok(s) => s,
        Err(e) => return Err(e.description().to_string()),
    };

    let file = BufReader::new(&f);
    let mut buff: Vec<String> = vec![];

    for line in file.lines() {
        match line {
            Ok(l) => buff.push(l),
            Err(e) => return Err(e.description().to_string()),
        }
    }

    Ok(buff.join(""))
}

/*
Convert the number in string form into a vector of individual digits.
*/
fn convert(s: &String) -> Result<Vec<u8>, String> {
    let mut digits: Vec<u8> = vec![0; s.chars().count()];

    for (idx, c) in s.chars().enumerate() {
        let n = match c.to_digit(10) {
            Some(n) => n,
            None => return Err("Error converting to digit".to_string()),
        };

        digits[idx] = n as u8;
    }

    Ok(digits)
}

/*
Small convenience. Chains `read_file` and `convert`.
*/
fn read_and_convert(filename: &str) -> Result<Vec<u8>, String> {
    let contents = try!(read_file(filename));
    let converted: Vec<u8> = try!(convert(&contents));
    Ok(converted)
}

/*
Multiply every element in the slice.
*/
fn prod(v: &[u8]) -> u64 {
    let mut prod: u64 = 1;

    for i in 0..v.len() {
        prod *= v[i] as u64;
    }

    prod
}

/*
The actual algorithm to solve the problem.

Returns the biggest product found.
*/
fn find_biggest_prod(num: &Vec<u8>, size: usize) -> u64 {
    let mut biggest_so_far: u64 = 0;

    for slice in num.windows(size) {
        let p = prod(slice);

        if p > biggest_so_far {
            biggest_so_far = p;
        }
    }

    biggest_so_far
}

fn main() {
    let filename = match env::args().nth(1) {
        Some(a) => a,
        None => "num.txt".to_string()
    };

    let n = match read_and_convert(&filename) {
        Ok(n) => n,
        Err(e) => {
            println!("{}", e);
            return;
        },
    };

    println!("{:?}", find_biggest_prod(&n, 13));
}
