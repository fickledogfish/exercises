/*
 * A Pythagorean triplet is a set of three natural numbers, a < b < c, for
 * which,
 *
 *     a^2 + b^2 = c^2
 *
 * For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
 *
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 * Find the product abc.
 */

// First, define a type for the numbers. Should be big enough to hold a * b * c,
// but not too big.
//
// After some testing, u32 seems to be the choice.
type Num = u32;

// Next, a simple way to check if a given triplet of numbers is a Pythagorean
// triplet.
fn is_triplet(a: Num, b: Num, c: Num) -> bool {
    a * a + b * b == c * c
}

fn main() {
    // Just for error checking.
    let mut found = false;

    // Now, just brute force our way through. Just kidding.
    //
    // For starters, no need to start checking at zero.
    //
    // I could probably make a go to infinity, but since I'm error checking at
    // the end, I'd better put a safe stop here.
    'outer: for a in 1..500 {
        // Since a * b == b * a, b can start on the following number.
        //
        // I cant, however, let this go to infinity, or this loop will never
        // end, so 500 it is. If I can't find the Pythagorean triplet like this,
        // I'll try to increase it to 1000.
        for b in (a + 1)..500 {
            // Pretty much the same thing as b.
            for c in (b + 1)..500 {
                // Only triplets which sum to 1000 are interesting, so no need
                // to check the others.
                if a + b + c == 1000 && is_triplet(a, b, c) {
                    // Making sure the error at the end won't print.
                    found = true;

                    // And show the results.
                    let prod = a * b * c;
                    println!("{}^2 + {}^2 = {}^2 ({})", a, b, c, prod);
                    break 'outer;
                }
            }
        }
    }

    // Just as a warning. Better to print this than do nothing I guess.
    if !found {
        println!("Not a single Pythagorean triplet was found");
    }
}
