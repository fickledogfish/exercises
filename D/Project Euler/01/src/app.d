import std.algorithm.iteration : sum;
import std.range : iota;
import std.stdio : writeln;

bool is_multiple_of_any(immutable int n, immutable int[] divisors) pure {
    foreach(i; divisors) {
        if(n % i == 0)
            return true;
    }

    return false;
}

int[] multiples_in_range(immutable int[] divisors, immutable int lower,
                         immutable int cap) pure {
    int[] multiples;

    foreach(i; 0.iota(cap))
        if(i.is_multiple_of_any(divisors))
            multiples ~= i;

    return multiples;
}

private unittest {
    assert([3, 5].multiples_in_range(0, 10).sum == 23);
}

void main() {
    immutable lower = 0;
    immutable limit = 1_000;
    immutable divisors = [3, 5];

    writeln(divisors.multiples_in_range(lower, limit).sum);
}
