import std.algorithm : filter, sum;
import std.stdio : writeln;

int[] fib(immutable int cap) pure {
    int[] fibs = [1, 2];

    for(auto nxt = fibs[$ - 1] + fibs[$ - 2]; nxt < cap; nxt += fibs[$ - 2]) {
        fibs ~= nxt;
    }

    return fibs;
}

private unittest {
    assert(fib(90) == [1, 2, 3, 5, 8, 13, 21, 34, 55, 89]);
}

void main() {
    immutable cap = 4_000_000;
    static fibs = fib(cap);

    writeln(fibs.filter!(n => n % 2 == 0).sum);
}
