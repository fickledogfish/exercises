/*
# The Goal

Big News!

The SETI program has received a series of messages from Alpha Centauri. The most
frequent message seems to be:

    541a4231 5d324646 27219a26 12497b0e 724eddcb 0e131617 9521bedf 55544dc7

It is not known why these messages are encoded, but there is a good chance that
the Alpha Centaurians are trying to evaluate our cognitive abilities before
establishing advanced contact.

Our best engineers are working to decode these messages, and they've already
succeeded at identifying the program that the Centaurians use to encode the
messages. This program takes a size and a list of numbers as inputs. It then
outputs the encoded message (see the pseudo-code below).

But so far, no one has been able to decode the messages. We are well aware that
this task is by far the hardest that we’ve encountered, and that only a true
NERD will be able to pull it off!

## Rules

Here is a pseudo-code version of the encoding program:

    READ size
    READ size / 16 integers in array a
    WRITE size / 16 zeros in array b

    For i from 0 to size - 1:
        For j from 0 to size - 1:
            b[(i+j)/32] ^= ((a[i/32] >> (i%32)) & (a[j/32 + size/32] >>
            (j%32)) & 1) << ((i+j)%32)

    PRINT b

You can download a C++ version of the program here.

The goal is to determine the series of numbers entered (array a) from the
encoded output of the program (array b). The numbers --- input and output ---
should be displayed in hexadecimal, 8 characters padded with 0 (for example, 42
would be displayed as 0000002a).

If you pass the output of your program as input to the encoder above, you should
obtain the input provided to your program.

If there are several possible decoded values, you should display all the
possibilities in alphabetical order.

## Game Input

### Input

Line 1: size S

Line 2: N1 integers in hexadecimal format, separated by blank spaces

### Output

N2 integers in hexadecimal format, representing the decoded message.

### Constraints

0 < S <= 256
0 < N1 <= 16
0 < N2 <= 32

## Example

Input
> 32
> 46508fb7 6677e201

Output
> b0c152f9 ebf2831f
> ebf2831f b0c152f9
*/

#include <algorithm>
#include <iostream>
#include <iomanip>
#include <string>
#include <vector>

typedef uint8_t messagesize;
typedef uint32_t messagedata;

// print a vector with std::cout << v;
auto operator<<(std::ostream& out, const std::vector<messagedata>& v)
    -> std::ostream&;

auto read_message(messagesize size) -> std::vector<messagedata>;

// encodes a message
auto encode(messagesize size, std::vector<messagedata> a)
    -> std::vector<messagedata>;

auto main() -> int {
    messagesize size = 0;
    std::cin >> size;

    auto message = read_message(size);

    std::cout <<
        //encode(size, message) <<
        message <<
        std::endl;

    return 0;
}

auto operator<<(std::ostream& out, const std::vector<messagedata>& v)
    -> std::ostream&
{
    if(!v.empty()) {
        //out << '[';
        for(auto it = v.begin(); it != v.end(); it++) {
            out <<
                std::setfill('0') <<
                std::setw(8) <<
                std::hex <<
                *it;

            if(it + 1 != v.end()) out << ' ';
        }
        // out << ']';
    }

    return out;
}

auto read_message(messagesize size) -> std::vector<messagedata> {
    std::vector<messagedata> message(size/16);

    for(int i = 0, n = size/16; i < n; i++) {
        std::cin >> std::hex >> message[i];
    }

    return message;
}

auto encode(messagesize size, std::vector<messagedata> a)
    -> std::vector<messagedata>
{
    std::vector<messagedata> b(size/16, 0);

    for(int i = 0; i < size; i++)
        for(int j = 0; j < size; j++)
            b[(i+j)/32] ^= ((a[i/32] >> (i % 32)) &
                            (a[j/32 + size/32] >> (j % 32)) &
                            1) << ((i + j)%32);

    for(auto& el: b)
        std::cout <<
            el;


    return b;
}
