#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

/*
The while loop represents the game.

Each iteration represents a turn of the game where you are given inputs (the
heights of the mountains) and where you have to print an output (the index of
the moutain to fire on)

The inputs you are given are automatically updated according to your last
actions.
*/
int main() {
    vector<int> mountains;

    // game loop
    while (true) {
        for (int i = 0; i < 8; i++) {
            int mountainH; // represents the height of one mountain.
            cin >> mountainH; cin.ignore();
            mountains.push_back(mountainH);
        }

        // Write an action using cout. DON'T FORGET THE "<< endl"
        // To debug: cerr << "Debug messages..." << endl;

        auto max_h = max_element(mountains.begin(), mountains.end());
        auto target = distance(mountains.begin(), max_h);
        cout << target << endl; // The index of the mountain to fire on.

        mountains.clear();
    }
}
