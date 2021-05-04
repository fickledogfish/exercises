// https://www.codingame.com/ide/puzzle/ascii-art

/*
Input:

    - L: width of each letter in the style
    - H: height of each letter in the style
    - T: text to be converted into ASCII
    - style: the string "ABCDEFGHIJKLMNOPQRSTUVWXYZ?" in ASCII art
*/

let STYLE_STRING = "ABCDEFGHIJKLMNOPQRSTUVWXYZ?";

let L = parseInt(readline());
let H = parseInt(readline());
let T = readline().toUpperCase();

let styleRaw = [];

// Read the style string
for (let i = 0; i < H; i++) styleRaw.push(readline());

// Transform the style into a list of characters
let style = [];

for(let charIdx = 0; charIdx < STYLE_STRING.length; charIdx++) {
    style[STYLE_STRING[charIdx]] = [];

    for(let row of styleRaw) {
        let charRow = row.substring(charIdx*L, charIdx*L + L);
        style[STYLE_STRING[charIdx]].push(charRow);
    }
}

// Debug information

printErr("text to be converted: \"" + T + "\"");
printErr("keys in the style dict: " + Object.keys(style));
printErr("matching lengths? " +
         (Object.keys(style).length === STYLE_STRING.length));
for(let row of style.Z) printErr(row); printErr("\n");

// Print the answer
for(let row = 0; row < H; row++) {
    let currRow = "";
    for(let c of T) {
        if(!style[c])
            currRow += style["?"][row];
        else
            currRow += style[c][row];
    }
    print(currRow);
}
