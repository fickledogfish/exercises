/*
# The Goal

The game is played on a rectangular grid with a given size. Some cells contain
power nodes. The rest of the cells are empty.

The goal is to find, when they exist, the horizontal and vertical neighbors of
each node.

# Rules

To do this, you must find each (x1,y1) coordinates containing a node, and
display the (x2,y2) coordinates of the next node to the right, and the (x3,y3)
coordinates of the next node to the bottom within the grid.

If a neighbor does not exist, you must output the coordinates -1 -1 instead of
(x2,y2) and/or (x3,y3).

You lose if: You give an incorrect neighbor for a node. You give the neighbors
for an empty cell. You compute the same node twice. You forget to compute the
neighbors of a node.
*/

package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	grid, w, h := readGrid()
	neighborLst := findNeighbors(w, h, grid)

	fmt.Fprintln(os.Stderr, w, h, grid)
	fmt.Fprintln(os.Stderr, neighborLst)

	for _, cells := range neighborLst {
		for idx, cell := range cells {
			if idx != 0 {
				fmt.Printf(" ")
			}

			fmt.Printf("%d %d", cell.X, cell.Y)
		}
		fmt.Println("")
	}
}

type Cell uint8
type Grid [][]Cell

const (
	Nothing Cell = iota
	Something
)

func readGrid() (grid Grid, width, height int) {
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Buffer(make([]byte, 1000000), 1000000)

	// width: the number of cells on the X axis
	scanner.Scan()
	fmt.Sscan(scanner.Text(), &width)

	// height: the number of cells on the Y axis
	scanner.Scan()
	fmt.Sscan(scanner.Text(), &height)

	grid = make([][]Cell, height)

	for i := 0; i < height; i++ {
		scanner.Scan()
		line := scanner.Text() // width characters, each either 0 or .

		for _, c := range line {
			cell := Nothing

			if c == '0' {
				cell = Something
			}

			grid[i] = append(grid[i], cell)
		}
	}

	return
}

type Node struct {
	X, Y int
}

func CreateNode(x, y int) Node {
	return Node{x, y}
}

func (n *Node) ChangeNode(newX, newY int) {
	n.X = newX
	n.Y = newY
}

func findNeighbors(w, h int, grid Grid) (neighborLst [][3]Node) {
	for row := 0; row < h; row++ {
		for col := 0; col < w; col++ {
			if grid[row][col] == Nothing {
				continue
			}

			this := CreateNode(col, row)

			right := CreateNode(-1, -1)
			for currCol := col + 1; currCol < w; currCol++ {
				if grid[row][currCol] == Something {
					right.ChangeNode(currCol, row)
					break
				}
			}

			down := CreateNode(-1, -1)
			for currRow := row + 1; currRow < h; currRow++ {
				if grid[currRow][col] == Something {
					down.ChangeNode(col, currRow)
					break
				}
			}

			neighborLst = append(neighborLst, [3]Node{this, right, down})
		}
	}

	return
}
