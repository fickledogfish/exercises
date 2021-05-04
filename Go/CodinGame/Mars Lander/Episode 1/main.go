package main

import (
	"fmt"
	"os"
)

func main() {
	// We can ignore this input, be we still need to read the data
	_, _ = readInitializationData()

	// 2 integers: rotate power.
	//
	// rotate is the desired rotation angle (should be 0 for level 1)
	//
	// power is the desired thrust power (0 to 4).
	desiredRotation := 0 // always zero
	desiredPower := NewNumber(0)

	for {
		landerCoords, speed, fuel, rotation, power := readGameData()
		fmt.Fprintln(os.Stderr, landerCoords, speed, fuel, rotation, power)

		// manipulate power and rotation as needed
		if speed.Y < -40 {
			desiredPower.Inc()
		} else if speed.Y > 0 {
			desiredPower.Dec()
		}

		fmt.Println(desiredRotation, desiredPower.Val())
	}
}

type Point struct {
	X, Y int
}

func readInitializationData() (numSurfacePoints int, groundCoords []Point) {
	_, err := fmt.Scan(&numSurfacePoints)
	if err != nil {
		panic(err)
	}

	groundCoords = make([]Point, numSurfacePoints)

	for i := 0; i < numSurfacePoints; i++ {
		_, err := fmt.Scan(&groundCoords[i].X, &groundCoords[i].Y)
		if err != nil {
			panic(err)
		}
	}

	return
}

func readGameData() (landerCoords, speed Point, fuel, rotation, power int) {
	fmt.Scan(
		&landerCoords.X, &landerCoords.Y,
		&speed.X, &speed.Y,
		&fuel,
		&rotation,
		&power,
	)

	return
}

type Number struct {
	val int
}

func NewNumber(val int) Number {
	return Number{
		val: val,
	}
}

func (n *Number) Val() int {
	return n.val
}

func (n *Number) Inc() {
	if n.val < 4 {
		n.val++
	}
}

func (n *Number) Dec() {
	if n.val > 0 {
		n.val--
	}
}
