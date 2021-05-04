package main

import "testing"

func sliceEqual(s1, s2 []uint8) bool {
	result := true

	if len(s1) != len(s2) {
		return false
	}

	for idx, val := range s1 {
		if s2[idx] != val {
			result = false
			break
		}
	}

	return result
}

func TestConvertNum(t *testing.T) {
	nums_str := []string{
		"123",
		"9788362",
		"3",
	}
	convertedNums := [][]uint8{
		{1, 2, 3},
		{9, 7, 8, 8, 3, 6, 2},
		{3},
	}

	for idx, num_str := range nums_str {
		convertedNum := convertNum(num_str)

		if !sliceEqual(convertedNum, convertedNums[idx]) {
			t.Errorf("Failed convertion, expected %v (got %v)\n"+
				"\ttest case: %v (index %d)",
				convertedNums[idx],
				convertedNum,
				nums_str[idx],
				idx)
		}
	}
}

func TestSliceProd(t *testing.T) {
	slices := [][]uint8{
		{1},
		{1, 2, 3},
		{1, 2, 3, 4, 5},
	}
	prods := []uint64{
		1,
		6,
		120,
	}

	for idx, s := range slices {
		p := sliceProd(s)
		if p != prods[idx] {
			t.Errorf("Failed productory, expected %d (got %d)\n"+
				"\ttest case: %v (index %d)",
				prods[idx],
				p,
				slices[idx],
				idx)
		}
	}
}

func TestAdjacentProd(t *testing.T) {
	programResult := adjecentProd(convertNum(readNum("./num.txt")), 4)
	exampleResult := uint64(5832)

	if programResult != exampleResult {
		t.Errorf("Result does not agree with provided example (got %d, while "+
			"expecting %d)",
			programResult,
			exampleResult)
	}
}
