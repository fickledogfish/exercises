package main

import (
	"fmt"
	"testing"
)

func TestIsBalanced(t *testing.T) {
	type testT struct {
		c string
		e bool
	}

	for _, c := range []testT{
		{"()", true},
		{"()[]{}", true},
		{"{()}", true},
		{"[)]", false},
		{"][", false},
	} {
		balanced := isBalanced(c.c)
		if balanced != c.e {
			t.Error(fmt.Errorf(
				"invalid case: '%s', expected %v but got %v",
				c.c, c.e, balanced,
			))
		}
	}
}
