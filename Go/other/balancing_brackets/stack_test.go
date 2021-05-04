package main

import (
	"fmt"
	"testing"
)

func TestNewStack(t *testing.T) {
	s := NewStack()

	if s.Len() != 0 {
		t.Error(fmt.Errorf(
			"Expected a freshly created stack to have length zero, but it "+
				"has %d",
			s.Len(),
		))
	}
}

func TestLenAndPush(t *testing.T) {
	s := NewStack()

	for i := 1; i < 100; i++ {
		s.Push(StackData(i))
		l := s.Len()

		if l != i {
			t.Error(fmt.Errorf(
				"Expected the stack to have length %d, but it has %d",
				i,
				s.Len(),
			))

			return
		}

		if s.data[l-1] != StackData(i) {
			t.Error(fmt.Errorf(
				"stack[%d] = %d, but it should be %d",
				l-1,
				i,
				s.data[l-1],
			))

			return
		}
	}
}

func TestPop(t *testing.T) {
	s := NewStack()

	for i := 0; i < 100; i++ {
		s.Push(StackData(i))
	}

	for i := 99; i >= 0; i-- {
		err, el := s.Pop()

		if err != nil {
			t.Error(err)
			return
		}

		if el != StackData(i) {
			t.Error(fmt.Errorf(
				"stack[%d] = %d, but expected it to be %d",
				i,
				el,
				i,
			))

			return
		}
	}
}
