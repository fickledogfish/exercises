package main

import "fmt"

// Since Go doesn't have generics (yet), the stack needs to know what data
// it'll actually hold. For now, let's call it an int.
type StackData int

// A simple implementation of a stack to keep track of what kinds of brackets
// were previously opened.
type Stack struct {
	data []StackData
}

func NewStack() Stack {
	return Stack{
		data: []StackData{},
	}
}

func (s Stack) Len() int {
	return len(s.data)
}

func (s *Stack) Push(item StackData) {
	s.data = append(s.data, item)
}

func (s *Stack) Pop() (error, StackData) {
	last := len(s.data) - 1
	if last < 0 {
		return fmt.Errorf("No element to retrieve in the stack"), 0
	}

	lastEl := s.data[last]

	s.data = s.data[:last] // pop

	return nil, lastEl
}
