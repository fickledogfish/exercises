package main

const (
	openParen StackData = iota
	openSquare
	openCurly
)

func isBalanced(s string) bool {
	stack := NewStack()

	for _, c := range s {
		switch c {
		case '(':
			stack.Push(openParen)
		case ')':
			if err, el := stack.Pop(); err != nil || el != openParen {
				return false
			}

		case '[':
			stack.Push(openSquare)
		case ']':
			if err, el := stack.Pop(); err != nil || el != openSquare {
				return false
			}

		case '{':
			stack.Push(openCurly)
		case '}':
			if err, el := stack.Pop(); err != nil || el != openCurly {
				return false
			}
		}
	}

	if stack.Len() != 0 {
		return false
	}

	return true
}
