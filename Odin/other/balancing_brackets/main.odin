package main

import "core:fmt"
import "core:container"

main :: proc() {
	tests :: []struct{
		c: string, // case
		e: bool, // is the case expected to be valid?
	} {
		{"()", true},
		{"()[]{}", true},
		{"{()}", true},
		{"[)]", false},
		{"][", false},
	};

	all_tests_passed := true;

	for test in tests {
		balanced := is_balanced(test.c);

		if balanced != test.e {
			all_tests_passed = false;

			fmt.printf(
				"Error: expected '%s' to %sbe balanced\n",
				test.c,
				test.e ? "" : "not ",
			);
		}
	}

	if all_tests_passed {
		fmt.printf("All %d tests passed\n", len(tests));
	}
}

is_balanced :: proc(s: string) -> bool {
	using container;

	Bracket :: enum {
		Paren,
		Square,
		Curly,
	};

	stack := Array(Bracket){};
	array_init(&stack);

	for ch in s {
		is_closing := false;
		closing_type : Bracket = .Paren;

		switch ch {
		case '(':
			array_push_back(&stack, Bracket.Paren);
		case ')':
			is_closing = true;
			closing_type = .Paren;

		case '[':
			array_push_back(&stack, Bracket.Square);
		case ']':
			is_closing = true;
			closing_type = .Square;

		case '{':
			array_push_back(&stack, Bracket.Curly);
		case '}':
			is_closing = true;
			closing_type = .Curly;
		}

		if is_closing && (
			stack.len == 0 ||
			array_pop_back(&stack) != closing_type
		) {
			return false;
		}
	}

	return true;
}
