/*
 * Given a string containing only '(', ')', '[', ']', '{' or '}', check if the
 * brackets are balanced. For example, the string "(])" isn't balanced, while
 * "()[]" is.
 */

#include <iostream>
#include <stack>
#include <string_view>
#include <vector>

bool is_balanced(const std::string_view s) {
	enum class StackData {
		open_paren,
		open_square,
		open_curly,
	};

	std::stack<StackData> stack;

	for (auto ch : s) {
		switch (ch) {
		case '(':
			stack.push(StackData::open_paren);
			break;
		case ')':
			if (stack.empty() || stack.top() != StackData::open_paren) {
				return false;
			}
			stack.pop();
			break;

		case '[':
			stack.push(StackData::open_square);
			break;
		case ']':
			if (stack.empty() || stack.top() != StackData::open_square) {
				return false;
			}
			stack.pop();
			break;

		case '{':
			stack.push(StackData::open_curly);
			break;
		case '}':
			if (stack.empty() || stack.top() != StackData::open_curly) {
				return false;
			}
			stack.pop();
			break;

		default:
			break;
		}
	}

	return stack.empty();
}

int main() {
	struct testT {
		std::string c; // case
		bool e; // is it expected to be valid?
	};

	const std::vector<testT> tests = {
		{"()", true},
		{"()[]{}", true},
		{"{()}", true},
		{"[)]", false},
		{"][", false},
	};

	for (auto test : tests) {
		const auto balanced = is_balanced(test.c);

		if (balanced != test.e) {
			std::cout << "Error: expected '"
				<< test.c
				<< "' to "
				<< (test.e ? "" : "not ")
				<< "be balanced"
				<< std::endl;

			return -1;
		}
	}

	std::cout << "All tests passed" << std::endl;
}
