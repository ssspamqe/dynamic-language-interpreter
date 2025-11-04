package ru.innopolis.interpreter

/*
The next stage of your project is Semantic Analysis (Analyzer).
Semantic analysis is an essential step where you check for semantic correctness in your program and perform possible optimizations. This means making sure your program behaves logically and consistently according to the rules of the language.

You will present your semantic analyzer on next Thursday, November 6

Important Requirement
For this stage, we expect at least two checks that do not modify the AST and at least two optimizations that do modify the AST. You can choose from the examples below or come up with your own, but make sure to meet this minimum requirement. Also, try to implement as many as possible.

Semantic Analysis Checks
Here are some typical checks you might want to implement (note: these checks will not modify the AST):
1. Correct Keyword Usage: Ensure that keywords are used in the right context. For instance, the break keyword should only appear inside loops, and return should only be used inside functions.
2. Declarations Before Usage: Make sure that all classes, functions, and variables are declared (and possibly initialized) before they are used. For example, if you're calling a function, ensure that the function actually exists.
3. Type Checking: Implement partial type checking to ensure, for example, that return values match the declared function types and that the types align when assigning variables or passing function arguments.
4. Array Bound Checking (where possible): Ensure that accessing arrays (e.g., arr[i]) respects the array’s bounds, i.e., 0 <= i < length(arr).
5. Other Checks: Feel free to explore additional checks that fit your language's semantics.

Possible Optimizations
Here are some common optimizations you might want to implement. These optimizations modify the AST:
1. Constant Expression Simplification: Simplify constant expressions during compilation. For example, a = 5 + 3 can be reduced to a = 8, and expressions like 3 < 5 can be replaced with True.
2. Removing Unused Variables: If variables are declared but never used in the program, they can be safely removed to reduce code clutter.
3. Function Inlining: Replace function calls with the function body itself to reduce the overhead of function calls.
Example: Before inlining:
func foo(a : Integer) {
  print("Hello")
  print(a)
}

func main() {
  a : Integer = 1
  foo(a)

  foo(2)
}
After inlining:
func main() {
  a : Integer = 1
  print("Hello") // inlined foo(a)
  print(a)

  print("Hello") // inlined foo(2)
  print(2)
}
4. Code simplification: Simplify conditional structures where possible.
Example: Before:
if (True) {
  print("Hello")
  print("There")
}
else {
  print("Otherwise")
}
After simplification:
print("Hello") // if branch is always true. Completely remove else branch and replace the whole if structure with its body.
print("There")
5. Removing Unreachable Code: Remove any code that will never be executed. For instance, code after a return statement:
func foo() {
  return 0
  print("Hello") // this is unreachable code and can be removed at compile time
}
6. Additional Optimizations: You can implement other optimizations based on your analysis and creativity!

Presentation Format
The format of the presentation is the same as previous ones. You’ll need to upload your presentations to Moodle and demonstrate your work during the lab session. Each team will have 10 minutes for the presentation.
 */

class SemanticAnalyzer {

}
