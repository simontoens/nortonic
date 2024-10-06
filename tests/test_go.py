"""
These tests are Golang specific.
"""

import compilertest
import unittest


class GoTest(compilertest.CompilerTest):

    def test_if_expr_to_if_stmt(self):
        """
        If expression to if stmt translation is not implemented well.
        Some usages work though.
        """
        py = """
def to_binary(i):
    return 0 if i == 0 else 1
j = 0 if True else 1
to_binary(0 if j == 1 else 1)
"""

        self.go(py, """
func to_binary(i int) int {
    if i == 0 {
        return 0
    } else {
        return 1
    }
}
var j int
if true {
    j = 0
} else {
    j = 1
}
if j == 1 {
    to_binary(0)
} else {
    to_binary(1)
}
""")

    def test_call_function_with_pointer_and_none(self):
        py = """
def greet(name):
    if name is None: # ok because name is a pointer
        print("Hello, stranger!")
    else:
        print("hello", name)

def main2(name):
    greet(name) # name is a pointer, so it can be passed directly into greet
    greet(None) # None can also be passed directly, no address of

greet("Bernd")  # needs address of
main2("Simon")  # needs address of
"""

        self.go(py, """
func greet(name *string) {
    if name == nil {
        fmt.Println("Hello, stranger!")
    } else {
        fmt.Println("hello", *name)
    }
}
func main2(name *string) {
    greet(name)
    greet(nil)
}
t := "Bernd"
greet(&t)
t1 := "Simon"
main2(&t1)
""")


if __name__ == '__main__':
    unittest.main()
