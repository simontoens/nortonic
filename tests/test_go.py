"""
These tests are Golang specific.
"""

import compilertest
import unittest


class GoTest(compilertest.CompilerTest):

    def setUp(self):
        self.maxDiff = None

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
def get_greeting(name):
    if name is None: # ok because name is a pointer
        return "Hello stranger"
    else:
        return "Hello " + name

def main2(name):
    get_greeting(name) # name is a ptr, so it can be passed directly
    get_greeting(None) # None can also be passed directly, no address of

msg = get_greeting("Bernd")  # needs address of
print("Yo!", msg)
main2("Simon")  # needs address of
"""

        self.go(py, """
func get_greeting(name *string) *string {
    if name == nil {
        t := "Hello stranger"
        return &t
    } else {
        t1 := "Hello " + *name
        return &t1
    }
}
func main2(name *string) {
    get_greeting(name)
    get_greeting(nil)
}
t2 := "Bernd"
msg := get_greeting(&t2)
fmt.Println("Yo!", *msg)
t3 := "Simon"
main2(&t3)
""")


if __name__ == '__main__':
    unittest.main()
