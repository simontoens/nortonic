"""
These tests are Golang specific.
"""

from tests import compilertest
import unittest


class GoTest(compilertest.CompilerTest):

    def test_discarded_identifier(self):
        py = """
# In, Go, when _ is by itself on the lhs, := cannot be used
a, _, _, d = "this is a call".split(" ")
"""
        self.go(py, """
t := strings.Split("this is a call", " ")
a := t[0]
_ = t[1]
_ = t[2]
d := t[3]
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
