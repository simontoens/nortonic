"""
These tests are Golang specific.
"""

import compilertest
import unittest


class GoTest(compilertest.CompilerTest):

    def test_if_expr_to_if_stmt(self):
        py = """
def to_binary(i):
    return 0 if i == 0 else 1
j = 0 if True else 1
to_binary(0 if j == 1 else 1)
"""

        self.go(py, """
func to_binary(i int) int {
    var t int
    if i == 0 {
        t = 0
    } else {
        t = 1
    }
    return t
}
var j int
if true {
    j = 0
} else {
    j = 1
}
var t1 int
if j == 1 {
    t1 = 0
} else {
    t1 = 1
}
to_binary(t1)
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
import (
    "fmt"
)

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

    def test_add_pointer_to_list_of_pointers(self):
        py = """
def add_str_to_list(s):
    l = []
    l.append(s)
add_str_to_list("foo")
"""

        self.go(py, """
func add_str_to_list(s *string) {
    l := []*string{}
    l = append(l, s)
}
t := "foo"
add_str_to_list(&t)
""")



if __name__ == '__main__':
    unittest.main()
