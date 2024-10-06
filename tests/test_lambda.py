import compilertest
import unittest


class LambdaTest(compilertest.CompilerTest):

    def test_lambda_declaration(self):
        py = """
l = lambda: 1
"""
        self.py(py, expected=py)
        self.go(py, expected="""
l := func() int {
    return 1
}
""")

    def test_none_then_lambda_declaration(self):
        py = """
l = None
l = lambda: 1
"""
        self.py(py, expected=py)
        self.go(py, expected="""
var l func() int
l = func() int {
    return 1
}
""")

    def test_lambda_with_arg(self):
        py = """
l = None
l = lambda x: x + 1
l(1)
"""
        self.py(py, expected=py)
        self.go(py, expected="""
var l func(int) int
l = func(x int) int {
    return x + 1
}
l(1)
""")

    def test_lambda_as_func_arg(self):
        py = """
def foo(f, i):
    return f(i)
l = lambda op: op + 1
foo(l, 22)
"""
        self.py(py, expected=py)
        self.go(py, expected="""
func foo(f func(int) int, i int) int {
    return f(i)
}
l := func(op int) int {
    return op + 1
}
foo(l, 22)
""")

    def test_lambda_with_if_expr(self):
        py = """
f = lambda: True if 1 == 1 else False
"""
        self.py(py, expected=py)
        self.go(py, expected="""
f := func() bool {
    if 1 == 1 {
        return true
    } else {
        return false
    }
}
""")
        

if __name__ == '__main__':
    unittest.main()
