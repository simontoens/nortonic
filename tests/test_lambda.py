from tests import compilertest
import unittest


class DictLambda(compilertest.CompilerTest):

    def test_lambda_declaration(self):
        py = """
l = lambda: 1
"""
        self.py(py, expected=py)
        self.go(py, expected="""
l := func() int {
    1
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
    1
}
""")
        

if __name__ == '__main__':
    unittest.main()
