from run import run
import syntax as syntaxm
import unittest


class FuncTest(unittest.TestCase):

    def test_def(self):
        py = """
def foo(a):
    print("hello")
"""
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected=py)

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code.strip())


if __name__ == '__main__':
    unittest.main()
