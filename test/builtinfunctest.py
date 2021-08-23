from run import *
import syntax as syntaxm
import unittest


class BuiltInFuncTest(unittest.TestCase):

    def test_print(self):
        py = "print(1)"
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, "System.out.println(1);", syntaxm.JavaSyntax())

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected, generated_code)


if __name__ == '__main__':
    unittest.main()
