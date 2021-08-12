from run import *
import syntax as syntaxm
import unittest


class BuiltInFuncTest(unittest.TestCase):

    def test_print(self):
        self._test("print(1)", "print(1)", syntaxm.PythonSyntax())
        self._test("print(1)", "System.out.println(1);", syntaxm.JavaSyntax())

    def _test(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected, generated_code)


if __name__ == '__main__':
    unittest.main()
