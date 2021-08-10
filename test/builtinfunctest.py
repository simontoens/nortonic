from run import *
import unittest


class BuiltInFuncTest(unittest.TestCase):

    def test_print(self):
        self._test(code="print(1)", expected="print(1)")

    def _test(self, code, expected):
        generated_code = run(code, PythonSyntax())

        self.assertEqual(expected, generated_code)


if __name__ == '__main__':
    unittest.main()
