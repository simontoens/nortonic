from run import *
import ast as astm
import syntax
import unittest


class AssignmentTest(unittest.TestCase):

    def test_ass1(self):
        self._test(code="a = 1", expected="a=1")

    def test_ass2(self):
        self._test(code="a = 'hello' ;print(a)", 
                   expected="""
a="hello"
print(a)
""")

    def test_ass3(self):
        self._test(code="a = 1+2 ;print(a*3)", 
                   expected="""
a=1+2
print(a*3)
""")

    def _test(self, code, expected):
        generated_code = run(code, syntax.PythonSyntax())

        self.assertEqual(expected.strip(), generated_code)


if __name__ == '__main__':
    unittest.main()
