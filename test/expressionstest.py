from run import *
import unittest


class ExpresionsTest(unittest.TestCase):

    def test_expr1(self):
        self._test(code="1 + 1", expected="1+1", result=2)

    def test_expr2(self):
        self._test(code="1+1*2", expected="1+1*2", result=3)

    def test_expr3(self):
        self._test(code="(1+1)*2", expected="(1+1)*2", result=4)

    def test_expr4(self):
        self._test(code="1+(1*2)", expected="1+1*2", result=3)

    def test_expr5(self):
        self._test(code="(1+2)*(3+4)", expected="(1+2)*(3+4)", result=21)

    def test_expr6(self):
        self._test(code="(1+2)*((3+4)*(10+3))",
                   expected="(1+2)*(3+4)*(10+3)",
                   result=273)

    def test_expr7(self):
        self._test(code="(1+1*(2+3*4))*2",
                   expected="(1+1*(2+3*4))*2",
                   result=30)

    def _test(self, code, expected, result):
        syntax = PythonSyntax()  
        generated_code = run(code, syntax)

        self.assertEqual(result, eval(code))
        self.assertEqual(expected, generated_code)


if __name__ == '__main__':
    unittest.main()
