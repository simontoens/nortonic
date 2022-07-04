from run import *
import syntax as sy
import unittest


class ExpresionsTest(unittest.TestCase):

    def test_expr1(self):
        py = "1+1"
        self._test(code=py, expected="1 + 1",
                   syntax=sy.PythonSyntax(), result=2)
        self._test(code=py, expected="1 + 1;",
                   syntax=sy.JavaSyntax(), result=2)
        self._test(code=py, expected="(+ 1 1)",
                   syntax=sy.ElispSyntax(), result=2)

    def test_expr2(self):
        py = "1+1*2"
        self._test(code=py, expected="1 + 1 * 2",
                   syntax=sy.PythonSyntax(), result=3)
        self._test(code=py, expected="1 + 1 * 2;",
                   syntax=sy.JavaSyntax(), result=3)
        self._test(code=py, expected="(+ 1 (* 1 2))",
                   syntax=sy.ElispSyntax(), result=3)

    def test_expr3(self):
        py = "(1+1)*2"
        self._test(code=py, expected="(1 + 1) * 2",
                   syntax=sy.PythonSyntax(), result=4)
        self._test(code=py, expected="(1 + 1) * 2;",
                   syntax=sy.JavaSyntax(), result=4)
        self._test(code=py, expected="(* (+ 1 1) 2)",
                   syntax=sy.ElispSyntax(), result=4)

    def test_expr4(self):
        py = "1+(1*2)"
        self._test(code=py, expected="1 + 1 * 2",
                   syntax=sy.PythonSyntax(), result=3)
        self._test(code=py, expected="1 + 1 * 2;",
                   syntax=sy.JavaSyntax(), result=3)
        self._test(code=py, expected="(+ 1 (* 1 2))",
                   syntax=sy.ElispSyntax(), result=3)

    def test_expr5(self):
        py = "(1+2)*(3+4)"
        self._test(code=py, expected="(1 + 2) * (3 + 4)",
                   syntax=sy.PythonSyntax(), result=21)
        self._test(code=py, expected="(1 + 2) * (3 + 4);",
                   syntax=sy.JavaSyntax(), result=21)
        self._test(code=py, expected="(* (+ 1 2) (+ 3 4))",
                   syntax=sy.ElispSyntax(), result=21)

    def test_expr6(self):
        py = "(1+2)*((3+4)*(10+3))"
        self._test(code=py, expected="(1 + 2) * (3 + 4) * (10 + 3)",
                   syntax=sy.PythonSyntax(), result=273)
        self._test(code=py, expected="(1 + 2) * (3 + 4) * (10 + 3);",
                   syntax=sy.JavaSyntax(), result=273)
        self._test(code=py, expected="(* (+ 1 2) (* (+ 3 4) (+ 10 3)))",
                   syntax=sy.ElispSyntax(), result=273)

    def test_expr7(self):
        py = "(1+1*(2+3*4))*2"
        self._test(code=py, expected="(1 + 1 * (2 + 3 * 4)) * 2",
                   syntax=sy.PythonSyntax(), result=30)
        self._test(code=py, expected="(1 + 1 * (2 + 3 * 4)) * 2;",
                   syntax=sy.JavaSyntax(), result=30)
        self._test(code=py, expected="(* (+ 1 (* 1 (+ 2 (* 3 4)))) 2)",
                   syntax=sy.ElispSyntax(), result=30)

    def test_expr8(self):
        py = "15 + 20 / 2"
        self._test(code=py, expected="15 + 20 / 2",
                   syntax=sy.PythonSyntax(), result=25)
        self._test(code=py, expected="15 + 20 / 2;",
                   syntax=sy.JavaSyntax(), result=25)
        self._test(code=py, expected="(+ 15 (/ 20 2))",
                   syntax=sy.ElispSyntax(), result=25)

    def test_expr9(self):
        py = "10 * 10 / 2 + 1"
        self._test(code=py, expected="10 * 10 / 2 + 1",
                   syntax=sy.PythonSyntax(), result=51)
        self._test(code=py, expected="10 * 10 / 2 + 1;",
                   syntax=sy.JavaSyntax(), result=51)
        self._test(code=py, expected="(+ (/ (* 10 10) 2) 1)",
                   syntax=sy.ElispSyntax(), result=51)

    def test_expr10(self):
        py = "10 * (10 / 2 + 1)"
        self._test(code=py, expected="10 * (10 / 2 + 1)",
                   syntax=sy.PythonSyntax(), result=60)
        self._test(code=py, expected="10 * (10 / 2 + 1);",
                   syntax=sy.JavaSyntax(), result=60)
        self._test(code=py, expected="(* 10 (+ (/ 10 2) 1))",
                   syntax=sy.ElispSyntax(), result=60)

    def _test(self, code, expected, syntax, result):
        generated_code = run(code, syntax)

        self.assertEqual(result, eval(code))
        self.assertEqual(expected, generated_code)


if __name__ == '__main__':
    unittest.main()
