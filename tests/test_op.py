from tests import compilertest
import unittest


class OpTest(compilertest.CompilerTest):

    def test_equality(self):
        py = '"foo" == "blah"'
        self.py(py, expected=py)
        self.java(py, expected='"foo".equals("blah");')
        self.elisp(py, expected='(equal "foo" "blah")')

    def test_identity(self):
        py = '"foo" is "blah"'
        self.py(py, expected=py)
        self.java(py, expected='"foo" == "blah";')
        self.elisp(py, expected='(eq "foo" "blah")')

    def test_unary(self):
        py = "-1"
        self.py(py, expected="-1")
        self.java(py, expected="-1;")
        self.elisp(py, expected="-1")

    def test_expr1(self):
        py = "1+1"
        self.py(py, expected="1 + 1")
        self.java(py, expected="1 + 1;")
        self.elisp(py, expected="(+ 1 1)")

    def test_expr2(self):
        py = "1+1*2"
        self.py(py, expected="1 + 1 * 2")
        self.java(py, expected="1 + 1 * 2;")
        self.elisp(py, expected="(+ 1 (* 1 2))")

    def test_expr3(self):
        py = "(1+1)*2"
        self.py(py, expected="(1 + 1) * 2")
        self.java(py, expected="(1 + 1) * 2;")
        self.elisp(py, expected="(* (+ 1 1) 2)")

    def test_expr4(self):
        py = "1+(1*2)"
        self.py(py, expected="1 + 1 * 2")
        self.java(py, expected="1 + 1 * 2;")
        self.elisp(py, expected="(+ 1 (* 1 2))")

    def test_expr5(self):
        py = "(1+2)*(3+4)"
        self.py(py, expected="(1 + 2) * (3 + 4)")
        self.java(py, expected="(1 + 2) * (3 + 4);")
        self.elisp(py, expected="(* (+ 1 2) (+ 3 4))")

    def test_expr6(self):
        py = "(1+2)*((3+4)*(10+3))"
        self.py(py, expected="(1 + 2) * (3 + 4) * (10 + 3)")
        self.java(py, expected="(1 + 2) * (3 + 4) * (10 + 3);")
        self.elisp(py, expected="(* (+ 1 2) (* (+ 3 4) (+ 10 3)))")

    def test_expr7(self):
        py = "(1+1*(2+3*4))*2"
        self.py(py, expected="(1 + 1 * (2 + 3 * 4)) * 2")
        self.java(py, expected="(1 + 1 * (2 + 3 * 4)) * 2;")
        self.elisp(py, expected="(* (+ 1 (* 1 (+ 2 (* 3 4)))) 2)")

    def test_expr8(self):
        py = "15 + 20 / 2"
        self.py(py, expected="15 + 20 / 2")
        self.java(py, expected="15 + 20 / 2;")
        self.elisp(py, expected="(+ 15 (/ 20 2))")

    def test_expr9(self):
        py = "10 * 10 / 2 + 1"
        self.py(py, expected="10 * 10 / 2 + 1")
        self.java(py, expected="10 * 10 / 2 + 1;")
        self.elisp(py, expected="(+ (/ (* 10 10) 2) 1)")

    def test_expr10(self):
        py = "10 * (10 / 2 + 1)"
        self.py(py, expected="10 * (10 / 2 + 1)")
        self.java(py, expected="10 * (10 / 2 + 1);")
        self.elisp(py, expected="(* 10 (+ (/ 10 2) 1))")

    def test_expr11(self):
        py = "10 - 2 * (10 - 2 / 2 + 1) - 5"
        self.py(py, expected="10 - 2 * (10 - 2 / 2 + 1) - 5")
        self.java(py, expected="10 - 2 * (10 - 2 / 2 + 1) - 5;")
        self.elisp(py, expected="(- (- 10 (* 2 (+ (- 10 (/ 2 2)) 1))) 5)")

    def test_bool(self):
        py = "(True or True) and False"
        self.py(py, expected=py)
        self.java(py, expected="(true || true) && false;")
        self.elisp(py, expected="(and (or t t) nil)")

    def test_mod(self):
        py = "10 % 2"
        self.py(py, expected=py)
        self.java(py, expected="10 % 2;")
        self.elisp(py, expected="(mod 10 2)")

    def test_mod__str1(self):
        py = '"Hello %s" % "Kaito"'
        self.py(py, expected=py)
        self.java(py, expected='String.format("Hello %s", "Kaito");')
        self.elisp(py, expected='(format "Hello %s" "Kaito")')

    def test_mod__str2(self):
        py = '"Hello %s %s" % ("World", "Kaito")'
        # we transfor tupe -> list if all types are equal, but we shouldn't
        # do it for this special case
        self.py(py, expected='"Hello %s %s" % ["World", "Kaito"]')
        self.java(py, expected='String.format("Hello %s %s", "World", "Kaito");')
        self.elisp(py, expected='(format "Hello %s %s" "World" "Kaito")')

if __name__ == '__main__':
    unittest.main()
