from tests import compilertest
import unittest


class OpTest(compilertest.CompilerTest):

    def test_is_equals__str(self):
        py = '"foo" == "blah"'
        self.py(py, expected=py)
        self.java(py, expected='"foo".equals("blah");')
        self.elisp(py, expected='(equal "foo" "blah")')
        self.go(py, expected='"foo" == "blah"')

    def test_is_equals__int(self):
        py = "1 == 2"
        self.py(py, expected=py)
        self.java(py, expected="1 == 2;")
        self.elisp(py, expected="(equal 1 2)")
        self.go(py, expected=" 1 == 2")

    def test_is_not_equals__str(self):
        py = '"foo" != "blah"'
        self.py(py, expected=py)
        self.java(py, expected='!"foo".equals("blah");')
        self.elisp(py, expected='(not (equal "foo" "blah"))')
        self.go(py, expected='"foo" != "blah"')

    def test_is_not_equals__int(self):
        py = "1 != 2"
        self.py(py, expected=py)
        self.java(py, expected=" 1 != 2;")
        self.elisp(py, expected="(not (equal 1 2))")
        self.go(py, expected="1 != 2")

    def test_is_same(self):
        py = '"foo" is "blah"'
        self.py(py, expected=py)
        self.java(py, expected='"foo" == "blah";')
        self.elisp(py, expected='(eq "foo" "blah")')
        self.go(py, expected='"foo" == "blah"') # need to check pointers ...

    def test_is_not_same(self):
        py = '"foo" is not "blah"'
        self.py(py, expected=py)
        self.java(py, expected='"foo" != "blah";')
        self.elisp(py, expected='(not (eq "foo" "blah"))')
        self.go(py, expected='"foo" != "blah"') # need to check pointers ...

    def test_less_than(self):
        py = '1 < 3'
        self.py(py, expected=py)
        self.java(py, expected='1 < 3;')
        self.elisp(py, expected='(< 1 3)')
        self.go(py, expected='1 < 3')

    def test_unary__not(self):
        py = "not True"
        self.py(py, expected="not True")
        self.java(py, expected="!true;")
        self.elisp(py, expected="(not t)")
        self.go(py, expected="!true")

    def test_unary__neg(self):
        py = "-1"
        self.py(py, expected="-1")
        self.java(py, expected="-1;")
        self.elisp(py, expected="-1")
        self.go(py, expected="-1")

    def test_unary__func_neg(self):
        py = """
def foo():
    return -1
i = -foo()
"""
        self.py(py, py)
        self.java(py, expected="""
static Integer foo() {
    return -1;
}
static Integer i = -foo();
""")
        self.elisp(py, expected="""
(defun foo ()
    -1)
(setq i (- (foo)))
""")
        self.go(py, expected="""
func foo() int {
    return -1
}
i := -foo()
""")

    def test_expr1(self):
        py = "1+1"
        self.py(py, expected="1 + 1")
        self.java(py, expected="1 + 1;")
        self.elisp(py, expected="(+ 1 1)")
        self.go(py, expected="1 + 1")

    def test_expr2(self):
        py = "1+1*2"
        self.py(py, expected="1 + 1 * 2")
        self.java(py, expected="1 + 1 * 2;")
        self.elisp(py, expected="(+ 1 (* 1 2))")
        # mostly skipping Golang, it is handled in the same way as Python/Java

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
        self.go(py, expected="(1 + 1 * (2 + 3 * 4)) * 2")

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
        self.go(py, expected="10 - 2 * (10 - 2 / 2 + 1) - 5")        

    def test_bool(self):
        py = "(True or True) and False"
        self.py(py, expected=py)
        self.java(py, expected="(true || true) && false;")
        self.elisp(py, expected="(and (or t t) nil)")
        self.go(py, expected="(true || true) && false")

    def test_mod(self):
        py = "10 % 2"
        self.py(py, expected=py)
        self.java(py, expected="10 % 2;")
        self.elisp(py, expected="(mod 10 2)")
        self.go(py, expected="10 % 2")

    def test_mod__str1(self):
        py = '"Hello %s" % "Kaito"'
        self.py(py, expected=py)
        self.java(py, expected='String.format("Hello %s", "Kaito");')
        self.elisp(py, expected='(format "Hello %s" "Kaito")')
        self.go(py, expected='fmt.Sprintf("Hello %s", "Kaito")')

    def test_mod__str2(self):
        py = '"Hello %s %s" % ("World", "Kaito")'
        self.py(py, expected='"Hello %s %s" % ("World", "Kaito")')
        self.java(py, expected='String.format("Hello %s %s", "World", "Kaito");')
        self.elisp(py, expected='(format "Hello %s %s" "World" "Kaito")')
        self.go(py, expected='fmt.Sprintf("Hello %s %s", "World", "Kaito")')

if __name__ == '__main__':
    unittest.main()
