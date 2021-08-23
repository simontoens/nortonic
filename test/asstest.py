from run import *
import ast as astm
import syntax as syntaxm
import unittest


class AssignmentTest(unittest.TestCase):

    def test_assign_int(self):
        py = "a = 1"
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected="a=1")
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="int a=1;")

    def test_assign_float1(self):
        py = "a = 1.2"
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected="a=1.2")
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="float a=1.2;")

    def test_assign_float2(self):
        py = "a = 10 * 1.2"
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected="a=10*1.2")
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="float a=10*1.2;")

    def test_assign_string(self):
        py = "a = 'name'"
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected="a=\"name\"")
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="String a=\"name\";")

    def test_assign_ref1(self):
        py = "a = 'hello' ;print(a)"
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected="""
a="hello"
print(a)
""")
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="""
String a="hello";
System.out.println(a);
""")


    def test_assign_ref2(self):
        py = "a = 1+2 ;print(a*3)"
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected="""
a=1+2
print(a*3)
""")
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="""
int a=1+2;
System.out.println(a*3);
""")        

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code)


if __name__ == '__main__':
    unittest.main()
