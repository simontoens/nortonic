from run import run
import syntax as syntaxm
import unittest


class FuncTest(unittest.TestCase):

    def test_func_noargs(self):
        py = """
def foo():
    print("hello")
foo()
"""
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected=py)
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="""
public void foo() {
    System.out.println("hello");
}
foo();
""")
        self._t(syntax=syntaxm.ElispSyntax(), code=py, expected="""
(defun foo ()
    (message "hello"))
(foo)
""")

    def test_func_string_arg(self):
        py = """
def foo(a):
    print(a)
foo("hello")
"""
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected=py)
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="""
public void foo(String a) {
    System.out.println(a);
}
foo("hello");
""")
        self._t(syntax=syntaxm.ElispSyntax(), code=py, expected="""
(defun foo (a)
    (message a))
(foo "hello")
""")

    def test_int_return(self):
        py = """
def foo(a):
    return 1
print(foo("test"))
"""
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected=py)
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="""
public Integer foo(String a) {
    return 1;
}
System.out.println(foo("test"));
""")
        self._t(syntax=syntaxm.ElispSyntax(), code=py, expected="""
(defun foo (a)
    1)
(message "%s" (foo "test"))
""")

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code.strip())


if __name__ == '__main__':
    unittest.main()
