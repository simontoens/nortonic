from run import run
import syntax as syntaxm
import unittest


class BuiltInFuncTest(unittest.TestCase):

    def test_print__single_arg_int(self):
        py = "print(1)"
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, "System.out.println(1);", syntaxm.JavaSyntax())
        self._t(py, "(message \"%s\" 1)", syntaxm.ElispSyntax())

    def test_print__single_arg_str(self):
        py = 'print("hello")'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, "System.out.println(\"hello\");", syntaxm.JavaSyntax())
        self._t(py, "(message \"hello\")", syntaxm.ElispSyntax())

    def test_print__multiple_args(self):
        py = "print(1, \"foo\", 1.2)"
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, "System.out.println(String.format(\"%d %s %d\", 1, \"foo\", 1.2));", syntaxm.JavaSyntax())
        self._t(py, "(message \"%s %s %s\" 1 \"foo\" 1.2)", syntaxm.ElispSyntax())

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected, generated_code)


if __name__ == '__main__':
    unittest.main()
