import lang.compiler as compiler
import lang.target.elisp
import lang.target.golang
import lang.target.java
import lang.target.python
import unittest


class CompilerTest(unittest.TestCase):

    def setUp(self):
        self.maxDiff = None

    def elisp(self, py, expected):
        self._test(lang.target.elisp.ElispSyntax(), py, expected)

    def go(self, py, expected):
        gotarget = lang.target.golang.GolangSyntax()
        # historically we've tested pointers with some additional types
        pointer_types_for_tests = (str, list, type)
        gotarget.pointer_types = pointer_types_for_tests
        self._test(gotarget, py, expected)

    def java(self, py, expected):
        self._test(lang.target.java.JavaSyntax(), py, expected)

    def py(self, py, expected):
        self._test(lang.target.python.PythonSyntax(), py, expected)

    def _test(self, target, py, expected):
        self.assertEqual(expected.strip(), compiler.transcompile(py, target))
