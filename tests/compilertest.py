from run import run
import target.elisp
import target.java
import target.python
import unittest


class CompilerTest(unittest.TestCase):

    def elisp(self, py, expected):
        self._test(target.elisp.ElispSyntax(), py, expected)

    def golang(self, py, expected):
        self._test(target.golang.GolangSyntax(), py, expected)

    def java(self, py, expected):
        self._test(target.java.JavaSyntax(), py, expected)

    def py(self, py, expected):
        self._test(target.python.PythonSyntax(), py, expected)

    def _test(self, target, py, expected):
        self.assertEqual(expected.strip(), run(py, target))
