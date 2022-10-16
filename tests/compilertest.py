from run import run
from target import targetlanguage
import unittest


class CompilerTest(unittest.TestCase):

    def py(self, py, expected):
        self._test(targetlanguage.PythonSyntax(), py, expected)

    def java(self, py, expected):
        self._test(targetlanguage.JavaSyntax(), py, expected)

    def elisp(self, py, expected):
        self._test(targetlanguage.ElispSyntax(), py, expected)

    def _test(self, target, py, expected):
        self.assertEqual(expected.strip(), run(py, target))
