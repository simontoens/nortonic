from run import run
import syntax as syntaxm
import unittest


class StrTest(unittest.TestCase):

    def test_slice(self):
        py = """
s = "Kaito"
s2 = s[1:2]
"""
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected=py)
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="""
String s = "Kaito";
String s2 = s.substring(1, 2);
""")
        self._t(syntax=syntaxm.ElispSyntax(), code=py, expected="""
(setq s "Kaito")
(setq s2 (substring s 1 2))
""")

    def test_slice__negative_2nd_index(self):
        py = """
s = "Kaito"
s2 = "arub" + s[1:-2] + "to"
"""
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected=py)
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="""
String s = "Kaito";
String s2 = "arub" + s.substring(1, s.length() - 2) + "to";
""")
        self._t(syntax=syntaxm.ElispSyntax(), code=py, expected="""
(setq s "Kaito")
(setq s2 (concat (concat "arub" (substring s 1 -2)) "to"))
""")

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code.strip())


if __name__ == '__main__':
    unittest.main()
