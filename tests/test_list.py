from run import run
import syntax as syntaxm
import unittest


class ListTest(unittest.TestCase):

    def test_append(self):
        py = """
l = []
l.append("foo")
"""
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected=py)
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="""
List<String> l = List.of();
l.add("foo");
""")
        self._t(syntax=syntaxm.ElispSyntax(), code=py, expected="""
(setq l (list))
(setq l (append l "foo"))
""")

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code.strip())


if __name__ == '__main__':
    unittest.main()
