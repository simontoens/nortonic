from run import run
import syntax as syntaxm
import unittest


class ListTest(unittest.TestCase):

    def test_append_and_get(self):
        py = """
l = []
l.append("foo")
s = l[0]
"""
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected=py)
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="""
List<String> l = List.of();
l.add("foo");
String s = l.get(0);
""")
        self._t(syntax=syntaxm.ElispSyntax(), code=py, expected="""
(setq l (list))
(add-to-list 'l "foo")
(setq s (nth 0 l))
""")

    def test_get(self):
        py = """
l = ["name1", "name2"]
s = l[1]
"""
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected=py)
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="""
List<String> l = List.of("name1", "name2");
String s = l.get(1);
""")
        self._t(syntax=syntaxm.ElispSyntax(), code=py, expected="""
(setq l (list "name1" "name2"))
(setq s (nth 1 l))
""")

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code.strip())


if __name__ == '__main__':
    unittest.main()
