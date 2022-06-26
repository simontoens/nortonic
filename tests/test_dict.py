from run import run
import syntax as syntaxm
import unittest


class DictTest(unittest.TestCase):

    def test_literal(self):
        py = """
d = {"k1": 1, "k2": 2}
"""
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected=py)
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="""
Map<String, Integer> d = Map.of("k1", 1, "k2", 2);
""")
        self._t(syntax=syntaxm.ElispSyntax(), code=py, expected="""
(setq d #s(hash-table test equal data ("k1" 1 "k2" 2)))
""")

    def test_get(self):
        py = """
d = {"k1": 1, "k2": 2}
v = d["k1"]
"""
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected=py)
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="""
Map<String, Integer> d = Map.of("k1", 1, "k2", 2);
Integer v = d.get("k1");
""")
        self._t(syntax=syntaxm.ElispSyntax(), code=py, expected="""
(setq d #s(hash-table test equal data ("k1" 1 "k2" 2)))
(setq v (gethash "k1" d))
""")

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code.strip())


if __name__ == '__main__':
    unittest.main()
