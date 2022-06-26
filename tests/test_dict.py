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
Map<String, Integer> d = new HashMap<>(Map.of("k1", 1, "k2", 2));
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
Map<String, Integer> d = new HashMap<>(Map.of("k1", 1, "k2", 2));
Integer v = d.get("k1");
""")
        self._t(syntax=syntaxm.ElispSyntax(), code=py, expected="""
(setq d #s(hash-table test equal data ("k1" 1 "k2" 2)))
(setq v (gethash "k1" d))
""")

    def test_put(self):
        py = """
d = {"k1": 1, "k2": 2}
d["k2"] = 3
"""
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected=py)
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="""
Map<String, Integer> d = new HashMap<>(Map.of("k1", 1, "k2", 2));
d.put("k2", 3);
""")
        self._t(syntax=syntaxm.ElispSyntax(), code=py, expected="""
(setq d #s(hash-table test equal data ("k1" 1 "k2" 2)))
(puthash "k2" 3 d)
""")

    def test_get_and_put__empty_literal(self):
        py = """
d = {}
el = d[2]
d[1] = "foo"
"""
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected=py)
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="""
Map<Integer, String> d = new HashMap<>(Map.of());
String el = d.get(2);
d.put(1, "foo");
""")
        self._t(syntax=syntaxm.ElispSyntax(), code=py, expected="""
(setq d #s(hash-table test equal data ()))
(setq el (gethash 2 d))
(puthash 1 "foo" d)
""")

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code.strip())


if __name__ == '__main__':
    unittest.main()
