from tests import compilertest
import unittest


class DictTest(compilertest.CompilerTest):

    def test_nested_types(self):
        py = """
l1 = [1, 2, 3]
d1 = {"k1": l1}
d2 = {"k2": d1}
"""
        self.py(py, expected=py)
        self.java(py, expected="""
List<Integer> l1 = new ArrayList<>(List.of(1, 2, 3));
Map<String, List<Integer>> d1 = new HashMap<>(Map.of("k1", l1));
Map<String, Map<String, List<Integer>>> d2 = new HashMap<>(Map.of("k2", d1));
""")
        self.elisp(py, expected="""
(setq l1 (list 1 2 3))
(setq d1 #s(hash-table test equal data ("k1" l1)))
(setq d2 #s(hash-table test equal data ("k2" d1)))
""")

    def test_literal(self):
        py = """
d = {"k1": 1, "k2": 2}
"""
        self.py(py, expected=py)
        self.java(py, expected="""
Map<String, Integer> d = new HashMap<>(Map.of("k1", 1, "k2", 2));
""")
        self.elisp(py, expected="""
(setq d #s(hash-table test equal data ("k1" 1 "k2" 2)))
""")

    def test_get(self):
        py = """
d = {"k1": 1, "k2": 2}
v = d["k1"]
"""
        self.py(py, expected=py)
        self.java(py, expected="""
Map<String, Integer> d = new HashMap<>(Map.of("k1", 1, "k2", 2));
Integer v = d.get("k1");
""")
        self.elisp(py, expected="""
(setq d #s(hash-table test equal data ("k1" 1 "k2" 2)))
(setq v (gethash "k1" d))
""")

    def test_put(self):
        py = """
d = {"k1": 1, "k2": 2}
d["k2"] = 3
"""
        self.py(py, expected=py)
        self.java(py, expected="""
Map<String, Integer> d = new HashMap<>(Map.of("k1", 1, "k2", 2));
d.put("k2", 3);
""")
        self.elisp(py, expected="""
(setq d #s(hash-table test equal data ("k1" 1 "k2" 2)))
(puthash "k2" 3 d)
""")

    def test_get_and_put__empty_literal(self):
        py = """
d = {}
el = d[2]
d[1] = "foo"
"""
        self.py(py, expected=py)
        self.java(py, expected="""
Map<Integer, String> d = new HashMap<>(Map.of());
String el = d.get(2);
d.put(1, "foo");
""")
        self.elisp(py, expected="""
(setq d #s(hash-table test equal data ()))
(setq el (gethash 2 d))
(puthash 1 "foo" d)
""")


if __name__ == '__main__':
    unittest.main()
