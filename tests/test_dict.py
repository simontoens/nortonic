import compilertest
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
import java.util.ArrayList;
import java.util.List;

static List<Integer> l1 = new ArrayList<>(List.of(1, 2, 3));
static Map<String, List<Integer>> d1 = new HashMap<>(Map.of("k1", l1));
static Map<String, Map<String, List<Integer>>> d2 = new HashMap<>(Map.of("k2", d1));
""")
        self.elisp(py, expected="""
(setq l1 (list 1 2 3))
(setq d1 #s(hash-table test equal data ("k1" l1)))
(setq d2 #s(hash-table test equal data ("k2" d1)))
""")

        self.go(py, expected="""
l1 := []int{1, 2, 3}
d1 := map[string][]int{"k1": l1}
d2 := map[string]map[string][]int{"k2": d1}
""")

    def test_literal(self):
        py = """
d = {"k1": 1, "k2": 2}
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static Map<String, Integer> d = new HashMap<>(Map.of("k1", 1, "k2", 2));
""")
        self.elisp(py, expected="""
(setq d #s(hash-table test equal data ("k1" 1 "k2" 2)))
""")

        self.go(py, expected="""
d := map[string]int{"k1": 1, "k2": 2}
""")

    def test_get(self):
        py = """
d = {"k1": 1, "k2": 2}
v = d["k1"]
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static Map<String, Integer> d = new HashMap<>(Map.of("k1", 1, "k2", 2));
static Integer v = d.get("k1");
""")
        self.elisp(py, expected="""
(setq d #s(hash-table test equal data ("k1" 1 "k2" 2)))
(setq v (gethash "k1" d))
""")

        self.go(py, expected="""
d := map[string]int{"k1": 1, "k2": 2}
v := d["k1"]
""")
        

    def test_put(self):
        py = """
d = {"k1": 1, "k2": 2}
d["k2"] = 3
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static Map<String, Integer> d = new HashMap<>(Map.of("k1", 1, "k2", 2));
d.put("k2", 3);
""")
        self.elisp(py, expected="""
(setq d #s(hash-table test equal data ("k1" 1 "k2" 2)))
(puthash "k2" 3 d)
""")

        self.go(py, expected="""
d := map[string]int{"k1": 1, "k2": 2}
d["k2"] = 3
""")

    def test_get_and_put__empty_literal(self):
        py = """
d = {}
el = d[2]
d[1] = "foo"
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static Map<Integer, String> d = new HashMap<>(Map.of());
static String el = d.get(2);
d.put(1, "foo");
""")
        self.elisp(py, expected="""
(setq d #s(hash-table test equal data ()))
(setq el (gethash 2 d))
(puthash 1 "foo" d)
""")

        self.go(py, expected="""
d := map[int]string{}
el := d[2]
d[1] = "foo"
""")

    def test_none_contained_type(self):
        py = """
def foo():
    return None

d = {foo(): foo()}
d[None] = None
d[foo()] = foo()
d[2] = 3
"""

        self.java(py, """
static void foo() {
    return null;
}
static Map<Integer, Integer> d = new HashMap<>(Map.of(foo(), foo()));
d.put(null, null);
d.put(foo(), foo());
d.put(2, 3);
""")

def test_separate_key_and_value_types(self):
    """
    Verifies that key and value types do not have to be set in the same
    assignment.
    """
    py = """
def foo(d):
    return d[1]
d = {}
d[foo(d)] = 2
d[1] = foo(d)
"""
    self.py(py, expected=py)
    self.java(py, expected="""
static Integer foo(Map<Integer, Integer> d) {
    return d.get(1);
}
static Map<Integer, Integer> d = new HashMap<>(Map.of());
d.put(foo(d), 2);
d.put(1, foo(d));
""")
    self.go(py, expected="""
func foo(d2 map[int]int) int {
    return d2[1]
}
d := map[int]int{}
d[foo(d)] = 2
d[1] = foo(d)
""")
    self.elisp(py, expected="""
(defun foo (d2)
    (gethash 1 d2))
(setq d #s(hash-table test equal data ()))
(puthash (foo d) 2 d)
(puthash 1 (foo d) d)
""")


if __name__ == '__main__':
    unittest.main()
