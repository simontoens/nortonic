from tests import compilertest
import unittest


class ListTest(compilertest.CompilerTest):

    def test_nested_types(self):
        py = """
l1 = [1, 2, 3]
l2 = [l1]
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static List<Integer> l1 = new ArrayList<>(List.of(1, 2, 3));
static List<List<Integer>> l2 = new ArrayList<>(List.of(l1));
""")
        self.elisp(py, expected="""
(setq l1 (list 1 2 3))
(setq l2 (list l1))
""")

        self.go(py, expected="""
l1 := []int{1, 2, 3}
l2 := [][]int{l1}
""")

    def test_append_and_get(self):
        py = """
l = []
l.append("foo")
s = l[0]
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static List<String> l = new ArrayList<>();
l.add("foo");
static String s = l.get(0);
""")
        self.elisp(py, expected="""
(setq l (list))
(add-to-list 'l "foo")
(setq s (nth 0 l))
""")

        self.go(py, expected="""
l := []string{}
l = append(l, "foo")
s := l[0]
""")

    def test_get_and_append(self):
        py = """
l = []
s = l[0]
l.append("foo")
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static List<String> l = new ArrayList<>();
static String s = l.get(0);
l.add("foo");
""")
        self.elisp(py, expected="""
(setq l (list))
(setq s (nth 0 l))
(add-to-list 'l "foo")
""")
        self.go(py, expected="""
l := []string{}
s := l[0]
l = append(l, "foo")
""")

    def test_get(self):
        py = """
l = ["name1", "name2"]
s = l[1]
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static List<String> l = new ArrayList<>(List.of("name1", "name2"));
static String s = l.get(1);
""")
        self.elisp(py, expected="""
(setq l (list "name1" "name2"))
(setq s (nth 1 l))
""")
        self.go(py, expected="""
l := []string{"name1", "name2"}
s := l[1]
""")

    def test_homogeneous_types(self):
        py = "t = [1, 2, 3]"
        self.py(py, expected=py)
        self.java(py, expected="static List<Integer> t = new ArrayList<>(List.of(1, 2, 3));")
        self.elisp(py, "(setq t (list 1 2 3))")
        self.go(py, "t := []int{1, 2, 3}")

    def test_non_homogeneous_types(self):
        py = """t = [1, "foo"]"""
        self.py(py, expected=py)
        self.java(py, expected="""static Tuple<Integer, String> t = Tuple.of(1, "foo");""")
        self.elisp(py, """(setq t (list 1 "foo"))""")
        self.go(py, """t := []int{1, "foo"}""") # TODO

if __name__ == '__main__':
    unittest.main()
