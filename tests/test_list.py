import compilertest
import unittest


class ListTest(compilertest.CompilerTest):

    def test_nested_types(self):
        py = """
l1 = [1, 2, 3]
l2 = [l1]
"""
        self.py(py, expected=py)
        self.java(py, expected="""
import java.util.ArrayList;
import java.util.List;

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
import java.util.ArrayList;
import java.util.List;

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
import java.util.ArrayList;
import java.util.List;

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
import java.util.ArrayList;
import java.util.List;

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
        self.java(py, expected="""
import java.util.ArrayList;
import java.util.List;

static List<Integer> t = new ArrayList<>(List.of(1, 2, 3));
""")
        self.elisp(py, "(setq t (list 1 2 3))")
        self.go(py, "t := []int{1, 2, 3}")

    def test_non_homogeneous_types(self):
        py = """t = [1, "foo"]"""
        self.py(py, expected=py)
        self.java(py, expected="""static Tuple<Integer, String> t = Tuple.of(1, "foo");""")
        self.elisp(py, """(setq t (list 1 "foo"))""")
        self.go(py, """t := []int{1, "foo"}""") # TODO

    def test_sort_list(self):
        py = """
l = [3, 2, 1]
l.sort()
"""
        self.py(py, py)
        self.java(py, """
import java.util.ArrayList;
import java.util.List;

static List<Integer> l = new ArrayList<>(List.of(3, 2, 1));
l.sort(null);
""")
        self.elisp(py, """
(setq l (list 3 2 1))
(setq l (sort l '<))
""")
        self.go(py, """
import (
    "sort"
)

l := []int{3, 2, 1}
sort.Slice(l, func(i, j int) bool {
    return l[i] < l[j]
})
""")

    def test_sorted_list(self):
        """
        Not implemented properly, but tests the rtn types for Java.
        """
        py = """
l1 = [3, 2, 1]
l2 = ["foo"]
l3 = sorted(l1)
l4 = sorted(l2)
"""
        self.java(py, """
import java.util.ArrayList;
import java.util.List;

static List<Integer> l1 = new ArrayList<>(List.of(3, 2, 1));
static List<String> l2 = new ArrayList<>(List.of("foo"));
static List<Integer> l3 = sorted(l1);
static List<String> l4 = sorted(l2);
""")

    def test_none_contained_type(self):
        py = """
def foo():
    return None

l = [foo()]
l.append(None)
l.append(foo())
l.append(2)
"""

        self.java(py, """
import java.util.ArrayList;
import java.util.List;

static void foo() {
    return null;
}
static List<Integer> l = new ArrayList<>(List.of(foo()));
l.add(null);
l.add(foo());
l.add(2);
""")


if __name__ == '__main__':
    unittest.main()
