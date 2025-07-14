import compilertest
import unittest


class ListCompTest(compilertest.CompilerTest):

    def test_list_comp(self):
        py = """
l = [1, 2, 3]
a = [i for i in l]
"""
        self.py(py, expected=py)

        self.go(py, expected="""
l := []int{1, 2, 3}
a := []int{}
for i1 := 0; i1 < len(l); i1 += 1 {
    i := l[i1]
    a = append(a, i)
}
""")

    def test_list_comp__func(self):
        py = """
def add_one(i):
    return i + 1
l = [1, 2, 3]
a = [add_one(i) for i in l]
"""
        self.py(py, expected=py)

        self.go(py, expected="""
func add_one(i int) int {
    return i + 1
}
l := []int{1, 2, 3}
a := []int{}
for i1 := 0; i1 < len(l); i1 += 1 {
    i := l[i1]
    a = append(a, add_one(i))
}
""")

    def test_list_comp__cond(self):
        py = """
l = [1, 2, 3]
a = [i for i in l if i % 2 == 0]
"""
        self.py(py, expected=py)

        self.go(py, expected="""
l := []int{1, 2, 3}
a := []int{}
for i1 := 0; i1 < len(l); i1 += 1 {
    i := l[i1]
    if i % 2 == 0 {
        a = append(a, i)
    }
}
""")


if __name__ == '__main__':
    unittest.main()
