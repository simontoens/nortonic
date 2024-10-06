import compilertest
import unittest


class TupleTest(compilertest.CompilerTest):

    def test_homogeneous_types(self):
        py = "t = (1, 2, 3)"
        self.py(py, expected=py)
        self.java(py, expected="static Tuple<Integer, Integer, Integer> t = Tuple.of(1, 2, 3);") # or could be List<Integer>
        self.elisp(py, "(setq t (list 1 2 3))")
        self.go(py, "t := []int{1, 2, 3}") # tuple type?

    def test_non_homogeneous_types(self):
        py = """t = (1, "foo")"""
        self.py(py, expected=py)
        self.java(py, expected="""static Tuple<Integer, String> t = Tuple.of(1, "foo");""")
        self.elisp(py, """(setq t (list 1 "foo"))""")
        self.go(py, """t := []int, string{1, "foo"}""") # TODO
        

if __name__ == '__main__':
    unittest.main()
    
