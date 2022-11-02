from tests import compilertest
import unittest


class TupleTest(compilertest.CompilerTest):

    def test_homogeneous_types(self):
        py = "a = 1"
        self.py(py, expected="a = 1")
        self.java(py, expected="static Integer a = 1;")
        self.elisp(py, expected="(setq a 1)")
        

if __name__ == '__main__':
    unittest.main()
    
