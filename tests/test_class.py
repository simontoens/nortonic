import compilertest
import unittest


class ClassTest(compilertest.CompilerTest):

    def test_class(self):
        py = """
class A:
    def f1(self):
        return 1
    def f2(self):
        return "test"
"""
        self.py(py, expected=py)
        self.java(py, expected="""
class A {
    Integer f1() {
        return 1;
    }
    String f2() {
        return "test";
    }
}
""")


if __name__ == '__main__':
    unittest.main()
