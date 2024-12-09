import compilertest
import unittest


class ClassTest(compilertest.CompilerTest):

    def test_class(self):
        py = """
class A:
    def greeting(self, name):
        return "hello, " + name
a = A()
a.greeting("foo")
"""
        self.py(py, expected=py)
        self.java(py, expected="""
class A {
    String greeting(String name) {
        return "hello, " + name;
    }
}
static A a = A();
a.greeting("foo");
""")


if __name__ == '__main__':
    unittest.main()
