import compilertest
import unittest


class ClassTest(compilertest.CompilerTest):

    def test_class(self):
        py = """
class A:
    def __init__(self, name):
        print("ok")
    def greeting(self, name):
        return "hello, " + name
a = A("goo")
a.greeting("foo")
"""
        self.py(py, expected=py)
        self.java(py, expected="""
class A {
    A(String name) {
        System.out.println("ok");
    }
    String greeting(String name) {
        return "hello, " + name;
    }
}
static A a = A("goo");
a.greeting("foo");
""")


if __name__ == '__main__':
    unittest.main()
