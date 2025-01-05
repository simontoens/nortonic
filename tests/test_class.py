import compilertest
import unittest


class ClassTest(compilertest.CompilerTest):

    def test_class(self):
        py = """
class A:
    def __init__(self, name):
        self.name = name
    def greeting(self):
        return "hello, " + self.name
a = A("goo")
a.greeting()
"""
        self.py(py, expected=py)
        self.java(py, expected="""
class A {
    public String name = null;
    public A(String name) {
        this.name = name;
    }
    public String greeting() {
        return "hello, " + this.name;
    }
}
static A a = new A("goo");
a.greeting();
""")


if __name__ == '__main__':
    unittest.main()
