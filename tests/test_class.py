import compilertest
import unittest


class ClassTest(compilertest.CompilerTest):

    def test_class(self):
        py = """
class A:
    def __init__(self, n):
        self.name = n
    def greeting(self):
        return "hello, " + self.name
a = A("foo")
gr = A("goo").greeting()
print(gr)
"""
        self.py(py, expected=py)
        self.java(py, expected="""
public class A {
    public String name = null;
    public A(String n) {
        this.name = n;
    }
    public String greeting() {
        return "hello, " + this.name;
    }
}
static A a = new A("foo");
static String gr = new A("goo").greeting();
System.out.println(gr);
""")

        self.go(py, expected="""
type A struct {
    name *string
}
func (self *A) greeting() *string {
    t := "hello, " + self.name
    return &t
}
func (self *A) __init__(n *string) A {
    self.name = n
}
t1 := "foo"
a := A(&t1)
t2 := "goo"
gr := A(&t2).greeting()
fmt.Println(*gr)
""")


if __name__ == '__main__':
    unittest.main()
