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
def printGreeting(a):
    gr = a.greeting()
    print(gr)
a = A("foo")
printGreeting(a)
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
static void printGreeting(A a) {
    String gr = a.greeting();
    System.out.println(gr);
}
static A a = new A("foo");
printGreeting(a);
""")

        self.go(py, expected="""
type A struct {
    name *string
}
func (self *A) greeting() *string {
    t := "hello, " + *self.name
    return &t
}
func NewA(n *string) *A {
    self := A{}
    self.name = n
    return &self
}
func printGreeting(a *A) {
    gr := a.greeting()
    fmt.Println(*gr)
}
t1 := "foo"
a := NewA(&t1)
printGreeting(a)
""")


if __name__ == '__main__':
    unittest.main()
