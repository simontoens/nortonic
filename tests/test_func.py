from tests import compilertest
import unittest


class FuncTest(compilertest.CompilerTest):

    def test_func_noargs(self):
        py = """
def foo():
    print("hello")
foo()
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static void foo() {
    System.out.println("hello");
}
foo();
""")
        self.elisp(py, expected="""
(defun foo ()
    (message "hello"))
(foo)
""")

    def test_func_string_arg(self):
        py = """
def foo(a):
    print(a)
foo("hello")
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static void foo(String a) {
    System.out.println(a);
}
foo("hello");
""")
        self.elisp(py, expected="""
(defun foo (a)
    (message a))
(foo "hello")
""")

    def test_func_list_arg(self):
        py = """
def print_ints(list_of_ints):
    for i in list_of_ints:
        print("Got int", i)
l = []
l.append(1)
print_ints(l)
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static void print_ints(List<Integer> list_of_ints) {
    for (Integer i : list_of_ints) {
        System.out.println(String.format("%s %d", "Got int", i));
    }
}
static List<Integer> l = new ArrayList<>(List.of());
l.add(1);
print_ints(l);
""")
        self.elisp(py, expected="""
(defun print_ints (list_of_ints)
    (dolist (i list_of_ints)
        (message "%s %s" "Got int" i)))
(setq l (list))
(add-to-list 'l 1)
(print_ints l)
""")

    def test_int_return(self):
        py = """
def foo(a):
    return 1
print(foo("test"))
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static Integer foo(String a) {
    return 1;
}
System.out.println(foo("test"));
""")
        self.elisp(py, expected="""
(defun foo (a)
    1)
(message "%s" (foo "test"))
""")

    def test_nested(self):
        py = """
def echo(m):
    return m
def say_hello(foo):
    print("hello", foo)
say_hello(echo("name"))
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static String echo(String m) {
    return m;
}
static void say_hello(String foo) {
    System.out.println(String.format("%s %s", "hello", foo));
}
say_hello(echo("name"));
""")
        self.elisp(py, expected="""
(defun echo (m)
    m)
(defun say_hello (foo)
    (message "%s %s" "hello" foo))
(say_hello (echo "name"))
""")

    def test_remove_docstring(self):
        py = """
def foo():
    "this is a doc string"
    print("hello")
foo()
"""
        self.py(py, expected="""
def foo():
    print("hello")
foo()
""")                


if __name__ == '__main__':
    unittest.main()
