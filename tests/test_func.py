import compilertest
import unittest


class FuncTest(compilertest.CompilerTest):

    def test_func_noargs(self):
        py = """
def foo():
    print("hello")
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static void foo() {
    System.out.println("hello");
}
""")
        self.elisp(py, expected="""
(defun foo ()
    (message "hello"))
""")
        self.go(py, expected="""
import (
    "fmt"
)

func foo() {
    fmt.Println("hello")
}
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
        self.go(py, expected="""
import (
    "fmt"
)

func foo(a *string) {
    fmt.Println(*a)
}
t := "hello"
foo(&t)
""")

    def test_func_string_arg__no_invocation(self):
        py = """
def foo(a):
    a = "foo"
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static void foo(String a) {
    a = "foo";
}
""")
        self.elisp(py, expected="""
(defun foo (a)
    (setq a "foo"))
""")
        self.go(py, expected="""
func foo(a *string) {
    *a = "foo"
}
""")

    def test_func_int_arg__no_invocation__used_as_subscript(self):
        """
        This is only implemented for subscript syntax.
        """
        py = """
def foo(i):
    l = [1, 2]
    return l[i]
"""
        self.py(py, expected=py)
        self.go(py, expected="""
func foo(i int) int {
    l := []int{1, 2}
    return l[i]
}
""")

    def test_func_list_arg(self):
        py = """
def print_ints(lots_of_ints):
    print("Lots of ints:", lots_of_ints)
l = [1]
print_ints(l)
"""
        self.py(py, expected=py)
        self.java(py, expected="""
import java.util.ArrayList;
import java.util.List;

static void print_ints(List<Integer> lots_of_ints) {
    System.out.println(String.format("Lots of ints: %s", lots_of_ints));
}
static List<Integer> l = new ArrayList<>(List.of(1));
print_ints(l);
""")
        self.elisp(py, expected="""
(defun print_ints (lots_of_ints)
    (message "%s %s" "Lots of ints:" lots_of_ints))
(setq l (list 1))
(print_ints l)
""")
        self.go(py, expected="""
import (
    "fmt"
)

func print_ints(lots_of_ints *[]int) {
    fmt.Println("Lots of ints:", *lots_of_ints)
}
l := []int{1}
print_ints(&l)
""")

    def test_func_arg__decl_node_is_none(self):
        py = """
def foo(a):
    return a + 1
i = None
i = 1
foo(i)
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static Integer foo(Integer a) {
    return a + 1;
}
static Integer i = null;
i = 1;
foo(i);
""")
        self.go(py, expected="""
func foo(a int) int {
    return a + 1
}
var i int
i = 1
foo(i)
""")
        self.elisp(py, expected="""
(defun foo (a)
    (+ a 1))
(setq i nil)
(setq i 1)
(foo i)
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
        self.go(py, expected="""
import (
    "fmt"
)

func foo(a *string) int {
    return 1
}
t := "test"
fmt.Println(foo(&t))
""")

    def test_str_and_None_return(self):
        py = """
def foo(a):
    if a == "test":
        return None
    else:
        return "Naha->Kobe"
print(foo("test"))
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static String foo(String a) {
    if (a.equals("test")) {
        return null;
    } else {
        return "Naha->Kobe";
    }
}
System.out.println(foo("test"));
""")
        self.elisp(py, expected="""
(defun foo (a)
    (if (equal a "test")
        nil
        "Naha->Kobe"))
(message (foo "test"))
""")
        self.go(py, expected="""
import (
    "fmt"
)

func foo(a *string) *string {
    if *a == "test" {
        return nil
    } else {
        t := "Naha->Kobe"
        return &t
    }
}
t1 := "test"
fmt.Println(*foo(&t1))
""")

    def test_tuple_return(self):
        """
        A function returning a tuple identifier (non-literal) is NOT interpreted
        as returning multiple values.
        """
        py = """
def foo():
    t = (1, "hello", 1.2)
    return t
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static Tuple<Integer, String, Float> foo() {
    Tuple<Integer, String, Float> t = Tuple.of(1, "hello", 1.2);
    return t;
}
""")
        self.elisp(py, expected="""
(defun foo ()
    (setq t (list 1 "hello" 1.2))
    t)
""")
        self.go(py, expected="""
func foo() []Tuple {
    t := []int, string, float32{1, "hello", 1.2}
    return t
}
""")

    def test_multiple_return_values(self):
        """
        Returning a tuple literal is interpreted as returning multiple values.
        """
        py = """
def foo():
    return 1, "hello", 1.2
a, b, c = foo()
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static Tuple<Integer, String, Float> foo() {
    return Tuple.of(1, "hello", 1.2);
}
static Tuple<Integer, String, Float> t = foo();
static Integer a = t.get(0);
static String b = t.get(1);
static Float c = t.get(2);
""")
        self.elisp(py, expected="""
(defun foo ()
    (list 1 "hello" 1.2))
(setq t (foo))
(setq a (nth 0 t))
(setq b (nth 1 t))
(setq c (nth 2 t))
""")
        self.go(py, expected="""
func foo() (int, string, float32) {
    return 1, "hello", 1.2
}
a, b, c := foo()
""")

    def test_multiple_return_values__with_and_without_callsite_unwrapping(self):
        """
        Returning a tuple literal is interpreted as returning multiple values,
        but for Golang we check whether the callsite actually uses the function
        as if it returns multiple values or as if it returns a slice.
        """
        py = """
def foo():
    return 1, 2
a, b = foo()
t = foo()
print(a, b, t)
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static Tuple<Integer, Integer> foo() {
    return Tuple.of(1, 2);
}
static Tuple<Integer, Integer> t1 = foo();
static Integer a = t1.get(0);
static Integer b = t1.get(1);
static Tuple<Integer, Integer> t = foo();
System.out.println(String.format("%d %d %s", a, b, t));
""")
        self.elisp(py, expected="""
(defun foo ()
    (list 1 2))
(setq t1 (foo))
(setq a (nth 0 t1))
(setq b (nth 1 t1))
(setq t (foo))
(message "%s %s %s" a b t)
""")
        self.go(py, expected="""
import (
    "fmt"
)

func foo() []int {
    return []int{1, 2}
}
t1 := foo()
a := t1[0]
b := t1[1]
t := foo()
fmt.Println(a, b, t)
""")

    def test_nested_calls(self):
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
    System.out.println(String.format("hello %s", foo));
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
        self.go(py, expected="""
import (
    "fmt"
)

func echo(m *string) *string {
    return m
}
func say_hello(foo *string) {
    fmt.Println("hello", *foo)
}
t := "name"
say_hello(echo(&t))
""")

    def test_type_information_propagates_through_calls__list(self):
        py = """
def f1(l1, o):
    f2(l1, o)
def f2(l2, o):
    f3(l2, o)
def f3(l3, o):
    l3.append(1)
    print("Hello", o)
l = []
f1(l, "msg")
print("List:", l)
"""
        self.py(py, expected=py)

        self.java(py, expected="""
import java.util.ArrayList;
import java.util.List;

static void f1(List<Integer> l1, String o) {
    f2(l1, o);
}
static void f2(List<Integer> l2, String o) {
    f3(l2, o);
}
static void f3(List<Integer> l3, String o) {
    l3.add(1);
    System.out.println(String.format("Hello %s", o));
}
static List<Integer> l = new ArrayList<>();
f1(l, "msg");
System.out.println(String.format("List: %s", l));
""")

        # this doesn't actually work in elisp - at least not the naive way -
        # the caller doesn't see the element added in the function
        self.elisp(py, expected="""
(defun f1 (l1 o)
    (f2 l1 o))
(defun f2 (l2 o)
    (f3 l2 o))
(defun f3 (l3 o)
    (add-to-list 'l3 1)
    (message "%s %s" "Hello" o))
(setq l (list))
(f1 l "msg")
(message "%s %s" "List:" l)
""")

        self.go(py, expected="""
import (
    "fmt"
)

func f1(l1 *[]int, o *string) {
    f2(l1, o)
}
func f2(l2 *[]int, o *string) {
    f3(l2, o)
}
func f3(l3 *[]int, o *string) {
    *l3 = append(*l3, 1)
    fmt.Println("Hello", *o)
}
l := []int{}
t := "msg"
f1(&l, &t)
fmt.Println("List:", l)
""")

    def test_type_information_propagates_through_calls__dict(self):
        py = """
def f(m):
    f2(m)
def f2(m2):
    m2[1] = "foo"
d = {}
f(d)
print("Dict:", d)
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static void f(Map<Integer, String> m) {
    f2(m);
}
static void f2(Map<Integer, String> m2) {
    m2.put(1, "foo");
}
static Map<Integer, String> d = new HashMap<>(Map.of());
f(d);
System.out.println(String.format("Dict: %s", d));
""")

        self.elisp(py, expected="""
(defun f (m)
    (f2 m))
(defun f2 (m2)
    (puthash 1 "foo" m2))
(setq d #s(hash-table test equal data ()))
(f d)
(message "%s %s" "Dict:" d)
""")

        self.go(py, expected="""
import (
    "fmt"
)

func f(m map[int]string) {
    f2(m)
}
func f2(m2 map[int]string) {
    m2[1] = "foo"
}
d := map[int]string{}
f(d)
fmt.Println("Dict:", d)
""")

    def test_return_from_func_if_stmt(self):
        py = """
def read_lines(path2):
    if path2 == "":
        return ["a", "b"]
    return ["a", "b"]
read_lines("")
"""
        self.py(py, """
def read_lines(path2):
    if path2 == "":
        return "a", "b"
    return "a", "b"
read_lines("")
""")

        self.java(py, """
static List<String> read_lines(String path2) {
    if (path2.equals("")) {
        return new ArrayList<>(List.of("a", "b"));
    }
    return new ArrayList<>(List.of("a", "b"));
}
read_lines("");
""")

        self.go(py, """
func read_lines(path2 *string) *[]string {
    if *path2 == "" {
        t := []string{"a", "b"}
        return &t
    }
    t1 := []string{"a", "b"}
    return &t1
}
t2 := ""
read_lines(&t2)
""")

        self.elisp(py, """
(defun read_lines (path2)
    (if (equal path2 "")
        (list "a" "b"))
    (list "a" "b"))
(read_lines "")
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
