import compilertest
import unittest


class IfTest(compilertest.CompilerTest):

    def test_if_expr__assignment(self):
        py = """
a = 2 if 1 == 1 else 3
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static Integer a = 1 == 1 ? 2 : 3;
""")

        self.elisp(py, expected="""
(setq a (if (equal 1 1)
    2
    3))
""")

        self.go(py, expected="""
if 1 == 1 {
    a := 2
} else {
    a := 3
}
""")

    def test_if_expr__assignment__if_branch_none(self):
        py = """
a = None if 1 == 1 else 3
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static Integer a = 1 == 1 ? null : 3;
""")

        self.elisp(py, expected="""
(setq a (if (equal 1 1)
    nil
    3))
""")
        
    def test_if_expr__assignment__else_branch_none(self):
        py = """
a = 1 if 1 == 1 else None
"""
        self.py(py, expected=py)

        self.java(py,  expected="""
static Integer a = 1 == 1 ? 1 : null;
""")

        self.elisp(py, expected="""
(setq a (if (equal 1 1)
    1
    nil))
""")

    def test_if_expr__rtn(self):
        py = """
def foo():
    return 1 if 2 == 3 else 2
foo()
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static Integer foo() {
    return 2 == 3 ? 1 : 2;
}
foo();
""")

        self.elisp(py, expected="""
(defun foo ()
    (if (equal 2 3)
        1
        2))
(foo)
""")

        self.go(py, expected="""
func foo() int {
    var t int
    if 2 == 3 {
        t = 1
    } else {
        t = 2
    }
    return t
}
foo()
""")

    def test_if_expr__nested(self):
        py = """
a = 2
b = 3 if a > 100 else 200 if a > 50 else 1
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static Integer a = 2;
static Integer b = a > 100 ? 3 : a > 50 ? 200 : 1;
""")

        self.elisp(py, expected="""
(setq a 2)
(setq b (if (> a 100)
    3
    (if (> a 50)
        200
        1)))
""")

        self.go(py, expected="""
a := 2
if a > 100 {
    b := 3
} else {
    if a > 50 {
        b := 200
    } else {
        b := 1
    }
}
""")

    def test_if_expr__complicated_nested(self):
        py = """
def foo(i):
    return i + 1
a = 2
b = 3 if a > 100 else foo(3 if 1 == 1 else 4 if 2 == 2 else 5) if a == 2 else foo(1 if a > 100 else 2 if a > 200 else a)
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static Integer foo(Integer i) {
    return i + 1;
}
static Integer a = 2;
static Integer b = a > 100 ? 3 : a == 2 ? foo(1 == 1 ? 3 : 2 == 2 ? 4 : 5) : foo(a > 100 ? 1 : a > 200 ? 2 : a);
""")

        self.elisp(py, expected="""
(defun foo (i)
    (+ i 1))
(setq a 2)
(setq b (if (> a 100)
    3
    (if (equal a 2)
        (foo (if (equal 1 1)
            3
            (if (equal 2 2)
                4
                5)))
        (foo (if (> a 100)
            1
            (if (> a 200)
                2
                a))))))
""")

        self.go(py, expected="""
func foo(i int) int {
    return i + 1
}
a := 2
var t int
if 1 == 1 {
    t = 3
} else {
    if 2 == 2 {
        t = 4
    } else {
        t = 5
    }
}
var t1 int
if a > 100 {
    t1 = 1
} else {
    if a > 200 {
        t1 = 2
    } else {
        t1 = a
    }
}
if a > 100 {
    b := 3
} else {
    if a == 2 {
        b := foo(t)
    } else {
        b := foo(t1)
    }
}
""")

    def test_if_single_stmt__1(self):
        py = """
name = "smoke"
if name == "water":
    status = "ok"
    print(status)
print("done")
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static String name = "smoke";
if (name.equals("water")) {
    String status = "ok";
    System.out.println(status);
}
System.out.println("done");
""")

        self.elisp(py, expected="""
(setq name "smoke")
(if (equal name "water")
    (progn
        (setq status "ok")
        (message status)))
(message "done")
""")

        self.go(py, expected="""
import (
    "fmt"
)

name := "smoke"
if name == "water" {
    status := "ok"
    fmt.Println(status)
}
fmt.Println("done")
""")

    def test_if_single_stmt__2(self):
        py = """
name = "smoke"
if name == "water":
    name = "water"
print("done")
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static String name = "smoke";
if (name.equals("water")) {
    name = "water";
}
System.out.println("done");
""")

        self.elisp(py, expected="""
(setq name "smoke")
(if (equal name "water")
    (setq name "water"))
(message "done")
""")

        self.go(py, expected="""
import (
    "fmt"
)

name := "smoke"
if name == "water" {
    name = "water"
}
fmt.Println("done")
""")

    def test_if_multiple_stmts(self):
        py = """
name = "smoke"
if name == "water":
    print("ok")
    name = "water"
print("done")
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static String name = "smoke";
if (name.equals("water")) {
    System.out.println("ok");
    name = "water";
}
System.out.println("done");
""")

        self.elisp(py, expected="""
(setq name "smoke")
(if (equal name "water")
    (progn
        (message "ok")
        (setq name "water")))
(message "done")
""")

        self.go(py, expected="""
import (
    "fmt"
)

name := "smoke"
if name == "water" {
    fmt.Println("ok")
    name = "water"
}
fmt.Println("done")
""")

    def test_if_else_single_stmt(self):
        py = """
name = "smoke"
if name == "water":
    print("ok")
else:
    print("computer")
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static String name = "smoke";
if (name.equals("water")) {
    System.out.println("ok");
} else {
    System.out.println("computer");
}
""")

        self.elisp(py, expected="""
(setq name "smoke")
(if (equal name "water")
    (message "ok")
    (message "computer"))
""")

        self.go(py, expected="""
import (
    "fmt"
)

name := "smoke"
if name == "water" {
    fmt.Println("ok")
} else {
    fmt.Println("computer")
}
""")

    def test_if_else_multiple_stmt(self):
        py = """
name = "smoke"
if name == "water":
    print("ok")
    print("radio")
else:
    print("computer")
    print("head")
print("done")
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static String name = "smoke";
if (name.equals("water")) {
    System.out.println("ok");
    System.out.println("radio");
} else {
    System.out.println("computer");
    System.out.println("head");
}
System.out.println("done");
""")

        self.elisp(py, expected="""
(setq name "smoke")
(if (equal name "water")
    (progn
        (message "ok")
        (message "radio"))
    (message "computer")
    (message "head"))
(message "done")
""")

        self.go(py, expected="""
import (
    "fmt"
)

name := "smoke"
if name == "water" {
    fmt.Println("ok")
    fmt.Println("radio")
} else {
    fmt.Println("computer")
    fmt.Println("head")
}
fmt.Println("done")
""")

    def test_nested_if(self):
        py = """
name = "water"
if name == "water":
    if 1 == 1:
        print("yes")
    else:
        print("no")
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static String name = "water";
if (name.equals("water")) {
    if (1 == 1) {
        System.out.println("yes");
    } else {
        System.out.println("no");
    }
}
""")

        self.elisp(py, expected="""
(setq name "water")
(if (equal name "water")
    (if (equal 1 1)
        (message "yes")
        (message "no")))
""")

        self.go(py, expected="""
import (
    "fmt"
)

name := "water"
if name == "water" {
    if 1 == 1 {
        fmt.Println("yes")
    } else {
        fmt.Println("no")
    }
}
""")
                   
    def test_elif(self):
        """
        In the AST, elif has been normalized to nested if/else.
        """
        py = """
if 1 == 1:
    print(1)
elif 1 == 2:
    print(2)
else:
    print(3)
print("done")
"""
        self.py(py, expected="""
if 1 == 1:
    print(1)
else:
    if 1 == 2:
        print(2)
    else:
        print(3)
print("done")
""")

        self.java(py, expected="""
if (1 == 1) {
    System.out.println(1);
} else {
    if (1 == 2) {
        System.out.println(2);
    } else {
        System.out.println(3);
    }
}
System.out.println("done");
""")

        self.elisp(py, expected="""
(if (equal 1 1)
    (message "%s" 1)
    (if (equal 1 2)
        (message "%s" 2)
        (message "%s" 3)))
(message "done")
""")

        self.go(py, expected="""
import (
    "fmt"
)

if 1 == 1 {
    fmt.Println(1)
} else {
    if 1 == 2 {
        fmt.Println(2)
    } else {
        fmt.Println(3)
    }
}
fmt.Println("done")
""")


if __name__ == '__main__':
    unittest.main()
