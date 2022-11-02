from tests import compilertest
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
public Integer foo() {
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

    def test_if_single_stmt__1(self):
        py = """
name = "smoke"
if name == "water":
    print("ok")
print("done")
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static String name = "smoke";
if (name.equals("water")) {
    System.out.println("ok");
}
System.out.println("done");
""")

        self.elisp(py, expected="""
(setq name "smoke")
(if (equal name "water")
    (message "ok"))
(message "done")
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


if __name__ == '__main__':
    unittest.main()
