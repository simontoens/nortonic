import compilertest
import unittest


class BlockScopeTest(compilertest.CompilerTest):

    def test_declaration_before_block(self):
        py = """
name = "smoke"
if name == "water":
    name = "water"
print(name)
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static String name = "smoke";
if (name.equals("water")) {
    name = "water";
}
System.out.println(name);
""")

        self.elisp(py, expected="""
(setq name "smoke")
(if (equal name "water")
    (setq name "water"))
(message name)
""")

        self.go(py, expected="""
import (
    "fmt"
)

name := "smoke"
if name == "water" {
    name = "water"
}
fmt.Println(name)
""")

    def test_declaration_in_block(self):
        py = """
if 1 == 1:
    name = "water"
print(name)
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static String name = null;
if (1 == 1) {
    name = "water";
}
System.out.println(name);
""")

        self.elisp(py,  expected="""
(if (equal 1 1)
    (setq name "water"))
(message name)
""")

        self.go(py, expected="""
import (
    "fmt"
)

var name string
if 1 == 1 {
    name = "water"
}
fmt.Println(name)
""")

    def test_declaration_in_block__function(self):
        py = """
def foo():
    if 1 == 1:
        name = "water"
    print(name)
foo()
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static void foo() {
    String name = null;
    if (1 == 1) {
        name = "water";
    }
    System.out.println(name);
}
foo();
    """)

        self.elisp(py, expected="""
(defun foo ()
    (if (equal 1 1)
        (setq name "water"))
    (message name))
(foo)
""")

        self.go(py, expected="""
import (
    "fmt"
)

func foo() {
    var name string
    if 1 == 1 {
        name = "water"
    }
    fmt.Println(name)
}
foo()
""")

    def test_declaration_in_block_innermost_block(self):
        py = """
if 1 == 1:
    if 2 == 2:
        status = "live is life"
    else:
        status = "live is not life"
else:
    status = "two is not one"
print(status)
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static String status = null;
if (1 == 1) {
    if (2 == 2) {
        status = "live is life";
    } else {
        status = "live is not life";
    }
} else {
    status = "two is not one";
}
System.out.println(status);
""")

        self.elisp(py, expected="""
(if (equal 1 1)
    (if (equal 2 2)
        (setq status "live is life")
        (setq status "live is not life"))
    (setq status "two is not one"))
(message status)
""")

        self.go(py, expected="""
import (
    "fmt"
)

var status string
if 1 == 1 {
    if 2 == 2 {
        status = "live is life"
    } else {
        status = "live is not life"
    }
} else {
    status = "two is not one"
}
fmt.Println(status)
""")

    def test_if_test__ref_previous_block(self):
        """
        The test in the if-stmt references an identifier declared in the
        previous block.
        """
        py = """
numbers = [1, 2, 3]
if len(numbers) == 3:
    ok = True
if ok:
    print("ok")
"""
        self.py(py, expected=py)

        self.java(py, expected="""
import java.util.ArrayList;
import java.util.List;

static List<Integer> numbers = new ArrayList<>(List.of(1, 2, 3));
static Boolean ok = null;
if (numbers.size() == 3) {
    ok = true;
}
if (ok) {
    System.out.println("ok");
}
""")

        self.elisp(py, expected="""
(setq numbers (list 1 2 3))
(if (equal (length numbers) 3)
    (setq ok t))
(if ok
    (message "ok"))
""")

        self.go(py, expected="""
import (
    "fmt"
)

numbers := []int{1, 2, 3}
var ok bool
if len(numbers) == 3 {
    ok = true
}
if ok {
    fmt.Println("ok")
}
""")


if __name__ == '__main__':
    unittest.main()
