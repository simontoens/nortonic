from tests import compilertest
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
String name = "smoke";
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

    def test_declaration_in_block(self):
        py = """
if 1 == 1:
    name = "water"
print(name)
"""
        self.py(py, expected=py)

        self.java(py, expected="""
String name = null;
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
public void foo() {
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
String status = null;
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
Boolean ok = null;
List<Integer> numbers = new ArrayList<>(List.of(1, 2, 3));
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


if __name__ == '__main__':
    unittest.main()
