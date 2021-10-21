from run import *
import ast as astm
import syntax as syntaxm
import unittest


class IfTest(unittest.TestCase):

    def test_simple_if(self):
        py = """
name = "smoke"
if name == "water":
    print("ok")
    return True
"""
        self._t(py, syntax=syntaxm.PythonSyntax(), expected=py)

        self._t(py, syntax=syntaxm.JavaSyntax(), expected="""
String name = "smoke";
if (name == "water") {
    System.out.println("ok");
    return true;
}
""")

        self._t(py, syntax=syntaxm.ElispSyntax(), expected="""
(setq name "smoke")
(if (equal name "water") (progn
    (message "ok")
    t))
""")

    def test_simple_if_else(self):
        py = """
name="smoke"
if name == "water":
    print("ok")
else:
    print("computer")
"""
        self._t(py, syntax=syntaxm.PythonSyntax(), expected="""
name = "smoke"
if name == "water":
    print("ok")
else:
    print("computer")""")

        self._t(py, syntax=syntaxm.JavaSyntax(), expected="""
String name = "smoke";
if (name == "water") {
    System.out.println("ok");
} else {
    System.out.println("computer");
}
""")

        self._t(py, syntax=syntaxm.ElispSyntax(), expected="""
(setq name "smoke")
(if (equal name "water")
    (message "ok")
    (message "computer"))
""")

    def test_nested_if(self):
        py = """
name="water"
if name == "water":
    if 1==1:
        print("yes")
    else:
        print("no")
"""
        self._t(py, syntax=syntaxm.PythonSyntax(), expected="""
name = "water"
if name == "water":
    if 1 == 1:
        print("yes")
    else:
        print("no")
""")

        self._t(py, syntax=syntaxm.JavaSyntax(), expected="""
String name = "water";
if (name == "water") {
    if (1 == 1) {
        System.out.println("yes");
    } else {
        System.out.println("no");
    }
}
""")

        self._t(py, syntax=syntaxm.ElispSyntax(), expected="""
(setq name "water")
(if (equal name "water")
    (if (equal 1 1)
        (message "yes")
        (message "no")))
""")
                   
    def test_elif(self):
        """
        In the AST elif has been normalized to nested if/else.
        """
        py = """
if 1==1:
    return True
elif 1==2:
    return False
else:
    return 3
"""
        self._t(py, syntax=syntaxm.PythonSyntax(), expected="""
if 1 == 1:
    return True
else:
    if 1 == 2:
        return False
    else:
        return 3
""")

        self._t(py, syntax=syntaxm.JavaSyntax(), expected="""
if (1 == 1) {
    return true;
} else {
    if (1 == 2) {
        return false;
    } else {
        return 3;
    }
}
""")

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code)


if __name__ == '__main__':
    unittest.main()
