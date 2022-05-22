from run import *
import ast as astm
import syntax as syntaxm
import unittest


class BlockScopeTest(unittest.TestCase):

    def test_declaration_before_block(self):
        py = """
name = "smoke"
if name == "water":
    name = "water"
print(name)
"""
        self._t(py, syntax=syntaxm.PythonSyntax(), expected=py)

        self._t(py, syntax=syntaxm.JavaSyntax(), expected="""
String name = "smoke";
if (name.equals("water")) {
    name = "water";
}
System.out.println(name);
""")

        self._t(py, syntax=syntaxm.ElispSyntax(), expected="""
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
        self._t(py, syntax=syntaxm.PythonSyntax(), expected=py)

        self._t(py, syntax=syntaxm.JavaSyntax(), expected="""
String name = null;
if (1 == 1) {
    name = "water";
}
System.out.println(name);
""")

        self._t(py, syntax=syntaxm.ElispSyntax(), expected="""
(if (equal 1 1)
    (setq name "water"))
(message name)
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
        self._t(py, syntax=syntaxm.PythonSyntax(), expected=py)

        self._t(py, syntax=syntaxm.JavaSyntax(), expected="""
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

        self._t(py, syntax=syntaxm.ElispSyntax(), expected="""
(if (equal 1 1)
    (if (equal 2 2)
        (setq status "live is life")
        (setq status "live is not life"))
    (setq status "two is not one"))
(message status)
""")

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code)


if __name__ == '__main__':
    unittest.main()
